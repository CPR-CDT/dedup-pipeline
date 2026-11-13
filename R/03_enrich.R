# 03_enrich.R ---------------------------------------------------------------
# Fill in missing fields (mainly abstracts) for records from sources with
# poor exports - by default ACM, whose export has no abstracts.
#
#   Records with a DOI    -> looked up by DOI in OpenAlex; if OpenAlex has no
#                            abstract, Crossref is tried for one.
#   Records without a DOI -> searched by title in OpenAlex. A result is only
#                            accepted if the titles are near-identical
#                            (similarity >= title_min_similarity), the year
#                            is within 1, and the first author's surname
#                            appears among the result's authors. Accepted
#                            matches supply the abstract only.
#
# Only empty fields are filled; values from the original export are never
# overwritten. Every lookup is stored in a cache file, and later runs read
# the cache instead of querying again, so results do not drift as the
# online databases change. With `fetch: false` the pipeline runs offline
# from the cache alone.
#
# Optional environment variables: OPENALEX_API_KEY (own daily allowance
# instead of the shared free one), CONTACT_EMAIL (sent to OpenAlex and
# Crossref as `mailto` to use their "polite" pools).

cache_cols <- c("key", "lookup", "query", "status", "service", "openalex_id",
                "doi", "title", "authors", "year", "venue", "volume", "issue",
                "pages", "abstract", "abstract_service", "match_score",
                "retrieved_at")

read_cache <- function(path) {
  if (!file.exists(path)) {
    return(tibble::as_tibble(sapply(cache_cols, function(c) character(), simplify = FALSE)))
  }
  readr::read_csv(path, col_types = readr::cols(.default = "c"), progress = FALSE)
}

write_cache <- function(cache, path) {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  cache |>
    dplyr::arrange(key) |>
    readr::write_csv(path, na = "")
}

# --- HTTP ---------------------------------------------------------------------
api_get <- function(url, query = list()) {
  email <- Sys.getenv("CONTACT_EMAIL")
  if (nzchar(email)) query$mailto <- email
  if (grepl("openalex", url) && nzchar(Sys.getenv("OPENALEX_API_KEY"))) {
    query$api_key <- Sys.getenv("OPENALEX_API_KEY")
  }
  resp <- httr2::request(url) |>
    httr2::req_url_query(!!!query) |>
    httr2::req_user_agent("systematic-review-dedup-pipeline (R httr2)") |>
    httr2::req_retry(max_tries = 4) |>
    httr2::req_error(is_error = function(r) FALSE) |>
    httr2::req_perform()
  status <- httr2::resp_status(resp)
  if (status == 404) return(NULL)
  if (status >= 400) stop("HTTP ", status, " from ", url)
  httr2::resp_body_json(resp, simplifyVector = FALSE)
}

# --- OpenAlex -----------------------------------------------------------------
openalex_select <- "id,doi,title,publication_year,authorships,biblio,primary_location,abstract_inverted_index"

# OpenAlex stores abstracts as {word: [positions]}; rebuild the text.
rebuild_abstract <- function(inv) {
  if (is.null(inv) || !length(inv)) return(NA_character_)
  pos <- unlist(inv)
  words <- rep(names(inv), lengths(inv))
  paste(words[order(pos)], collapse = " ")
}

nz <- function(x) if (is.null(x) || !length(x) || identical(x, "")) NA_character_ else as.character(x)

openalex_fields <- function(w) {
  b <- w$biblio
  pages <- if (!is.null(b$first_page) && !is.null(b$last_page) && b$first_page != b$last_page) {
    paste0(b$first_page, "-", b$last_page)
  } else nz(b$first_page)
  authors <- vapply(w$authorships, function(a) nz(a$author$display_name), character(1))
  abstract <- rebuild_abstract(w$abstract_inverted_index)
  list(
    service = "OpenAlex", openalex_id = nz(w$id),
    doi = clean_doi(nz(w$doi)), title = nz(w$title),
    authors = if (length(authors)) paste(authors, collapse = "; ") else NA_character_,
    year = nz(w$publication_year), venue = nz(w$primary_location$source$display_name),
    volume = nz(b$volume), issue = nz(b$issue), pages = nz(pages),
    abstract = abstract,
    abstract_service = if (is.na(abstract)) NA_character_ else "OpenAlex"
  )
}

# Look up DOIs in batches of 50 (one request per batch).
openalex_by_doi <- function(dois) {
  out <- list()
  for (batch in split(dois, ceiling(seq_along(dois) / 50))) {
    res <- api_get("https://api.openalex.org/works", list(
      filter = paste0("doi:", paste(batch, collapse = "|")),
      select = openalex_select, `per-page` = 50))
    for (w in res$results) {
      f <- openalex_fields(w)
      if (!is.na(f$doi)) out[[f$doi]] <- f
    }
  }
  out
}

# Crossref: abstract (and bibliographic fields) for one DOI.
crossref_by_doi <- function(doi) {
  res <- api_get(paste0("https://api.crossref.org/works/", utils::URLencode(doi, reserved = TRUE)))
  if (is.null(res)) return(NULL)
  m <- res$message
  abstract <- nz(m$abstract)
  list(
    service = "Crossref", doi = clean_doi(nz(m$DOI)), title = nz(unlist(m$title)[1]),
    authors = paste(vapply(m$author, function(a) paste(nz(a$given), nz(a$family)), character(1)), collapse = "; "),
    year = nz(unlist(m$issued$`date-parts`)[1]), venue = nz(unlist(m$`container-title`)[1]),
    volume = nz(m$volume), issue = nz(m$issue), pages = nz(m$page),
    abstract = abstract,
    abstract_service = if (is.na(abstract)) NA_character_ else "Crossref"
  )
}

# --- Title matching -----------------------------------------------------------
norm_title <- function(x) {
  x <- stringi::stri_trans_general(tolower(x), "Latin-ASCII")
  stringr::str_squish(gsub("[^a-z0-9 ]", " ", x))
}

title_similarity <- function(a, b) {
  a <- norm_title(a)
  b <- norm_title(b)
  1 - utils::adist(a, b)[1, 1] / max(nchar(a), nchar(b))
}

surname_of <- function(name) {
  tolower(stringi::stri_trans_general(sub(" .*$", "", trimws(name)), "Latin-ASCII"))
}

openalex_by_title <- function(title, year, first_author, min_sim) {
  y <- suppressWarnings(as.integer(year))
  query <- list(search = norm_title(title), select = openalex_select, `per-page` = 5)
  if (!is.na(y)) query$filter <- sprintf("publication_year:%d-%d", y - 1, y + 1)
  res <- api_get("https://api.openalex.org/works", query)
  best <- NULL
  best_score <- -1
  for (w in res$results) {
    f <- openalex_fields(w)
    if (is.na(f$title)) next
    score <- title_similarity(title, f$title)
    if (score > best_score) {
      best <- f
      best_score <- score
    }
  }
  if (is.null(best)) return(list(status = "not_found"))
  cand_surnames <- vapply(strsplit(dplyr::coalesce(best$authors, ""), ";\\s*")[[1]], function(n) {
    tolower(stringi::stri_trans_general(sub("^.* ", "", trimws(n)), "Latin-ASCII"))
  }, character(1))
  author_ok <- is.na(first_author) || surname_of(first_author) %in% cand_surnames
  year_ok <- is.na(y) || is.na(best$year) || abs(as.integer(best$year) - y) <= 1
  best$match_score <- sprintf("%.3f", best_score)
  best$status <- if (best_score >= min_sim && author_ok && year_ok) "found" else "rejected"
  best
}

# --- Main step ----------------------------------------------------------------
lookup_key <- function(recs) {
  ifelse(!is.na(recs$doi), paste0("doi:", recs$doi),
         paste0("title:", norm_title(recs$title), "|", dplyr::coalesce(recs$year, "")))
}

fetch_missing <- function(todo, min_sim) {
  rows <- list()
  stamp <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  row <- function(key, lookup, query, f) {
    f <- f[intersect(names(f), cache_cols)]
    tibble::as_tibble(c(list(key = key, lookup = lookup, query = query,
                             retrieved_at = stamp), f))
  }

  dois <- todo$doi[todo$lookup == "doi"]
  if (length(dois)) {
    message("    OpenAlex: looking up ", length(dois), " DOIs...")
    found <- openalex_by_doi(dois)
    need_crossref <- dois[vapply(dois, function(d) is.null(found[[d]]) || is.na(found[[d]]$abstract), logical(1))]
    message("    Crossref: looking up ", length(need_crossref), " DOIs without an OpenAlex abstract...")
    for (d in dois) {
      f <- found[[d]]
      if (d %in% need_crossref) {
        cr <- tryCatch(crossref_by_doi(d), error = function(e) NULL)
        Sys.sleep(0.05)
        if (is.null(f) && !is.null(cr)) {
          f <- cr
        } else if (!is.null(f) && !is.null(cr) && !is.na(cr$abstract)) {
          f$abstract <- cr$abstract
          f$abstract_service <- "Crossref"
        }
      }
      if (is.null(f)) f <- list()
      f$status <- if (length(f)) "found" else "not_found"
      rows[[length(rows) + 1]] <- row(paste0("doi:", d), "doi", d, f)
    }
  }

  titles <- todo[todo$lookup == "title", ]
  if (nrow(titles)) message("    OpenAlex: searching ", nrow(titles), " titles...")
  for (i in seq_len(nrow(titles))) {
    t <- titles[i, ]
    f <- tryCatch(openalex_by_title(t$title, t$year, t$first_author, min_sim),
                  error = function(e) list(status = "error"))
    rows[[length(rows) + 1]] <- row(t$key, "title", t$title, f)
  }
  dplyr::bind_rows(rows)
}

enrich_records <- function(recs, cfg) {
  empty_log <- tibble::tibble(record_id = character(), lookup = character(),
                              status = character(), filled = character())
  if (is.null(cfg) || !isTRUE(cfg$enabled)) return(list(records = recs, log = empty_log))

  target <- recs$source %in% unlist(cfg$sources) & !recs$type %in% "PROCEEDINGS"
  todo <- recs[target, ] |>
    dplyr::mutate(key = lookup_key(dplyr::pick(dplyr::everything())),
                  lookup = ifelse(is.na(doi), "title", "doi"),
                  first_author = sub(",.*$", "", author)) |>
    dplyr::distinct(key, .keep_all = TRUE)

  cache <- read_cache(cfg$cache_file)
  missing <- todo[!todo$key %in% cache$key, ]
  if (nrow(missing)) {
    if (isTRUE(cfg$fetch)) {
      message("    ", nrow(missing), " records not in the enrichment cache; querying online...")
      new <- fetch_missing(missing, cfg$title_min_similarity)
      new[setdiff(cache_cols, names(new))] <- NA_character_
      cache <- dplyr::bind_rows(cache, new[cache_cols])
      write_cache(cache, cfg$cache_file)
    } else {
      message("    ", nrow(missing), " records not in the enrichment cache and fetch is off; they are not enriched.")
    }
  }
  # Lookups that failed with an error are retried next run.
  cache_ok <- cache[cache$status %in% c("found", "not_found", "rejected"), ]
  if (any(cache$status %in% "error")) {
    write_cache(cache_ok, cfg$cache_file)
    warning(sum(cache$status %in% "error"), " lookups failed and will be retried on the next run.", call. = FALSE)
  }

  # Fill empty fields from found lookups.
  idx <- which(target)
  keys <- lookup_key(recs[idx, ])
  # A title match can point to another version of the paper (e.g. a
  # repository copy with its own DOI), so it only supplies the abstract.
  # Its DOI is logged for checking but not used, as a wrong DOI could block
  # a correct merge.
  fill_by_doi <- c(abstract = "abstract", volume = "volume", number = "issue",
                   pages = "pages", journal = "venue")
  fill_by_title <- c(abstract = "abstract")
  log <- vector("list", length(idx))
  for (j in seq_along(idx)) {
    i <- idx[j]
    h <- cache_ok[match(keys[j], cache_ok$key), ]
    status <- h$status
    filled <- character()
    if (status %in% "found") {
      fill <- if (startsWith(h$key, "doi:")) fill_by_doi else fill_by_title
      for (field in names(fill)) {
        new <- squish(decode_html(h[[fill[[field]]]]))
        if (is.na(recs[[field]][i]) && !is.na(new)) {
          recs[[field]][i] <- new
          filled <- c(filled, field)
        }
      }
    }
    service <- if ("abstract" %in% filled) h$abstract_service else NA_character_
    log[[j]] <- tibble::tibble(
      record_id = recs$record_id[i], lookup = ifelse(startsWith(keys[j], "doi:"), "doi", "title"),
      status = dplyr::coalesce(status, "not_looked_up"),
      filled = paste(filled, collapse = "; "),
      abstract_service = service,
      match_score = h$match_score, openalex_id = h$openalex_id,
      matched_title = if (!is.na(h$key) && startsWith(h$key, "title:")) h$title else NA_character_,
      matched_doi = if (!is.na(h$key) && startsWith(h$key, "title:")) h$doi else NA_character_)
  }
  recs$pages <- gsub("[–—]", "-", recs$pages)
  list(records = recs, log = dplyr::bind_rows(empty_log, log))
}
