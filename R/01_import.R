# 01_import.R -------------------------------------------------------------
# Readers for each database export. Every reader returns a tibble with the
# same columns (see `empty_record()`), one row per record, in file order.
# Author names are kept as exported here; they are standardised in 02_clean.R.

empty_record <- function(n = 0) {
  cols <- c("native_id", "type", "author", "year", "title", "journal",
            "volume", "number", "pages", "doi", "isbn", "abstract",
            "keywords", "url")
  tibble::as_tibble(sapply(cols, function(c) rep(NA_character_, n), simplify = FALSE))
}

# Join start/end page into "start-end"; fall back to an article number.
make_pages <- function(start, end, article = NA_character_) {
  start <- dplyr::na_if(trimws(start), "")
  end <- dplyr::na_if(trimws(end), "")
  article <- dplyr::na_if(trimws(article), "")
  dplyr::case_when(
    !is.na(start) & !is.na(end) ~ paste0(start, "-", end),
    !is.na(start) ~ start,
    TRUE ~ article
  )
}

# Collapse a list of tag values into a single string (or NA).
tag_value <- function(rec, tag, sep = " ") {
  v <- rec[[tag]]
  if (is.null(v) || length(v) == 0) return(NA_character_)
  paste(v, collapse = sep)
}

# --- Tagged formats (RIS, WoS) ---------------------------------------------
# Generic parser: splits lines into records and collects values per tag.
# `tag_re` captures (tag, value); lines not matching it continue the
# previous tag's value. `end_tag` closes a record.
parse_tagged <- function(path, tag_re, end_tag) {
  lines <- readr::read_lines(path, progress = FALSE)
  lines <- sub("^﻿", "", lines)
  records <- list()
  rec <- list()
  last_tag <- NULL
  for (line in lines) {
    m <- regmatches(line, regexec(tag_re, line, perl = TRUE))[[1]]
    if (length(m) == 3) {
      tag <- m[2]
      value <- trimws(m[3])  # "" for value-less tags such as ER
      if (tag == end_tag) {
        if (length(rec)) records[[length(records) + 1]] <- rec
        rec <- list()
        last_tag <- NULL
        next
      }
      rec[[tag]] <- c(rec[[tag]], value)
      last_tag <- tag
    } else if (!is.null(last_tag) && grepl("^\\s+\\S", line)) {
      # Continuation line (WoS): new author for list tags, else append text.
      value <- trimws(line)
      if (last_tag %in% c("AU", "AF", "CR")) {
        rec[[last_tag]] <- c(rec[[last_tag]], value)
      } else {
        n <- length(rec[[last_tag]])
        rec[[last_tag]][n] <- paste(rec[[last_tag]][n], value)
      }
    }
  }
  records
}

read_ris <- function(path) {
  recs <- parse_tagged(path, "^([A-Z][A-Z0-9])  - ?(.*)$", end_tag = "ER")
  out <- empty_record(length(recs))
  for (i in seq_along(recs)) {
    r <- recs[[i]]
    url <- tag_value(r, "UR")
    out$native_id[i] <- sub(".*/publications/(\\d+).*", "\\1", url)
    out$type[i] <- sub("^label\\.ris\\.referenceType\\.", "", tag_value(r, "TY"))
    out$author[i] <- tag_value(r, "AU", "; ")
    out$year[i] <- tag_value(r, "PY")
    out$title[i] <- tag_value(r, "TI")
    out$journal[i] <- tag_value(r, "T2")
    out$volume[i] <- tag_value(r, "VL")
    out$number[i] <- tag_value(r, "IS")
    out$pages[i] <- make_pages(tag_value(r, "SP"), tag_value(r, "EP"), tag_value(r, "C7"))
    out$doi[i] <- tag_value(r, "DO")
    out$isbn[i] <- tag_value(r, "SN", "; ")
    out$abstract[i] <- tag_value(r, "AB")
    out$keywords[i] <- tag_value(r, "KW", "; ")
    out$url[i] <- url
  }
  out
}

read_wos <- function(path) {
  recs <- parse_tagged(path, "^([A-Z][A-Z0-9])(?: (.*))?$", end_tag = "ER")
  # Drop file header records (FN/VR only).
  recs <- Filter(function(r) !is.null(r$UT), recs)
  out <- empty_record(length(recs))
  for (i in seq_along(recs)) {
    r <- recs[[i]]
    out$native_id[i] <- tag_value(r, "UT")
    out$type[i] <- tag_value(r, "PT")
    out$author[i] <- tag_value(r, "AU", "; ")
    out$year[i] <- tag_value(r, "PY")
    out$title[i] <- tag_value(r, "TI")
    out$journal[i] <- tag_value(r, "SO")
    out$volume[i] <- tag_value(r, "VL")
    out$number[i] <- tag_value(r, "IS")
    out$pages[i] <- make_pages(tag_value(r, "BP"), tag_value(r, "EP"), tag_value(r, "AR"))
    out$doi[i] <- tag_value(r, "DI")
    isbn <- c(r$SN, r$EI, r$BN)
    out$isbn[i] <- if (length(isbn)) paste(isbn, collapse = "; ") else NA_character_
    out$abstract[i] <- tag_value(r, "AB")
    kw <- c(r$DE, r$ID)
    out$keywords[i] <- if (length(kw)) paste(kw, collapse = "; ") else NA_character_
  }
  out
}

# --- IEEE Xplore CSV ----------------------------------------------------------
read_ieee_csv <- function(path) {
  d <- readr::read_csv(path, col_types = readr::cols(.default = "c"),
                       na = c("", "NA"), progress = FALSE)
  isbn <- paste(dplyr::coalesce(d$ISSN, ""), dplyr::coalesce(d$ISBNs, ""), sep = "; ")
  isbn <- dplyr::na_if(gsub("^; |; $", "", isbn), "; ")
  tibble::tibble(
    native_id = sub(".*arnumber=(\\d+).*", "\\1", d$`PDF Link`),
    type = d$`Document Identifier`,
    author = d$Authors,
    year = d$`Publication Year`,
    title = d$`Document Title`,
    journal = d$`Publication Title`,
    volume = d$Volume,
    number = d$Issue,
    pages = make_pages(d$`Start Page`, d$`End Page`),
    doi = d$DOI,
    isbn = dplyr::na_if(isbn, ""),
    abstract = d$Abstract,
    keywords = d$`Author Keywords`,
    url = d$`PDF Link`
  )
}

# --- ACM Digital Library plain-text citations -------------------------------
# The ACM export is in "ACM Reference Format", one citation per paragraph:
#   Authors. Year. Title. In Proceedings ... (ACRONYM). Publisher, 1-10. DOI
#   Authors. Year. Title. Journal Abbrev. Vol, Issue, Article N (Month Year), N pages. DOI
#   Year. Proceedings title. Publisher.            (whole proceedings volume)
# These have no abstracts, so ACM records are matched on title, authors,
# year, venue, pages and DOI only.

# Split "A, B, and C" / "A and B" into individual names.
split_acm_authors <- function(x) {
  x <- sub(",? and ", ", ", x)
  trimws(strsplit(x, ", ")[[1]])
}

parse_acm_citation <- function(cit) {
  rec <- as.list(empty_record(1))
  cit <- trimws(cit)

  doi_m <- regmatches(cit, regexpr("https://doi\\.org/\\S+$", cit))
  if (length(doi_m)) {
    rec$doi <- doi_m
    rec$url <- doi_m
    cit <- trimws(sub("https://doi\\.org/\\S+$", "", cit))
  }

  # Authors. Year. Rest   (authors may be absent for proceedings volumes)
  m <- regmatches(cit, regexec("^(?:(.*?)\\. )?((?:19|20)\\d{2})[a-z]?\\. (.*)$", cit, perl = TRUE))[[1]]
  if (!length(m)) {
    rec$title <- cit
    return(rec)
  }
  if (nzchar(m[2])) rec$author <- paste(split_acm_authors(m[2]), collapse = "; ")
  rec$year <- m[3]
  rest <- m[4]

  if (grepl("\\. In ", rest)) {
    # Conference paper: Title. In Venue. Publisher, City, Country, [Article N, ]pages.
    rec$type <- "CONF"
    rec$title <- sub("^(.*?)\\. In .*$", "\\1", rest, perl = TRUE)
    venue <- sub("^.*?\\. In ", "", rest, perl = TRUE)
    rec$journal <- trimws(sub("\\. (Association for Computing Machinery|ACM|IEEE Press|[A-Z][^.]*Press)\\b.*$", "", venue))
    pg <- regmatches(venue, regexpr("\\d+[–-]\\d+\\.?$", venue))
    if (length(pg)) rec$pages <- sub("\\.$", "", pg)
  } else if (is.na(rec$author)) {
    # Whole proceedings volume: "Proceedings of .... Publisher."
    rec$type <- "PROCEEDINGS"
    rec$title <- sub("\\. Association for Computing Machinery.*$", "", rest)
  } else {
    # Journal article. The venue is a run of capitalised (abbreviated) words
    # followed by "Vol, Issue," or "Just Accepted".
    rec$type <- "JOUR"
    jm <- regmatches(rest, regexec(paste0(
      "^(.*?[.?!]) ((?:[A-Z][A-Za-z&.\\-]* )+?)",
      "(?:(\\d+), ([^,(]+?)(?:, Article (\\S+))? \\(|Just Accepted)"), rest, perl = TRUE))[[1]]
    if (length(jm)) {
      rec$title <- sub("\\.$", "", jm[2])
      rec$journal <- trimws(jm[3])
      rec$volume <- dplyr::na_if(jm[4], "")
      rec$number <- dplyr::na_if(jm[5], "")
      pg <- regmatches(rest, regexpr("\\), \\d+[–-]\\d+\\.?$", rest))
      rec$pages <- if (length(pg)) sub("^\\), |\\.$", "", pg) else dplyr::na_if(jm[6], "")
    } else {
      rec$title <- sub("^(.*?)\\. .*$", "\\1", rest, perl = TRUE)
      rec$journal <- sub("^.*?\\. ", "", rest, perl = TRUE)
    }
  }
  rec
}

read_acm_text <- function(path) {
  lines <- readr::read_lines(path, progress = FALSE)
  lines <- sub("^﻿", "", lines)
  cits <- trimws(lines[nzchar(trimws(lines))])
  out <- dplyr::bind_rows(lapply(cits, function(x) tibble::as_tibble(parse_acm_citation(x))))
  out$native_id <- dplyr::coalesce(out$doi, as.character(seq_len(nrow(out))))
  out$pages <- gsub("–", "-", out$pages)
  out
}

# --- Dispatcher -------------------------------------------------------------
readers <- list(ris = read_ris, wos = read_wos, ieee_csv = read_ieee_csv,
                acm_text = read_acm_text)

# List the raw files for every source (alphabetical within a source).
list_source_files <- function(config) {
  purrr::map_dfr(config$sources, function(s) {
    files <- sort(unlist(lapply(s$files, function(g) Sys.glob(file.path(config$raw_dir, g)))))
    if (!length(files)) stop("No files found for source ", s$name)
    tibble::tibble(source = s$name, format = s$format, path = files)
  })
}

# Read every file, tag each record with its source and file, and assign a
# stable record_id (source + running number within the source).
import_all <- function(config) {
  files <- list_source_files(config)
  recs <- purrr::pmap_dfr(files, function(source, format, path) {
    reader <- readers[[format]]
    if (is.null(reader)) stop("Unknown format: ", format)
    d <- reader(path)
    d$source <- source
    d$file <- basename(path)
    d
  })
  recs |>
    dplyr::group_by(source) |>
    dplyr::mutate(record_id = sprintf("%s-%05d", source, dplyr::row_number())) |>
    dplyr::ungroup() |>
    dplyr::relocate(record_id, source, file)
}
