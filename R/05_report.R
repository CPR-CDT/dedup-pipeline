# 05_report.R ---------------------------------------------------------------
# Write the deduplicated library and the audit files.

# SHA-256 of every raw file, so others can check they have the same exports.
input_manifest <- function(files, recs) {
  n <- dplyr::count(recs, source, file, name = "n_records")
  files |>
    dplyr::mutate(file = basename(path),
                  sha256 = vapply(path, digest::digest, character(1),
                                  algo = "sha256", file = TRUE),
                  bytes = file.size(path)) |>
    dplyr::left_join(n, by = c("source", "file")) |>
    dplyr::select(source, file, format, n_records, bytes, sha256)
}

split_names <- function(x) {
  if (is.na(x)) character() else trimws(strsplit(x, ";")[[1]])
}

split_pages <- function(p) {
  parts <- strsplit(ifelse(is.na(p), "", p), "-", fixed = TRUE)
  list(start = vapply(parts, function(x) if (length(x)) x[1] else NA_character_, character(1)),
       end = vapply(parts, function(x) if (length(x) > 1) x[2] else NA_character_, character(1)))
}

ris_type <- function(type) {
  t <- toupper(dplyr::coalesce(type, ""))
  dplyr::case_when(
    grepl("CHAP", t) ~ "CHAP",
    grepl("CONF|^C$|PROCEEDINGS", t) ~ "CONF",
    grepl("BOOK|^B$", t) ~ "BOOK",
    TRUE ~ "JOUR"
  )
}

write_ris <- function(lib, path) {
  pg <- split_pages(lib$pages)
  out <- character()
  for (i in seq_len(nrow(lib))) {
    r <- lib[i, ]
    tag <- function(t, v) if (length(v) && !is.na(v[1]) && nzchar(v[1])) paste0(t, "  - ", v) else NULL
    out <- c(out,
      tag("TY", ris_type(r$type)),
      unlist(lapply(split_names(r$author_original), function(a) tag("AU", a))),
      tag("TI", r$title), tag("T2", r$journal), tag("PY", r$year),
      tag("VL", r$volume), tag("IS", r$number),
      tag("SP", pg$start[i]), tag("EP", pg$end[i]),
      tag("DO", r$doi), tag("SN", r$isbn), tag("AB", r$abstract),
      unlist(lapply(split_names(r$keywords), function(k) tag("KW", k))),
      tag("UR", r$url), tag("DB", r$sources), tag("AN", r$duplicate_id),
      tag("N1", paste("Dedup group records:", r$record_ids)),
      "ER  - ", "")
  }
  writeLines(enc2utf8(out), path, useBytes = TRUE)
}

write_bib <- function(lib, path) {
  bib_type <- c(JOUR = "article", CONF = "inproceedings", CHAP = "incollection", BOOK = "book")
  venue_field <- c(JOUR = "journal", CONF = "booktitle", CHAP = "booktitle", BOOK = "series")
  clean <- function(x) gsub("[{}]", "", x)
  out <- character()
  for (i in seq_len(nrow(lib))) {
    r <- lib[i, ]
    t <- ris_type(r$type)
    entry <- if (grepl("PROCEEDINGS|CONFERENCE_REVIEW", toupper(dplyr::coalesce(r$type, "")))) "proceedings" else bib_type[[t]]
    fields <- c(
      author = paste(split_names(r$author_original), collapse = " and "),
      title = r$title, year = r$year, volume = r$volume, number = r$number,
      pages = r$pages, doi = r$doi, issn = r$isbn, abstract = r$abstract,
      keywords = r$keywords, url = r$url, note = paste("Sources:", r$sources),
      dedup_records = r$record_ids)
    fields[venue_field[[t]]] <- r$journal
    fields <- fields[!is.na(fields) & nzchar(fields)]
    out <- c(out, sprintf("@%s{%s,", entry, r$duplicate_id),
             sprintf("  %s = {%s},", names(fields), clean(fields)), "}", "")
  }
  writeLines(enc2utf8(out), path, useBytes = TRUE)
}

prisma_counts <- function(recs, membership, edges, n_pending) {
  by_source <- dplyr::count(recs, source, name = "n") |>
    dplyr::mutate(step = paste("Records identified:", source)) |>
    dplyr::select(step, n)
  n_total <- nrow(recs)
  n_unique <- dplyr::n_distinct(membership$duplicate_id)
  removed_by <- function(types) {
    # Records removed in groups that only exist because of these link types.
    g <- igraph::graph_from_data_frame(
      edges[edges$link %in% types, c("record_id1", "record_id2")],
      directed = FALSE, vertices = data.frame(name = recs$record_id))
    n_total - igraph::components(g)$no
  }
  n_auto <- removed_by(c("auto", "exact"))
  dplyr::bind_rows(
    by_source,
    tibble::tibble(
      step = c("Records identified: total",
               "Duplicates removed: automatic (ASySD)",
               "Duplicates removed: manual decisions",
               "Duplicates removed: total",
               "Records after deduplication",
               "Possible duplicate pairs still awaiting manual decision"),
      n = c(n_total, n_auto, (n_total - n_unique) - n_auto,
            n_total - n_unique, n_unique, n_pending))
  )
}

write_outputs <- function(out_dir, lib, membership, edges, review, manifest,
                          counts, conflicts) {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  f <- function(name) file.path(out_dir, name)

  write_ris(lib, f("deduplicated_library.ris"))
  write_bib(lib, f("deduplicated_library.bib"))
  readr::write_excel_csv(lib, f("deduplicated_library.csv"), na = "")

  membership |>
    dplyr::select(record_id, duplicate_id, is_primary, group_size, source,
                  file, native_id, title, year, doi) |>
    dplyr::arrange(record_id) |>
    readr::write_excel_csv(f("record_log.csv"), na = "")
  readr::write_excel_csv(edges, f("matched_pairs.csv"), na = "")
  readr::write_excel_csv(review, f("manual_review_pairs.csv"), na = "")
  readr::write_excel_csv(conflicts, f("groups_with_doi_conflicts.csv"), na = "")
  readr::write_csv(manifest, f("input_manifest.csv"), na = "")
  readr::write_csv(counts, f("prisma_counts.csv"))

  summary <- c(
    paste("Deduplication run:", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
    paste("ASySD version:", as.character(utils::packageVersion("ASySD"))),
    paste("R version:", R.version.string), "",
    sprintf("%-60s %6s", counts$step, counts$n))
  writeLines(summary, f("summary.txt"))
  writeLines(utils::capture.output(utils::sessionInfo()), f("session_info.txt"))
  invisible(summary)
}
