# 02_clean.R --------------------------------------------------------------
# Put fields from different databases into a common format before matching.
# The original values are kept in the source files; only the copies used
# for matching and output are changed here.

decode_html <- function(x) {
  x <- gsub("<[^>]+>", "", x)            # e.g. <i>, <sub>, <inf>
  x <- gsub("&amp;", "&", x, fixed = TRUE)
  x <- gsub("&lt;", "<", x, fixed = TRUE)
  x <- gsub("&gt;", ">", x, fixed = TRUE)
  x <- gsub("&quot;", "\"", x, fixed = TRUE)
  x <- gsub("&#0?39;|&apos;", "'", x)
  x <- gsub("&nbsp;|&#160;", " ", x)
  x
}

squish <- function(x) dplyr::na_if(stringr::str_squish(x), "")

# "10.1109/ABC.2020.123" from "https://doi.org/10.1109/abc.2020.123", "doi: ..." etc.
clean_doi <- function(x) {
  x <- tolower(trimws(x))
  x <- sub("^(https?://)?(dx\\.)?doi\\.org/", "", x)
  x <- sub("^doi:\\s*", "", x)
  x <- sub("[.,;]+$", "", x)
  dplyr::na_if(x, "")
}

# One name -> "Surname AB".
#   "Comert, G" / "Ng, J." / "Smith, J.A."  (surname first, with comma)
#   "H. Talat" / "Madeline J. Hallewell"    (given names first)
standardise_name <- function(name) {
  name <- trimws(gsub("(,?\\s+|^)(Jr|Sr|II|III|IV)\\.?(?=,|$|\\s)", "", name, perl = TRUE))
  if (!nzchar(name)) return(NA_character_)
  if (grepl(",", name)) {
    surname <- trimws(sub(",.*$", "", name))
    given <- trimws(sub("^[^,]*,", "", name))
  } else {
    parts <- strsplit(name, "\\s+")[[1]]
    surname <- parts[length(parts)]
    given <- paste(parts[-length(parts)], collapse = " ")
  }
  given_parts <- strsplit(given, "[\\s.\\-]+", perl = TRUE)[[1]]
  given_parts <- given_parts[nzchar(given_parts)]
  # Already-abbreviated initials such as "JH" (WoS) stay as they are.
  initials <- vapply(given_parts, function(p) {
    if (grepl("^[[:upper:]]+$", p) && nchar(p) <= 3) p else substr(p, 1, 1)
  }, character(1))
  trimws(paste(surname, toupper(paste(initials, collapse = ""))))
}

standardise_authors <- function(x) {
  vapply(x, function(a) {
    if (is.na(a) || !nzchar(trimws(a))) return(NA_character_)
    names <- trimws(strsplit(a, ";")[[1]])
    names <- vapply(names, standardise_name, character(1))
    paste(names[!is.na(names)], collapse = ", ")
  }, character(1), USE.NAMES = FALSE)
}

clean_records <- function(recs) {
  text_cols <- c("author", "title", "journal", "volume", "number", "pages",
                 "isbn", "abstract", "keywords")
  recs |>
    dplyr::mutate(
      dplyr::across(dplyr::all_of(text_cols), ~ squish(decode_html(.x))),
      # Original names are kept for the exported library; the standardised,
      # accent-free form ("Surname AB, ...") is what ASySD compares.
      author_original = author,
      author = standardise_authors(stringi::stri_trans_general(author, "Latin-ASCII")),
      year = stringr::str_extract(year, "(19|20)\\d{2}"),
      doi = clean_doi(doi),
      pages = gsub("[–—]", "-", pages),
      title = sub("[.]$", "", title)
    )
}
