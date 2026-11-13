# 06_irr_sample.R -----------------------------------------------------------
# Random sample of records for the inter-rater reliability (IRR) pilot.
#
# Each record gets a score = SHA-256 of "<seed>:<duplicate_id>" and the
# records with the lowest scores are chosen. This is a uniformly random
# sample that (unlike R's sample()) is identical on every computer and R
# version, and barely changes if a few records are later added to or
# removed from the library: only those records can enter or leave it.

# Whole-proceedings records are not publications, so they are not sampled.
is_proceedings_record <- function(type) {
  grepl("PROCEEDINGS|CONFERENCE_REVIEW", toupper(dplyr::coalesce(type, "")))
}

draw_irr_sample <- function(lib, size, seed) {
  pool <- lib[!is_proceedings_record(lib$type), ]
  if (size > nrow(pool)) stop("Sample size ", size, " is larger than the ", nrow(pool), " eligible records.")
  score <- vapply(paste0(seed, ":", pool$duplicate_id), digest::digest,
                  character(1), algo = "sha256", serialize = FALSE)
  pool$duplicate_id[order(score)][seq_len(size)]
}

# Sheet for the raters: one row per record, blank decision columns.
irr_sheet <- function(lib, ids, raters) {
  sheet <- lib[match(ids, lib$duplicate_id), ] |>
    dplyr::mutate(irr_id = seq_along(ids)) |>
    dplyr::select(irr_id, duplicate_id, title, author = author_original,
                  year, journal, doi, abstract)
  for (r in raters) {
    sheet[[paste0(r, "_decision")]] <- ""
    sheet[[paste0(r, "_note")]] <- ""
  }
  sheet
}
