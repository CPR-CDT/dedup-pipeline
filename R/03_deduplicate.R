# 03_deduplicate.R ----------------------------------------------------------
# Candidate matching with ASySD (Hair et al. 2023, BMC Biology 21:189).
#
# ASySD (1) normalises fields (upper case, punctuation removed, DOI prefixes
# stripped), (2) finds candidate pairs that agree exactly on blocking fields
# (e.g. title + author, DOI, author + year + pages), (3) scores each field
# with Jaro-Winkler similarity and (4) accepts a pair as a duplicate only if a
# rule combining several fields is met (e.g. title > 0.9, author > 0.75 and
# DOI > 0.95). Pairs that look similar but fail the rules are "maybe" pairs
# for manual review.
#
# We call ASySD's matching steps directly (rather than `dedup_citations()`)
# so that the accepted pairs are available as an edge list. Grouping and
# merging are then done in 04_groups.R, which lets us (a) apply manual
# decisions, including splitting a wrong automatic match, and (b) stop pairs
# with conflicting DOIs being merged automatically: ASySD's own DOI-conflict
# check contains `(journal|isbn > 0.9)`, which is always TRUE, so different
# reports (e.g. a conference paper and its journal version) can be merged.
# The ASySD version is pinned in renv.lock because these are internal
# functions.

run_asysd_matching <- function(recs) {
  input <- recs |>
    dplyr::mutate(label = NA_character_) |>
    dplyr::select(author, title, year, journal, abstract, doi, number, pages,
                  volume, isbn, record_id, label, source)

  ordered <- ASySD:::order_citations(input)
  formatted <- ASySD:::format_citations(ordered)
  pairs <- ASySD:::match_citations(formatted)
  if (is.null(pairs) || !nrow(pairs)) {
    return(list(true_pairs = empty_pairs(), maybe_pairs = empty_pairs()))
  }
  types <- ASySD:::identify_true_matches(pairs)

  # Similarity scores for every candidate pair, keyed by record_id.
  scores <- pairs |>
    dplyr::transmute(
      record_id1 = as.character(record_id1), record_id2 = as.character(record_id2),
      dplyr::across(c(title, author, abstract, year, journal, volume, number,
                      pages, doi, isbn), ~ round(as.numeric(.x), 3), .names = "sim_{.col}")
    ) |>
    normalise_pair_order() |>
    dplyr::distinct(record_id1, record_id2, .keep_all = TRUE)

  list(
    true_pairs = types$true_pairs |> normalise_pair_order() |>
      dplyr::distinct(record_id1, record_id2) |>
      dplyr::left_join(scores, by = c("record_id1", "record_id2")),
    maybe_pairs = types$maybe_pairs |> normalise_pair_order() |>
      dplyr::distinct(record_id1, record_id2) |>
      dplyr::left_join(scores, by = c("record_id1", "record_id2"))
  )
}

empty_pairs <- function() tibble::tibble(record_id1 = character(), record_id2 = character())

# Store each pair with the smaller record_id first so pairs can be joined.
normalise_pair_order <- function(pairs) {
  a <- as.character(pairs$record_id1)
  b <- as.character(pairs$record_id2)
  pairs$record_id1 <- pmin(a, b)
  pairs$record_id2 <- pmax(a, b)
  pairs
}

# Two DOIs refer to the same item if they are identical, differ only in the
# registrant prefix (journal moved publisher), or differ by <= 3 characters
# (typos are common in WoS DOIs, e.g. "10.11445/" for "10.1145/").
doi_equivalent <- function(d1, d2) {
  suffix <- function(d) sub("^10\\.[0-9]+/", "", d)
  same <- d1 == d2 | suffix(d1) == suffix(d2)
  close <- mapply(function(a, b) utils::adist(a, b)[1, 1] <= 3, d1, d2)
  same | close
}

# Split accepted pairs into those safe to merge and those whose DOIs
# conflict (sent to manual review instead).
check_doi_conflicts <- function(true_pairs, recs) {
  doi <- stats::setNames(recs$doi, recs$record_id)
  d1 <- doi[true_pairs$record_id1]
  d2 <- doi[true_pairs$record_id2]
  both <- !is.na(d1) & !is.na(d2)
  conflict <- both
  conflict[both] <- !doi_equivalent(d1[both], d2[both])
  list(accepted = true_pairs[!conflict, ], doi_conflicts = true_pairs[conflict, ])
}
