# 04_groups.R ---------------------------------------------------------------
# Turn matched pairs into duplicate groups and merge each group into one
# record.
#
# Every record is a node; every accepted pair is an edge. A group is a set
# of records connected by edges (connected component), so if A = B and
# B = C then A, B and C form one group.
#
# Edges come from:
#   auto      - ASySD rules, excluding pairs with conflicting DOIs
#   exact     - whole-proceedings records with identical title and year
#   manual    - pairs marked "match" in decisions/manual_decisions.csv
# Pairs marked "no_match" in the decisions file remove the auto edge.

decision_cols <- c("record_id1", "record_id2", "decision", "reviewer", "date", "note")

read_decisions <- function(path) {
  d <- if (file.exists(path)) {
    readr::read_csv(path, col_types = readr::cols(.default = "c"), progress = FALSE)
  } else {
    tibble::tibble(record_id1 = character(), record_id2 = character(),
                   decision = character())
  }
  d[setdiff(decision_cols, names(d))] <- NA_character_
  d$decision <- tolower(trimws(d$decision))
  bad <- !is.na(d$decision) & !d$decision %in% c("match", "no_match")
  if (any(bad)) {
    stop("Decisions must be 'match' or 'no_match'. Check rows: ",
         paste(which(bad) + 1, collapse = ", "), " of ", path)
  }
  d |>
    dplyr::filter(!is.na(decision)) |>
    normalise_pair_order() |>
    dplyr::distinct(record_id1, record_id2, .keep_all = TRUE)
}

# Proceedings volumes have no authors, so they are matched on exact title
# and year only.
exact_proceedings_pairs <- function(procs) {
  if (nrow(procs) < 2) return(empty_pairs())
  key <- paste(toupper(gsub("[^[:alnum:]]", "", procs$title)), procs$year)
  groups <- split(procs$record_id, key)
  pairs <- lapply(groups[lengths(groups) > 1], function(ids) {
    tibble::tibble(record_id1 = ids[1], record_id2 = ids[-1])
  })
  normalise_pair_order(dplyr::bind_rows(empty_pairs(), pairs))
}

build_edges <- function(auto_pairs, exact_pairs, decisions) {
  no_match <- decisions |> dplyr::filter(decision == "no_match")
  match <- decisions |> dplyr::filter(decision == "match")
  dplyr::bind_rows(
    auto_pairs |> dplyr::mutate(link = "auto"),
    exact_pairs |> dplyr::mutate(link = "exact"),
    match |> dplyr::select(record_id1, record_id2) |> dplyr::mutate(link = "manual")
  ) |>
    dplyr::anti_join(no_match, by = c("record_id1", "record_id2")) |>
    dplyr::distinct(record_id1, record_id2, .keep_all = TRUE)
}

# Assign each record to a group. The group's id is the record_id of its
# highest-priority member, which is also the record kept as the base.
assign_groups <- function(recs, edges, priority) {
  g <- igraph::graph_from_data_frame(
    edges[, c("record_id1", "record_id2")], directed = FALSE,
    vertices = data.frame(name = recs$record_id))
  comp <- igraph::components(g)$membership
  recs |>
    dplyr::mutate(component = comp[record_id],
                  rank = match(source, priority)) |>
    dplyr::arrange(component, rank, record_id) |>
    dplyr::group_by(component) |>
    dplyr::mutate(duplicate_id = dplyr::first(record_id),
                  is_primary = dplyr::row_number() == 1,
                  group_size = dplyr::n()) |>
    dplyr::ungroup() |>
    dplyr::select(-component, -rank)
}

# One row per group. Each field takes the first non-missing value in source
# priority order, so gaps in the primary record are filled from duplicates.
merge_groups <- function(membership) {
  first_value <- function(x) {
    x <- x[!is.na(x)]
    if (length(x)) x[1] else NA_character_
  }
  fields <- c("type", "author", "author_original", "year", "title", "journal",
              "volume", "number", "pages", "doi", "isbn", "abstract",
              "keywords", "url")
  membership |>
    dplyr::group_by(duplicate_id) |>
    dplyr::summarise(
      record_ids = paste(record_id, collapse = ", "),
      sources = paste(unique(source), collapse = ", "),
      n_records = dplyr::n(),
      dplyr::across(dplyr::all_of(fields), first_value),
      .groups = "drop"
    ) |>
    dplyr::arrange(match(duplicate_id, membership$duplicate_id))
}

# Pairs for a person to check: ASySD "maybe" pairs and DOI-conflict pairs,
# unless their records already ended up in the same group.
manual_review_pairs <- function(maybe_pairs, doi_conflicts, membership,
                                recs, decisions) {
  grp <- stats::setNames(membership$duplicate_id, membership$record_id)
  pairs <- dplyr::bind_rows(
    doi_conflicts |> dplyr::mutate(reason = "ASySD match, but DOIs differ"),
    maybe_pairs |> dplyr::mutate(reason = "ASySD possible match")
  ) |>
    dplyr::distinct(record_id1, record_id2, .keep_all = TRUE) |>
    dplyr::left_join(decisions |> dplyr::select(record_id1, record_id2, dplyr::any_of(c("decision", "reviewer", "date", "note"))),
                     by = c("record_id1", "record_id2"))
  # Keep pairs already decided. Of the undecided ones, drop pairs whose
  # records are already grouped together through other links, and show only
  # one pair per pair of groups (one "match" is enough to join two groups).
  pairs <- pairs |>
    dplyr::mutate(g1 = pmin(grp[record_id1], grp[record_id2]),
                  g2 = pmax(grp[record_id1], grp[record_id2])) |>
    dplyr::filter(!is.na(decision) | g1 != g2) |>
    dplyr::group_by(g1, g2) |>
    dplyr::filter(!is.na(decision) | !any(!is.na(decision)) & dplyr::row_number() == 1) |>
    dplyr::ungroup() |>
    dplyr::select(-g1, -g2)

  show <- c("source", "title", "author_original", "year", "journal", "volume",
            "number", "pages", "doi", "abstract")
  side <- function(ids, suffix) {
    recs[match(ids, recs$record_id), show] |>
      dplyr::mutate(abstract = substr(abstract, 1, 400)) |>
      dplyr::rename_with(~ paste0(.x, suffix))
  }
  dplyr::bind_cols(
    pairs |> dplyr::select(record_id1, record_id2, reason, dplyr::any_of(c("decision", "reviewer", "date", "note"))),
    side(pairs$record_id1, "_1"),
    side(pairs$record_id2, "_2"),
    pairs |> dplyr::select(dplyr::starts_with("sim_"))
  ) |>
    dplyr::mutate(decision = dplyr::coalesce(decision, ""),
                  reviewer = dplyr::coalesce(reviewer, ""),
                  date = dplyr::coalesce(date, ""),
                  note = dplyr::coalesce(note, "")) |>
    dplyr::relocate(decision, reviewer, date, note, .after = reason) |>
    dplyr::arrange(decision != "", reason, record_id1)
}

# Groups that still contain two clearly different DOIs (possible when they
# are linked through a record that has no DOI). Reported for checking.
groups_with_doi_conflicts <- function(membership) {
  membership |>
    dplyr::filter(!is.na(doi), group_size > 1) |>
    dplyr::group_by(duplicate_id) |>
    dplyr::filter(dplyr::n_distinct(doi) > 1) |>
    dplyr::filter({
      d <- unique(doi)
      any(!outer(d, d, Vectorize(doi_equivalent)))
    }) |>
    dplyr::ungroup()
}
