# run_pipeline.R ------------------------------------------------------------
# Deduplicate the systematic review search results, start to finish.
#
#   Rscript run_pipeline.R
#
# Reads the raw exports listed in config.yml, writes everything to output/.
# Same raw files + same decisions/manual_decisions.csv = same result.

suppressPackageStartupMessages({
  library(dplyr)
})

for (f in sort(list.files("R", pattern = "^0[0-9]_.*\\.R$", full.names = TRUE))) source(f)

config <- yaml::read_yaml("config.yml")
priority <- unlist(config$source_priority)

message("1/6 Importing raw exports...")
files <- list_source_files(config)
recs <- clean_records(import_all(config))
print(dplyr::count(recs, source, name = "records"))

message("2/6 Enriching records from OpenAlex / Crossref...")
enrichment <- enrich_records(recs, config$enrichment)
recs <- enrichment$records
cache_file <- config$enrichment$cache_file
if (isTRUE(config$enrichment$enabled) && file.exists(cache_file)) {
  files <- dplyr::bind_rows(files, tibble::tibble(
    source = "Enrichment cache", format = "cache", path = cache_file))
}
manifest <- input_manifest(files, recs)

message("3/6 Matching with ASySD...")
# Whole-proceedings volumes (ACM front matter, no authors) are not compared
# with articles; they only match each other on exact title + year.
is_proc <- recs$type %in% "PROCEEDINGS"
asysd <- run_asysd_matching(recs[!is_proc, ])
checked <- check_doi_conflicts(asysd$true_pairs, recs)

message("4/6 Applying manual decisions and grouping...")
decisions <- read_decisions(config$decisions_file)
edges <- build_edges(checked$accepted, exact_proceedings_pairs(recs[is_proc, ]), decisions)
membership <- assign_groups(recs, edges, priority)
library_merged <- merge_groups(membership)

message("5/6 Preparing manual review list...")
review <- manual_review_pairs(asysd$maybe_pairs, checked$doi_conflicts,
                              membership, recs, decisions)
n_pending <- sum(review$decision == "")
conflicts <- groups_with_doi_conflicts(membership)

message("6/6 Writing outputs...")
counts <- prisma_counts(recs, membership, edges, n_pending)
summary <- write_outputs(config$output_dir, library_merged, membership, edges,
                         review, manifest, counts, conflicts)
readr::write_excel_csv(enrichment$log, file.path(config$output_dir, "enrichment_log.csv"), na = "")
cat(summary, sep = "\n")

if (n_pending > 0) {
  message("\n", n_pending, " possible duplicate pairs need a decision: fill in ",
          "the 'decision' column of ", file.path(config$output_dir, "manual_review_pairs.csv"),
          ", then run `Rscript save_decisions.R` and rerun this script.")
}
if (nrow(conflicts) > 0) {
  message(dplyr::n_distinct(conflicts$duplicate_id), " groups contain different DOIs; see ",
          file.path(config$output_dir, "groups_with_doi_conflicts.csv"))
}
