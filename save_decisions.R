# save_decisions.R ----------------------------------------------------------
# Copy the decisions you typed into output/manual_review_pairs.csv into
# decisions/manual_decisions.csv (the git-tracked file the pipeline reads).
#
#   Rscript save_decisions.R
#
# Only IDs, decision, reviewer, date and note are saved - no bibliographic
# data. New decisions replace earlier ones for the same pair; earlier
# decisions not in the review file are kept.

source("R/04_deduplicate.R")
source("R/05_groups.R")

config <- yaml::read_yaml("config.yml")
review_file <- file.path(config$output_dir, "manual_review_pairs.csv")
if (!file.exists(review_file)) stop("Run `Rscript run_pipeline.R` first.")

new <- read_decisions(review_file)
old <- read_decisions(config$decisions_file)

today <- format(Sys.Date())
new$date <- ifelse(is.na(new$date) | new$date == "", today, new$date)

saved <- dplyr::bind_rows(new, dplyr::anti_join(old, new, by = c("record_id1", "record_id2"))) |>
  dplyr::select(dplyr::all_of(decision_cols)) |>
  dplyr::arrange(record_id1, record_id2)

dir.create(dirname(config$decisions_file), showWarnings = FALSE)
readr::write_csv(saved, config$decisions_file, na = "")
message("Saved ", nrow(saved), " decisions (", sum(saved$decision == "match"),
        " match, ", sum(saved$decision == "no_match"), " no_match) to ",
        config$decisions_file)
