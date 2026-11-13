# irr_sample.R --------------------------------------------------------------
# Select a random sample of publications for the inter-rater reliability
# test. Run after run_pipeline.R:
#
#   Rscript irr_sample.R
#
# The first run draws the sample and saves its IDs to
# decisions/irr_sample_ids.csv (commit this file). Later runs reuse the saved
# IDs, so the sample never changes, even if the library is rebuilt. To draw a
# new sample, delete that file (or change the seed in config.yml) and rerun.

source("R/05_report.R")
source("R/06_irr_sample.R")

config <- yaml::read_yaml("config.yml")
irr <- config$irr_sample
lib_file <- file.path(config$output_dir, "deduplicated_library.csv")
if (!file.exists(lib_file)) stop("Run `Rscript run_pipeline.R` first.")
lib <- readr::read_csv(lib_file, col_types = readr::cols(.default = "c"),
                       progress = FALSE)

counts <- readr::read_csv(file.path(config$output_dir, "prisma_counts.csv"),
                          show_col_types = FALSE)
pending <- counts$n[grepl("awaiting manual decision", counts$step)]
if (length(pending) && pending > 0) {
  warning(pending, " possible duplicate pairs are still undecided, so the ",
          "sample could contain both copies of a duplicate. Ideally finish ",
          "the manual review before drawing the sample.", call. = FALSE)
}

if (file.exists(irr$ids_file)) {
  saved <- readr::read_csv(irr$ids_file, col_types = readr::cols(.default = "c"))
  ids <- saved$duplicate_id
  missing <- setdiff(ids, lib$duplicate_id)
  if (length(missing)) {
    stop(length(missing), " sampled records are no longer in the library ",
         "(e.g. merged by a later manual decision): ",
         paste(head(missing, 5), collapse = ", "),
         ". Delete ", irr$ids_file, " to draw a new sample.")
  }
  message("Using the saved sample in ", irr$ids_file, " (", length(ids), " records).")
} else {
  ids <- draw_irr_sample(lib, irr$size, irr$seed)
  readr::write_csv(
    tibble::tibble(irr_id = seq_along(ids), duplicate_id = ids,
                   seed = irr$seed, drawn_on = format(Sys.Date())),
    irr$ids_file)
  message("Drew ", length(ids), " records with seed ", irr$seed, " and saved the IDs to ",
          irr$ids_file, ". Commit this file.")
}

out <- file.path(config$output_dir, "irr_sample")
readr::write_excel_csv(irr_sheet(lib, ids, unlist(irr$raters)),
                       paste0(out, ".csv"), na = "")
write_ris(lib[match(ids, lib$duplicate_id), ], paste0(out, ".ris"))
message("Wrote ", out, ".csv (screening sheet) and ", out, ".ris (for screening tools).")
