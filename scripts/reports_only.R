# Reports pipeline runner — generates report tables (RCS, article report,
# cluster reports) without charts or AI enrichment.
# Expects: analysis has already been run (clustering solution exists).

if (!file.exists("pipelines/reports/generator.R")) {
  stop("Run this script from repository root.")
}

if (!file.exists("config_analysis.yml")) {
  stop("Missing config_analysis.yml at repository root.")
}

message("=== Reports pipeline started ===")
source("pipelines/reports/generator.R")
message("=== Reports pipeline finished ===")
