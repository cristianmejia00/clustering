# Full pipeline
# Dataset routine + analysis routine

if (!file.exists("scripts/dataset_only.R") || !file.exists("scripts/analysis_only.R")) {
  stop("Run this script from repository root.")
}

message("=== Full pipeline started ===")
source("scripts/dataset_only.R")
source("scripts/analysis_only.R")
message("=== Full pipeline completed successfully ===")
