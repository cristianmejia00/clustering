# Unified pipeline launcher
# Usage:
#   source("scripts/run_pipeline.R")
#   run_pipeline("dataset")
#   run_pipeline("analysis")
#   run_pipeline("full")
#   run_pipeline("topic")

run_pipeline <- function(mode = c("full", "dataset", "analysis", "topic")) {
  mode <- match.arg(mode)

  if (!file.exists("scripts/dataset_only.R")) {
    stop("Run this script from repository root.")
  }

  message(paste("=== run_pipeline mode:", mode, "==="))

  if (mode == "dataset") {
    source("scripts/dataset_only.R")
  } else if (mode == "analysis") {
    source("scripts/analysis_only.R")
  } else if (mode == "full") {
    source("scripts/full_pipeline.R")
  } else if (mode == "topic") {
    source("scripts/topic_model_only.R")
  }

  invisible(TRUE)
}
