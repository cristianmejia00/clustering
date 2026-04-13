# Unified pipeline launcher
# Usage:
#   source("scripts/run_pipeline.R")
#   run_pipeline(c("dataset", "analysis", "reports", "ai", "charts"))
#   run_pipeline(c("reports", "ai", "charts"))
#   run_pipeline(c("ai", "charts"))
#   run_pipeline(c("charts"))
#
# The stages run in the order given. Each stage is independent and can be
# combined in any sequence. Common sequences:
#
#   Full pipeline:     c("dataset", "analysis", "reports", "ai", "charts")
#   Rerun from reports: c("reports", "ai", "charts")
#   AI + charts only:  c("ai", "charts")

run_pipeline <- function(stages) {
  VALID_STAGES <- c("dataset", "analysis", "reports", "ai", "charts", "enriched_embeds")

  STAGE_SCRIPTS <- c(
    dataset        = "scripts/dataset_only.R",
    analysis       = "scripts/analysis_only.R",
    reports        = "scripts/reports_only.R",
    ai             = "scripts/ai_only.R",
    charts         = "scripts/charts_only.R",
    enriched_embeds = "scripts/enriched_embeds_only.R"
  )

  if (!is.character(stages) || length(stages) == 0) {
    stop("stages must be a character vector, e.g. c(\"reports\", \"ai\", \"charts\")")
  }

  invalid <- setdiff(stages, VALID_STAGES)
  if (length(invalid) > 0) {
    stop("Unknown pipeline stage(s): ", paste(invalid, collapse = ", "),
         "\nValid stages: ", paste(VALID_STAGES, collapse = ", "))
  }

  if (!file.exists("scripts/dataset_only.R")) {
    stop("Run this script from repository root.")
  }

  message("=== run_pipeline: ", paste(stages, collapse = " -> "), " ===")

  for (stage in stages) {
    script <- STAGE_SCRIPTS[[stage]]
    message("\n", paste(rep("=", 60), collapse = ""))
    message("  Stage: ", stage)
    message(paste(rep("=", 60), collapse = ""))
    source(script)
  }

  message("\n=== Pipeline finished: ", paste(stages, collapse = " -> "), " ===")
  invisible(TRUE)
}
