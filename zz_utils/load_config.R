# ==============================================================================
# load_config.R
#
# Utilities for loading YAML configuration files and converting them into the
# `settings` list expected by all downstream scripts in this project.
#
# Usage:
#   source("zz_utils/load_config.R")
#   settings <- load_config("config_dataset.yml")
#
# The function also archives a timestamped JSON snapshot to the output folder
# for reproducibility (matching the old directive-file workflow).
# ==============================================================================

#' Load a YAML configuration file and return it as an R list.
#'
#' @param yaml_path  Path to the YAML file (relative to project root or absolute).
#' @param archive_to Optional directory where a JSON snapshot will be saved.
#'                   If NULL, no snapshot is created.
#' @return A nested list mirroring the YAML structure.
load_config <- function(yaml_path, archive_to = NULL) {
  if (!file.exists(yaml_path)) {
    stop("Configuration file not found: ", yaml_path)
  }
  config <- yaml::read_yaml(yaml_path)

  # Archive a timestamped JSON copy for reproducibility
  if (!is.null(archive_to)) {
    dir.create(archive_to, showWarnings = FALSE, recursive = TRUE)
    snapshot_name <- sprintf("config_snapshot_%s.json",
                             format(Sys.time(), "%Y-%m-%d-%H-%M"))
    archive_file <- file.path(archive_to, snapshot_name)
    writeLines(
      RJSONIO::toJSON(config, pretty = TRUE, auto_unbox = TRUE),
      archive_file
    )
    message("Config archived to: ", archive_file)
  }
  return(config)
}

#' Adapt analysis config to the internal settings structure.
#'
#' The YAML uses descriptive keys (citation_network, topic_model, reporting).
#' Some downstream scripts still expect the legacy short names (cno, tmo, rp).
#' This function creates aliases so both forms work during migration.
#'
#' It also resolves conditional structures:
#'   - column_labels: selects the sub-key matching params$dataset_source
#'
#' @param config The list returned by load_config().
#' @return The same list with legacy aliases added.
add_legacy_aliases <- function(config) {
  # citation_network -> cno
  if (!is.null(config$citation_network)) {
    config$cno <- config$citation_network
  }
  # topic_model -> tmo
  if (!is.null(config$topic_model)) {
    config$tmo <- config$topic_model
  }
  # reporting -> rp
  if (!is.null(config$reporting)) {
    config$rp <- config$reporting
  }
  # Unify bibliometrics_directory naming
  if (!is.null(config$metadata$bibliometrics_directory)) {
    config$metadata$bibliometrics_folder <- config$metadata$bibliometrics_directory
  }
  if (!is.null(config$metadata$bibliometrics_folder)) {
    config$metadata$bibliometrics_directory <- config$metadata$bibliometrics_folder
  }

  # Resolve column_labels: select the sub-key matching dataset_source
  if (!is.null(config$reporting$column_labels) && !is.null(config$params$dataset_source)) {
    source_key <- config$params$dataset_source
    if (source_key %in% names(config$reporting$column_labels)) {
      config$reporting$column_labels <- config$reporting$column_labels[[source_key]]
      config$rp$column_labels <- config$reporting$column_labels
    }
  }

  return(config)
}
