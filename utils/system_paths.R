# ==============================================================================
# System Paths
# ==============================================================================
# Sourced by: a00_data_loader.R, a01_network.R, a02_components.R, etc.
# Sets: output_folder_path  (root directory for all project data on cloud storage)
#
# Paths are resolved from project configuration files only to keep runs reproducible.
# If a path does not exist, execution stops early with a clear message.
# ==============================================================================

# Priority:
# 1) settings$metadata$bibliometrics_directory or settings$metadata$bibliometrics_folder
# 2) config_dataset.yml -> metadata.bibliometrics_directory
# 3) config_analysis.yml -> metadata.bibliometrics_folder / metadata.bibliometrics_directory

pick_path <- function(x) {
  if (is.null(x)) return("")
  if (!is.character(x) || length(x) == 0) return("")
  value <- trimws(x[[1]])
  if (is.na(value) || value == "") return("")
  value
}

resolve_from_settings <- function() {
  if (!exists("settings", inherits = TRUE)) return("")
  cfg <- get("settings", inherits = TRUE)
  if (!is.list(cfg) || is.null(cfg$metadata)) return("")
  value <- pick_path(cfg$metadata$bibliometrics_directory)
  if (value != "") return(value)
  pick_path(cfg$metadata$bibliometrics_folder)
}

resolve_from_yaml <- function(path) {
  if (!file.exists(path)) return("")
  cfg <- yaml::read_yaml(path)
  if (is.null(cfg) || is.null(cfg$metadata)) return("")
  value <- pick_path(cfg$metadata$bibliometrics_directory)
  if (value != "") return(value)
  pick_path(cfg$metadata$bibliometrics_folder)
}

output_folder_path <- resolve_from_settings()
if (output_folder_path == "") {
  output_folder_path <- resolve_from_yaml("config_dataset.yml")
}
if (output_folder_path == "") {
  output_folder_path <- resolve_from_yaml("config_analysis.yml")
}

if (output_folder_path == "") {
  stop(paste0(
    "Could not resolve bibliometrics root path from configuration.\n",
    "Define metadata.bibliometrics_directory in config_dataset.yml or ",
    "metadata.bibliometrics_folder/metadata.bibliometrics_directory in config_analysis.yml."
  ))
}

message("Using bibliometrics root from configuration files")

# Normalise path separators for the current OS
output_folder_path <- normalizePath(output_folder_path, mustWork = FALSE)

# Validate that the directory exists
if (!dir.exists(output_folder_path)) {
  stop(paste0(
    "output_folder_path does not exist:\n  ", output_folder_path, "\n",
    "Update the path value in your configuration file(s)."
  ))
}
