# ==============================================================================
# pipelines/dataset/network.R
#
# Builds a direct-citation network from the cleaned dataset.
#
# Prerequisites:
#   Edit config_dataset.yml with your project settings, then source this file.
#   Run pipelines/dataset/loader.R first to produce dataset_raw_cleaned.csv.
# ==============================================================================

source("utils/libraries.R")
source("utils/system_paths.R")
source("utils/load_config.R")

settings <- load_config("config_dataset.yml")
project_folder_name <- settings$metadata$project_folder

###############################################################################
# Load dataset
dataset <- readr::read_csv(file.path(
  output_folder_path,
  settings$metadata$project_folder,
  settings$network$from_filtered_dataset,
  "dataset_raw_cleaned.csv"
))

###############################################################################
###############################################################################
###############################################################################
# Compute network if needed
if (settings$network$get_network & settings$network$network_type == "direct_citation") {
  source("pipelines/dataset/utils/01_data_loading/01z_compute_direct_citation_network.R")
} else {
  print("User did not request a citation network")
}

###############################################################################
###############################################################################
###############################################################################
# Save
results_folder_path <- file.path(
  output_folder_path,
  project_folder_name,
  settings$network$from_filtered_dataset,
  settings$network$network_type
)

dir.create(file.path(results_folder_path), showWarnings = FALSE)

write.csv(network,
  file = file.path(results_folder_path, "network.csv"),
  row.names = FALSE
)

# Write the filtering settings in the same location
writeLines(
  RJSONIO::toJSON(settings$network,
    pretty = TRUE,
    auto_unbox = TRUE
  ),
  file.path(results_folder_path, "network_settings.json")
)

rm(list = ls())
