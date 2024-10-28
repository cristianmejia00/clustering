

#==============================================================================
# The folder and settings_directive file
dataset_folder_name <- "Q311_innovativeness"
settings_directive <- "settings_directive_2024-10-27-22-16.json"

###############################################################################
# Call necessary libraries
source("04_utils/02_libraries.R")
source("04_utils/00_system_paths.R")

###############################################################################
# Load the directive file
settings <- RJSONIO::fromJSON(file.path(root_path_to, 
                                        dataset_folder_name,
                                        settings_directive), 
                              simplify = FALSE)
###############################################################################
# Load dataset
dataset <- readr::read_csv(file.path(settings$metadata$bibliometrics_directory, 
                                     settings$metadata$dataset_folder,
                                     settings$network$from_filtered_dataset,
                                     "dataset.csv"))

# Compute network if needed
if (settings$network$get_network & settings$network$network_type == "direct_citation") {
  source('./01_data_loading/compute_direct_citation_network.R')
} else {
  print("User did not request acitation network")
}

###############################################################################
# Save and FINISH!
###############################################################################
results_folder_path <- file.path(root_path_to,
                                 dataset_folder_name,
                                 settings$network$from_filtered_dataset,
                                 settings$network$network_type)

dir.create(file.path(results_folder_path), showWarnings = FALSE)

write.csv(network, 
          file = file.path(results_folder_path, "network.csv"), 
          row.names = FALSE)

# Write the filtering settings in the same location
writeLines(RJSONIO::toJSON(settings$network, 
                           pretty = TRUE,
                           auto_unbox = TRUE),
           file.path(results_folder_path, "network_settings.json"))

rm(list = ls())