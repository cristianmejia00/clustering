
###############################################################################
###############################################################################
###############################################################################
# Load libraries and assets
source("04_utils/02_libraries.R")
source("settings.R")

# Are we computing our own citation network?
COMPUTE_NETWORK = TRUE
date_id <- settings$analysis_metadata$date_id
analysis_id <- settings$analysis_metadata$analysis_id

###############################################################################
# Create analysis folder
analysis_folder <- paste(date_id, analysis_id, sep = '_')
analysis_folder_path <- file.path(settings$analysis_metadata$bibliometrics_folder, 
                                  settings$analysis_metadata$project_folder, 
                                  analysis_folder)
dir.create(analysis_folder_path)

# Create network folder for this analysis
network_folder_path <- file.path(analysis_folder_path, 
                                 paste("network", date_id, analysis_id, sep = "_"))
dir.create(network_folder_path)


###############################################################################
dataset <- readr::read_csv(file.path(settings$analysis_metadata$bibliometrics_folder, 
                                     settings$analysis_metadata$project_folder, 
                                     glue("dataset_{settings$analysis_metadata$project_folder}.csv")))

# Compute network if needed
if (COMPUTE_NETWORK) {
  source('./01_data_loading/compute_direct_citation_network.R')
} else {
  network <- data.frame()
  orphans <- data.frame()
  dataset_original <- data.frame()
}

###############################################################################
# Save and FINISH!
###############################################################################

## Create directories
write.csv(dataset, file = file.path(network_folder_path, 'dataset_largest_component.csv'), row.names = FALSE)
write.csv(orphans, file = file.path(network_folder_path, 'orphans.csv'), row.names = FALSE)
write.csv(network, file = file.path(network_folder_path, 'network.csv'), row.names = FALSE)

rm(list = ls())
###############################################################################
###############################################################################
###############################################################################