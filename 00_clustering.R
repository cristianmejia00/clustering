# 20180323 -> 20220526 -> 20221220
# Framework for Citation Network Analysis with Recursive Clustering, or Topic Models.

# Input.
# A dataset having:
# - Web of Science formatted file (header and columns)

# Output
# A `dataset.rdata` object
# Being the same input dataset but with the "X_C" column.

##########################################################
# Select root directory
# It should be the directory where this code is placed.
# setwd("/var/container/MAIN TOPIC-CLUSTERING") #Linux
# setwd(choose.dir()) #Windows
getwd()

##########################################################
# Load libraries and assets
source("04_utils/02_libraries.R")
source("settings.R")
#settings <- fromJSON("path/to/your/file.json")

# Read input files
network_folder_path <- file.path(settings$analysis_metadata$bibliometrics_folder, 
                                 settings$analysis_metadata$project_folder, 
                                 paste(settings$analysis_metadata$date_id, 
                                       settings$analysis_metadata$analysis_id, 
                                       sep = "_"),
                                 paste("network", 
                                       settings$analysis_metadata$date_id, 
                                       settings$analysis_metadata$analysis_id, 
                                       sep = "_"))

dataset <- readr::read_csv(file.path(network_folder_path, 
                                     glue("dataset_largest_component.csv")))
orphans <- readr::read_csv(file.path(network_folder_path, 
                                     glue("orphans.csv")))
network <- readr::read_csv(file.path(network_folder_path, 
                                     glue("network.csv")))

##########################################################
# Check all documents have a cluster assigned
if (any(is.na(dataset$X_C))) {
  print('CRITICAL: At least one document is missing cluster assignation!')
  print('Those papers are removed')
  dataset <- dataset[!is.na(dataset$X_C),]
}

##########################################################
# Document classification (Get clusters or Get topics)
if (settings$params$type_of_analysis %in% c("citation_network", "both")) {
  source(file.path(getwd(), "02_citation_network", "00_citation_network_clustering.R"))
}

# Auxiliary code to find the right number of clusters. And update the threshold.
# Get the clusters collecting 90% of papers or the top 10, whatever is the smallest number.
table(dataset_minimal$X_C) %>% sort(decreasing = TRUE) %>% prop.table %>% cumsum %>% plot
table(dataset_minimal$X_C) %>% sort(decreasing = TRUE) %>% prop.table %>% cumsum
table(dataset_minimal$X_C) %>% sort(decreasing = TRUE) %>%  plot()
table(dataset_minimal$X_C)
table(dataset_minimal$subcluster_label1)
table(dataset_minimal$subcluster_label1) %>% sort(decreasing = TRUE)
# Update the threshold in settings file.


# ========================================================================
# Save files
dataset_clustering_results <- dataset_minimal
write.csv(dataset_clustering_results, 
          file = file.path(network_folder_path, "dataset_clustering_results.csv"), 
          row.names = FALSE)

# ========================================================================
# Create the summary
source(file.path(getwd(), "03_reports", "03_general_summary.R"))


# # Orphans treatment
# if (settings$addons$include_orphans == "99" | settings$addons$include_orphans == "999") {
#   source(file.path(getwd(), "04_utils", "zz-append_orphans.R"))
# }
# 
# # Add-ons
# if (settings$params$type_of_analysis == "citation_network" & 
#     exists('g1') &
#     (settings$addons$page_rank | settings$addons$eigen_centrality | settings$addons$closeness_centrality | settings$addons$betweeness_centrality)) {
#   source(file.path(getwd(), "04_utils", "zz-centrality_meassures.R"))
# }
# file.path(report_path, "dataset_clustering.csv")


rm(list = ls())