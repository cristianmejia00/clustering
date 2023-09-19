# 20180323 -> 20220526
# Framework for Citation Network Analysis with Recursive Clustering, or Topic Models.

##########################################################
# Select root directory
# It should be the directory where this code (00_general_parameters.R) is placed.
# setwd("/var/container/MAIN TOPIC-CLUSTERING") #Linux
# setwd(choose.dir()) #Windows
getwd()

##########################################################
# Load libraries
source("04_utils/02_libraries.R")

# Load settings from the project we are interested in
# source(file.choose())
source("settings.R")


##########################################################
# Output Folder
output_folder_reports <- file.path(settings$analysis_metadata$bibliometrics_folder, 
                                   settings$analysis_metadata$project_folder, 
                                   settings$analysis_metadata$analysis_folder)
dir.create(output_folder_reports)

##########################################################
# Load data
load(file.path(
  settings$analysis_metadata$bibliometrics_folder,
  settings$analysis_metadata$project_folder,
  settings$analysis_metadata$analysis_folder,
  "dataset_clustering.rdata"
))


##########################################################
# Verify the data is correctly formatted for reports
source(file.path(getwd(), "04_utils", "00_verify_data.R"))
zz_env <- list('x01' = ls())

# Reporting clusters
source(file.path(getwd(), "02_citation_network", "01_execute_and_reports.R"))

# Dataset merged RCS
source(file.path(getwd(), "03_reports", "15_rcs_merged.R"))

# figures
# Save PNG figures. Normal raster figures for easy navigation in PC.
extension <- 'png'
subfolder_dataset <- "charts_dataset"
subfolder_clusters <- "charts_clusters"
source(file.path(getwd(), "zz-charts_dataset.R"))
source(file.path(getwd(), "zz-charts_clusters_stats1.R"))
source(file.path(getwd(), "zz-charts_clusters_stats2.R"))
source(file.path(getwd(), "zz-charts_clusters_stats3.R"))
source(file.path(getwd(), "zz-charts_clusters_stats4.R"))
source(file.path(getwd(), "zz-charts_clusters_scatterplots.R"))
source(file.path(getwd(), "zz-charts_trends_and_clustered_bars.R"))


# Save PNG figures. Needed for notebook.
extension <- 'svg'
subfolder_dataset <- "index_files/charts"
subfolder_clusters <- "index_files/charts"
source(file.path(getwd(), "zz-charts_dataset.R"))
source(file.path(getwd(), "zz-charts_clusters_stats1.R"))
source(file.path(getwd(), "zz-charts_clusters_stats2.R"))
source(file.path(getwd(), "zz-charts_clusters_stats3.R"))
source(file.path(getwd(), "zz-charts_clusters_stats4.R"))
source(file.path(getwd(), "zz-charts_clusters_scatterplots.R"))
source(file.path(getwd(), "zz-charts_trends_and_clustered_bars.R"))


# Save code snapshot
files_to_save <- list.files(getwd(), full.names = TRUE, recursive = TRUE)
files_to_omit <- list.files(file.path(getwd(),'renv','library'), full.names = TRUE, recursive = TRUE)
files_to_save <- setdiff(files_to_save, files_to_omit)

# Not to zip Rdata environments as they are heavy and saved separately
files_to_save <- files_to_save[!grepl('rdata$', tolower(files_to_save))]
# Zip them. This needs Rtools to work
zip(zipfile = file.path(output_folder_level, 'source_code'),
    files = files_to_save)

# Save readable settings
writeLines(RJSONIO::toJSON(settings, pretty=TRUE, auto_unbox=TRUE), 
           file.path(output_folder_level, "settings.json"))
# Save settings object
save(settings, file = file.path(output_folder_level, "settings.rdata")) 

# Save package list
session_info <- sessionInfo()
save(session_info, file = file.path(output_folder_level, "sessionInfo.rdata")) 
writeLines(capture.output(sessionInfo()), file.path(output_folder_level, "sessionInfo.txt"))

# Save Global environment
save.image(file.path(output_folder_level, "environ_zz_reports.rdata"))

# Save cluster IDS
if ('fukan_original_cluster_id' %in% colnames(dataset)) {
  print('Saving cluster id comparison for subclusters')
  cluster_comparison <- dataset[c('X_C', 'fukan_X_C', 'fukan_original_cluster_id', 'fukan_subcluster_label')]
  cluster_comparison <- cluster_comparison[!duplicated(cluster_comparison$fukan_subcluster_label),]
  cluster_comparison <- cluster_comparison[order(cluster_comparison$fukan_X_C),]
  write.csv(cluster_comparison, file = file.path(output_folder_level, "cluster_id_comparison.csv"), row.names = FALSE)
}

