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
#settings$params$recursive_level <- 1

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
#dataset$X_E <- dataset$Z9
dataset$X_E[is.na(dataset$X_E)] <- 0
zz_env <- list('x01' = ls())

# Reporting clusters
source(file.path(getwd(), "02_citation_network", "01_execute_and_reports.R"))


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

###############################################
# LLM
# Using OpenAI and Claude in R.
source("zz-llm_v2_0_prompts.R")
source("zz-llm_v2_1_functions.R")
source("zz-llm_v2_2_execution.R")

source("zz-create_bib_file.R")

source("zz-generate_quarto_document.R")
#source("zz-generate_quarto_word.R")

###############################################
# Send to display
source("zzz-send_to_display_repo.R")
