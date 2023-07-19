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
# Load libraries
source("04_utils/02_libraries.R")

# Load input settings file
source("settings.R")

# Load data
load(file.path(
  settings$analysis_metadata$bibliometrics_folder,
  settings$analysis_metadata$project_folder,
  "dataset.rdata"
))

# Auxiliary code to find the right number of clusters. And update the threshold.
# Get the clusters collecting 90% of papers or the top 10, whatever is the smallest number.
# table(dataset$X_C) %>% sort(decreasing = TRUE) %>% prop.table %>% cumsum %>% plot
# table(dataset$X_C) %>% sort(decreasing = TRUE) %>% prop.table %>% cumsum
# table(dataset$X_C) %>% sort(decreasing = TRUE) %>%  plot()
# Update the threshold in settings file.


##########################################################
# Document classification (Get clusters or Get topics)
if (settings$params$type_of_analysis == "citation_network") {
  source(file.path(getwd(), "02_citation_network", "00_citation_network_clustering.R"))
}
if (settings$params$type_of_analysis == "topic_model") {
  source(file.path(getwd(), "02_topic_model", "00_topic_model_clustering.R"))
}

# Create stats folder
dir.create(file.path(
  settings$analysis_metadata$bibliometrics_folder,
  settings$analysis_metadata$project_folder,
  settings$analysis_metadata$analysis_folder
))
source(file.path(getwd(), "03_reports", "03_general_summary.R"))


# Orphans treatment
if (settings$addons$include_orphans == "99" | settings$addons$include_orphans == "999") {
  source(file.path(getwd(), "04_utils", "zz-append_orphans.R"))
}

# Add-ons
if (settings$params$type_of_analysis == "citation_network" & 
    exists('g1') &
    (settings$addons$page_rank | settings$addons$eigen_centrality | settings$addons$closeness_centrality | settings$addons$betweeness_centrality)) {
  source(file.path(getwd(), "04_utils", "zz-centrality_meassures.R"))
}

##########################################################
# save objects
if (settings$params$type_of_analysis == "topic_model") {
  dataset <- myDataCorrect
}

if (settings$params$type_of_analysis == "citation_network") {
  dataset <- merge(dataset, dataset_minimal[, c("X_N", "level0")])
  setnames(dataset, "X_C", "fukan_X_C")
  setnames(dataset, "level0", "X_C")
}

save(dataset, file = file.path(settings$analysis_metadata$bibliometrics_folder, 
                               settings$analysis_metadata$project_folder, 
                               settings$analysis_metadata$analysis_folder,
                               "dataset_clustering.rdata"))

save.image(file.path(settings$analysis_metadata$bibliometrics_folder, 
                     settings$analysis_metadata$project_folder, 
                     settings$analysis_metadata$analysis_folder,
                     "environ_clustering.rdata"))
rm(list = ls())