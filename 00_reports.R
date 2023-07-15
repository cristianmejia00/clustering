# 20180323 -> 20220526
# Framework for Citation Network Analysis with Recursive Clustering, or Topic Models.

##########################################################
# Select root directory
# It should be the directory where this code (00_general_parameters.R) is placed.
#setwd("/var/container/MAIN TOPIC-CLUSTERING") #Linux
#setwd(choose.dir()) #Windows
getwd()

##########################################################
# Load libraries
source("04_utils/02_libraries.R")

# Load settings from the project we are interested in
#source(file.choose())
source("settings.R")



##########################################################
# Load data
load(file.path(analysis_metadata$input_folder, 
               analysis_metadata$query_id, 
               "dataset.rdata"))

# Special filtering for palm oil news 
# dataset$X_C <- dataset$cluster
# dataset$cluster_code <- dataset$cluster
# dataset$X_C_name <- as.character(dataset$X_C)
# dataset$related_topics <- dataset$X_C_name
# dataset <- dataset[dataset$X_C > 0,]
# dataset$X_E <- dataset$score
#myDataCorrect <- dataset

# facet_dataset <- dataset
# 
# myDataCorrect_backup <- myDataCorrect
# dataset_backup <- dataset
##########################################################
# Output Folder
output_folder_reports <- file.path(input_folder, analysis_metadata$query_id, analysis_metadata$project_id) #dataset-analysis
dir.create(output_folder_reports) 

##########################################################
# Verify the data is correctly formatted for reports
source(file.path(getwd(), "03_reports", "00_verify_data.R"))

# Reporting clusters PAPERS
if (params$type_of_dataset == "papers") {
  source(file.path(getwd(), "02_citation_network", "01_execute_and_reports.R"))
}

# Reporting clusters NEWS
if (params$type_of_dataset == "news") {
  output_folder <- output_folder_reports
  source(file.path(getwd(), "02_topic_model", "01_1_execute_and_reports.R"))
}

# Dataset merged RCS
source(file.path(getwd(), "03_reports", "15_rcs_merged.R"))

# Dataset figures
source(file.path(getwd(), "zz-dataset_figures.R"))

# RCS figures
source(file.path(getwd(), "zz-rcs_scatterplot.R"))

# Sentiment Analysis
source(file.path(getwd(), "zz-sentiment_analysis.R"))

# Save environ
save.image(file.path(input_folder, analysis_metadata$query_id, "reports_environ.rdata"))
