####################################################
# Reports
####################################################
level_report <<- 0

# Get the level path and create the directory
output_folder_level <- output_folder_reports

rn <- list()
rn$PROJECTarticlereport <- file.path(output_folder_level, "article_report.csv")
rn$PROJECTrcs <- file.path(output_folder_level, "rcs.csv")
rn$PROJECTrcs2 <- file.path(output_folder_level, "rcs2.csv")
rn$PROJECTrcsviz <- file.path(output_folder_level, "rcs_viz.html")
rn$PROJECTKeywords <- file.path(output_folder_level, "ALL_Cluster_keywords.rdata")
rn$PROJECTKeywords_report <- file.path(output_folder_level, "report_keywords.csv")
rn$PROJECTenviron <- file.path(output_folder_level, "environ.rdata")

# Auxiliary functions
source("04_utils/zz_auxiliary_functions.R")

# Reports
source("03_reports/01_document_report_with_abstract.R")
source("03_reports/04_cluster_reports.R")
source("03_reports/02_rcs.R")

# Keyword reports
recursive_level <- 0

source("03_reports/05_heatmap_keywords_part_1.R")
if (!(exists("myDataCorrect_SAMPLE") & exists("papersText"))) {
  source("03_reports/05_heatmap_keywords_part_2.R")
}
source("03_reports/05_heatmap_keywords_part_3.R")


source("04_utils/zz_createJSON_cnet.R")
source("03_reports/06_citation_to_topic_model_converter.R")

K <- length(unique(dataset$X_C))
source("03_reports/07_prob_exclu_keywords.R")
source("03_reports/08_all_keywords_report.R")

# Optional views
# source("03_reports/09_keywords_per_clusters.R")
source("03_reports/10_rcs_keywords.R")

# Overlays (Only for WOS data)
if (params$dataset_source == "wos") {
  source(file.path(getwd(), "03_reports", "13_WC_overlays.R"))
}

# Save environmet
save.image(rn$PROJECTenviron)
