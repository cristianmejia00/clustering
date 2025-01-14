# ==============================================================================
# The folder and settings_directive file
project_folder_name = "Q321_TI_robot"
analysis_folder_name <- "a01_cn__f01_dc__c01_lv"
settings_directive <- "settings_analysis_directive_2025-01-11-23-00.json"

###############################################################################
# Call necessary libraries
source("04_utils/02_libraries.R")
source("04_utils/00_system_paths.R")

###############################################################################
# Load the directive file
settings <- RJSONIO::fromJSON(
    file.path(
        bibliometrics_folder_path,
        project_folder_name,
        analysis_folder_name,
        settings_directive
    ),
    simplify = FALSE
)
