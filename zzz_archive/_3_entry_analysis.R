# ==============================================================================
# The folder and settings_directive file
project_folder_name = "Q344 - hydrogen economy"
analysis_folder_name <- "a01_cn__f01_dc__c01_lv_v2"
settings_directive <- "settings_analysis_directive_2026-03-20-10-03.json"

###############################################################################
# Call necessary libraries
source("zz_utils/02_libraries.R")
source("zz_utils/00_system_paths.R")

###############################################################################
# Load the directive file
settings <- RJSONIO::fromJSON(
    file.path(
        output_folder_path,
        project_folder_name,
        analysis_folder_name,
        settings_directive
    ),
    simplify = FALSE
)
