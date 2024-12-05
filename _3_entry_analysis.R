# ==============================================================================
# The folder and settings_directive file
project_folder_name = "Q318_MKT_poverty"
analysis_folder_name <- "a01_tm__f01_e01__km01"
settings_directive <- "settings_analysis_directive_2024-12-05-17-33.json"

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
