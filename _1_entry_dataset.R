# ==============================================================================
# The folder and settings_directive file
project_folder_name <- "Q312_utokyo"
settings_directive <- "settings_dataset_directive_2024-11-02-14-59.json"

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
        settings_directive
    ),
    simplify = FALSE
)
