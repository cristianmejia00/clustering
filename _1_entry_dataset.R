# ==============================================================================
# The folder and settings_directive file
dataset_folder_name <- "Q311_innovativeness"
settings_directive <- "settings_directive_2024-10-27-22-16.json"

###############################################################################
# Call necessary libraries
source("04_utils/02_libraries.R")
source("04_utils/00_system_paths.R")

###############################################################################
# Load the directive file
settings <- RJSONIO::fromJSON(
    file.path(
        bibliometrics_folder_path,
        dataset_folder_name,
        settings_directive
    ),
    simplify = FALSE
)
