# ==============================================================================
# This code does not need to be run by yourself!
# Just replace the two following variables with the current analysis
# And click save.

# The folder and settings_directive file
project_folder_name <- "Q319_sust_rice"
settings_directive <- "settings_dataset_directive_2024-12-18-20-57.json"

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
