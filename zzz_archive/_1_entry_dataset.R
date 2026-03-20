# ==============================================================================
# This code does not need to be run by yourself!
# Just replace the two following variables with the current analysis
# And click save.

# The folder and settings_directive file
project_folder_name <- "Q344 - hydrogen economy"
settings_directive <- "settings_dataset_directive_2026-03-20-09-16.json"

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
        settings_directive
    ),
    simplify = FALSE
)
