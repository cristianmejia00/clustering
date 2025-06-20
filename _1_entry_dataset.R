# ==============================================================================
# This code does not need to be run by yourself!
# Just replace the two following variables with the current analysis
# And click save.

# The folder and settings_directive file
project_folder_name <- "Q336_derwent_fukuhara"
settings_directive <- "settings_dataset_directive_2025-06-20-10-10.json"

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
