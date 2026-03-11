# ==============================================================================
# The folder and settings_directive file
project_folder_name = "Q339_igem"
analysis_folder_name <- "a01_tm__f01_e01__hdbs"
settings_directive <- "settings_analysis_directive_2025-08-04-16-40.json"

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
