#==============================================================================
# The folder and settings_directive file
analysis_folder_name <- "Q311_a01_f001_cn_dc_c0_lv"
settings_directive <- "settings_directive_analysis_2024-10-28-11-10.json"

###############################################################################
# Call necessary libraries
source("04_utils/02_libraries.R")
source("04_utils/00_system_paths.R")

###############################################################################
# Load the directive file
settings <- RJSONIO::fromJSON(file.path(root_path_to, 
                                        analysis_folder_name,
                                        settings_directive), 
                              simplify = FALSE)