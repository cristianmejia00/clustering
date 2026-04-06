# ==============================================================================
# pipelines/reports/generator.R
#
# Generates report tables (RCS, article report, cluster reports).
# Charts and AI enrichment are handled by separate pipelines.
# ==============================================================================

source("utils/libraries.R")
source("utils/system_paths.R")
source("utils/load_config.R")

settings <- load_config("config_analysis.yml") |> add_legacy_aliases()

###############################################################################
# Load raw dataset

# Citation network assets
if (settings$params$type_of_analysis %in% c("citation_network")) {
  dataset <- readr::read_csv(file.path(
    settings$metadata$bibliometrics_folder,
    settings$metadata$project_folder,
    settings$metadata$analysis_id,
    "dataset_comp.csv"
  ))

  # Load the clustering solution once thresholded
  dataset_minimal <- readr::read_csv(file.path(
    settings$metadata$bibliometrics_folder,
    settings$metadata$project_folder,
    settings$metadata$analysis_id,
    settings$cno$clustering$algorithm,
    settings$cno$thresholding$threshold %>% as.character(),
    "dataset_minimal.csv"
  ))
}

# Topic Model
if (settings$params$type_of_analysis %in% c("topic_model", "both")) {
  dataset <- readr::read_csv(file.path(
    settings$metadata$bibliometrics_folder,
    settings$metadata$project_folder,
    settings$metadata$filtered_folder,
    "dataset_raw_cleaned.csv"
  ))

  # Load the clustering solution once thresholded
  dataset_minimal <- readr::read_csv(file.path(
    settings$metadata$bibliometrics_folder,
    settings$metadata$project_folder,
    settings$metadata$analysis_id,
    "dataset_minimal.csv"
  ))
}


# Ensure we have all the papers in the network
dataset_minimal$uuid <- dataset$uuid[match(dataset_minimal$UT, dataset$UT)]
stopifnot(all(dataset_minimal$uuid %in% dataset$uuid))

# Merge them
dataset <- merge(
  dataset_minimal %>%
    select(all_of(c(
      "uuid",
      setdiff(
        colnames(dataset_minimal),
        colnames(dataset)
      )
    ))),
  dataset,
  by = "uuid",
  all.x = TRUE,
  all.y = FALSE
)

# Verify the data is correctly formatted for reports
source(file.path(getwd(), "utils", "verify_data.R"))
zz_env <- list("x01" = ls())

###############################################################################
###############################################################################
###############################################################################
# Reporting clusters
source(file.path(
  getwd(),
  "pipelines",
  "reports",
  "helpers",
  "00_execute_and_reports.R"
))

###############################################################################
###############################################################################
###############################################################################
# Save code snapshot
files_to_save <- list.files(getwd(), full.names = TRUE, recursive = TRUE, pattern = "*\\.R$|*\\.r$")
files_to_omit <- list.files(file.path(getwd(), "renv", "library"), full.names = TRUE, recursive = TRUE, pattern = "*\\.R$|*\\.r$")
files_to_save <- setdiff(files_to_save, files_to_omit)

# Not to zip Rdata environments as they are heavy and saved separately
files_to_save <- files_to_save[!grepl("rdata$", tolower(files_to_save))]
# Zip them. This needs Rtools to work
zip(
  zipfile = file.path(output_folder_level, "source_code"),
  files = files_to_save
)


# Save package list
session_info <- sessionInfo()
save(session_info, file = file.path(output_folder_level, "sessionInfo.rdata"))
writeLines(capture.output(sessionInfo()), file.path(output_folder_level, "sessionInfo.txt"))

message("=== Reports pipeline completed ===")
