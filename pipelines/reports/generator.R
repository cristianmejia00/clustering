# ==============================================================================
# pipelines/reports/generator.R
#
# Generates all reports, charts, and archives for the analysis.
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

# # Save Global environment
# save.image(file.path(output_folder_level, "environ_zz_reports.rdata"))
#
# # Save cluster IDS
# if ('fukan_original_cluster_id' %in% colnames(dataset)) {
#   print('Saving cluster id comparison for subclusters')
#   cluster_comparison <- dataset[c('X_C', 'fukan_X_C', 'fukan_original_cluster_id', 'fukan_subcluster_label')]
#   cluster_comparison <- cluster_comparison[!duplicated(cluster_comparison$fukan_subcluster_label),]
#   cluster_comparison <- cluster_comparison[order(cluster_comparison$fukan_X_C),]
#   write.csv(cluster_comparison, file = file.path(output_folder_level, "cluster_id_comparison.csv"), row.names = FALSE)
# }

###############################################################################
###############################################################################
###############################################################################
# AI Enrichment — LLM-generated cluster names and descriptions
# Calls pipelines/ai/enrich_clusters.py via subprocess (same pattern as
# build_embeddings.py).  Requires:  pip install -r pipelines/ai/requirements.txt

llm_compute <- settings$llm$compute
if (!is.null(llm_compute) && length(llm_compute) > 0) {
  message("=== AI Enrichment: generating cluster names and descriptions ===")

  # Write the minimal dataset CSV that the Python script needs
  ai_dataset_path <- file.path(output_folder_level, "dataset_for_ai.csv")
  ai_cols <- intersect(c("UT", "TI", "AB", "X_C", "X_E", "Z9", "PY"), colnames(dataset))
  readr::write_csv(dataset[, ai_cols], ai_dataset_path)

  rcs_path <- file.path(output_folder_level, "rcs_merged.csv")

  # Find a working Python interpreter — prefer the dedicated venv
  py_exec <- file.path("pipelines", "ai", ".venv", "bin", "python3")
  if (!file.exists(py_exec)) py_exec <- Sys.which("python3")
  if (!nzchar(py_exec)) py_exec <- Sys.which("python")
  if (!nzchar(py_exec)) {
    warning("Python not found — skipping AI enrichment. Install Python and ",
            "run: pip install -r pipelines/ai/requirements.txt")
  } else {
    # Step 1: Per-cluster enrichment
    ai_status <- system2(
      py_exec,
      args = c(
        "pipelines/ai/enrich_clusters.py",
        "--rcs", shQuote(rcs_path),
        "--dataset", shQuote(ai_dataset_path),
        "--output-dir", shQuote(output_folder_level)
      ),
      stdout = "", stderr = ""
    )
    if (ai_status == 0) {
      message("AI enrichment completed. Reloading rcs_merged.csv")
      rcs_merged <- readr::read_csv(rcs_path, show_col_types = FALSE)
    } else {
      warning("AI enrichment script exited with status ", ai_status,
              ". Cluster names/descriptions may be incomplete.")
    }

    # Step 2: Global naming (distinctive renaming across all clusters)
    if ("global_naming" %in% llm_compute) {
      message("=== Global Naming ===")
      gn_status <- system2(
        py_exec,
        args = c(
          "pipelines/ai/global_naming.py",
          "--rcs", shQuote(rcs_path),
          "--output-dir", shQuote(output_folder_level)
        ),
        stdout = "", stderr = ""
      )
      if (gn_status == 0) {
        message("Global naming completed. Reloading rcs_merged.csv")
        rcs_merged <- readr::read_csv(rcs_path, show_col_types = FALSE)
      } else {
        warning("Global naming exited with status ", gn_status)
      }
    }
  }
} else {
  message("No LLM compute tasks configured — skipping AI enrichment.")
}
