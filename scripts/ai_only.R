# AI enrichment runner — calls the LLM pipeline without recomputing analysis/reports.
# Expects that the analysis has already been run (rcs_merged.csv and dataset exist).

if (!file.exists("pipelines/ai/enrich_clusters.py")) {
  stop("Run this script from repository root.")
}

if (!file.exists("config_analysis.yml")) {
  stop("Missing config_analysis.yml at repository root.")
}

source("utils/system_paths.R")
source("utils/libraries.R")
source("utils/load_config.R")

settings <- load_config("config_analysis.yml") |> add_legacy_aliases()

# Build output_folder_level (same logic as 00_execute_and_reports.R)
output_folder_reports <- if (settings$params$type_of_analysis == "citation_network") {
  file.path(
    settings$metadata$bibliometrics_folder,
    settings$metadata$project_folder,
    settings$metadata$analysis_id,
    settings$cno$clustering$algorithm,
    as.character(settings$cno$thresholding$threshold)
  )
} else {
  file.path(
    settings$metadata$bibliometrics_folder,
    settings$metadata$project_folder,
    settings$metadata$analysis_id
  )
}

available_levels <- if (settings$params$recursive_level > 0 &&
                        settings$params$type_of_analysis == "citation_network") {
  0:settings$params$recursive_level
} else {
  0
}

# Load the dataset and merge with clustering solution (X_C, X_E come from dataset_minimal)
if (settings$params$type_of_analysis %in% c("citation_network")) {
  dataset <- readr::read_csv(file.path(
    settings$metadata$bibliometrics_folder,
    settings$metadata$project_folder,
    settings$metadata$analysis_id,
    "dataset_comp.csv"
  ), show_col_types = FALSE)

  dataset_minimal <- readr::read_csv(file.path(
    settings$metadata$bibliometrics_folder,
    settings$metadata$project_folder,
    settings$metadata$analysis_id,
    settings$cno$clustering$algorithm,
    as.character(settings$cno$thresholding$threshold),
    "dataset_minimal.csv"
  ), show_col_types = FALSE)
} else {
  dataset <- readr::read_csv(file.path(
    settings$metadata$bibliometrics_folder,
    settings$metadata$project_folder,
    settings$metadata$filtered_folder,
    "dataset_raw_cleaned.csv"
  ), show_col_types = FALSE)

  dataset_minimal <- readr::read_csv(file.path(
    settings$metadata$bibliometrics_folder,
    settings$metadata$project_folder,
    settings$metadata$analysis_id,
    "dataset_minimal.csv"
  ), show_col_types = FALSE)
}

dataset_minimal$uuid <- dataset$uuid[match(dataset_minimal$UT, dataset$UT)]
dataset <- merge(
  dataset_minimal %>%
    dplyr::select(dplyr::all_of(c(
      "uuid",
      setdiff(colnames(dataset_minimal), colnames(dataset))
    ))),
  dataset,
  by = "uuid",
  all.x = TRUE, all.y = FALSE
)

# Find Python interpreter
py_exec <- file.path("pipelines", "ai", ".venv", "bin", "python3")
if (!file.exists(py_exec)) py_exec <- Sys.which("python3")
if (!nzchar(py_exec)) py_exec <- Sys.which("python")
if (!nzchar(py_exec)) {
  stop("Python not found. Install Python and run: pip install -r pipelines/ai/requirements.txt")
}

# Read compute tasks from config
llm_compute <- settings$llm$compute
if (is.null(llm_compute)) llm_compute <- character(0)

# Process deepest level first (e.g. level1 before level0) so that
# global_naming at level0 can use subcluster names as context.
ordered_levels <- sort(available_levels, decreasing = TRUE)

for (level_report in ordered_levels) {
  output_folder_level <- file.path(output_folder_reports, paste0("level", level_report))

  rcs_path <- file.path(output_folder_level, "rcs_merged.csv")
  if (!file.exists(rcs_path)) {
    warning("Skipping level ", level_report, " — rcs_merged.csv not found at: ", rcs_path)
    next
  }

  message("=== AI Enrichment: level ", level_report, " ===")

  # Remap X_C to the correct cluster column for this level
  # (level 0 = level0/X_C, level N = subcluster_labelN)
  level_dataset <- dataset
  if (level_report == 0) {
    if ("level0" %in% colnames(level_dataset)) {
      level_dataset$X_C <- as.character(level_dataset$level0)
    }
  } else {
    sub_col <- paste0("subcluster_label", level_report)
    if (sub_col %in% colnames(level_dataset)) {
      level_dataset$X_C <- as.character(level_dataset[[sub_col]])
    } else {
      warning("Skipping level ", level_report, " — column ", sub_col, " not found in dataset")
      next
    }
  }

  # Write the minimal dataset CSV
  ai_dataset_path <- file.path(output_folder_level, "dataset_for_ai.csv")
  ai_cols <- intersect(c("UT", "TI", "AB", "X_C", "X_E", "Z9", "PY"), colnames(level_dataset))
  readr::write_csv(level_dataset[, ai_cols], ai_dataset_path)

  # Step 1: Per-cluster enrichment (names, descriptions)
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
    message("AI enrichment completed for level ", level_report)
  } else {
    warning("AI enrichment exited with status ", ai_status, " for level ", level_report)
  }

  # Step 2: Global naming — rename all clusters at once for distinctiveness
  if ("global_naming" %in% llm_compute) {
    message("=== Global Naming: level ", level_report, " ===")

    # For level 0, pass subcluster summary as context (if available)
    subcluster_args <- character(0)
    if (level_report == 0) {
      # Find the deepest available subcluster summary
      for (sub_level in sort(setdiff(available_levels, 0), decreasing = TRUE)) {
        sub_summary <- file.path(output_folder_reports,
                                 paste0("level", sub_level),
                                 "cluster_summary.csv")
        if (file.exists(sub_summary)) {
          subcluster_args <- c("--subcluster-summary", shQuote(sub_summary))
          message("  Using subcluster context from level ", sub_level)
          break
        }
      }
    }

    gn_status <- system2(
      py_exec,
      args = c(
        "pipelines/ai/global_naming.py",
        "--rcs", shQuote(rcs_path),
        "--output-dir", shQuote(output_folder_level),
        subcluster_args
      ),
      stdout = "", stderr = ""
    )

    if (gn_status == 0) {
      message("Global naming completed for level ", level_report)
    } else {
      warning("Global naming exited with status ", gn_status, " for level ", level_report)
    }
  }
}

message("=== AI pipeline finished ===")
