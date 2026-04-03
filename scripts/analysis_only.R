# Analysis pipeline (citation network + reports)

if (!file.exists("pipelines/analysis/citation_network/components.R") || !file.exists("pipelines/reports/generator.R")) {
  stop("Run this script from repository root.")
}

if (!file.exists("config_analysis.yml")) {
  stop("Missing config_analysis.yml at repository root.")
}

if (!file.exists("config_dataset.yml")) {
  stop("Missing config_dataset.yml at repository root.")
}

cfg_analysis <- yaml::read_yaml("config_analysis.yml")
cfg_dataset <- yaml::read_yaml("config_dataset.yml")

source("utils/system_paths.R")

project_folder <- cfg_analysis$metadata$project_folder
filtered_folder <- cfg_analysis$metadata$filtered_folder
analysis_id <- cfg_analysis$metadata$analysis_id

dataset_csv <- file.path(
  output_folder_path,
  project_folder,
  filtered_folder,
  "dataset_raw_cleaned.csv"
)

network_csv <- file.path(
  output_folder_path,
  project_folder,
  filtered_folder,
  cfg_analysis$citation_network$network_type,
  "network.csv"
)

if (!file.exists(dataset_csv)) {
  stop(paste("Required dataset artifact not found:", dataset_csv, "Run scripts/dataset_only.R first."))
}

if (!file.exists(network_csv)) {
  stop(paste("Required network artifact not found:", network_csv, "Run scripts/dataset_only.R first."))
}

embed_keys <- names(cfg_dataset$embeds)
embed_keys <- embed_keys[grepl("^e", embed_keys)]
for (ek in embed_keys) {
  embed_dir <- file.path(output_folder_path, project_folder, cfg_dataset$embeds$from_filtered_dataset, ek)
  if (!file.exists(file.path(embed_dir, "embeddings.npy"))) {
    stop(paste("Required embeddings artifact not found for", ek, "at", embed_dir, "Run scripts/dataset_only.R first."))
  }
}

message("=== Step 1/4: Components ===")
source("pipelines/analysis/citation_network/components.R")

message("=== Step 2/4: Clustering ===")
source("pipelines/analysis/citation_network/clustering.R")

message("=== Step 3/4: Thresholding ===")
source("pipelines/analysis/citation_network/thresholding.R")

message("=== Step 4/4: Reports ===")
source("pipelines/reports/generator.R")

analysis_folder <- file.path(output_folder_path, project_folder, analysis_id)
if (!dir.exists(analysis_folder)) {
  stop(paste("Expected analysis output folder not found:", analysis_folder))
}

message("=== Analysis pipeline completed successfully ===")
