# Analysis pipeline runner (citation network and/or topic model reports)

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

analysis_type <- cfg_analysis$params$type_of_analysis
if (is.null(analysis_type) || !analysis_type %in% c("citation_network", "topic_model", "both")) {
  stop("config_analysis.yml params$type_of_analysis must be one of: citation_network, topic_model, both")
}

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

topic_dataset_minimal_csv <- file.path(
  output_folder_path,
  project_folder,
  analysis_id,
  "dataset_minimal.csv"
)

if (!file.exists(dataset_csv)) {
  stop(paste("Required dataset artifact not found:", dataset_csv, "Run scripts/dataset_only.R first."))
}

if (analysis_type %in% c("citation_network", "both") && !file.exists(network_csv)) {
  stop(paste("Required network artifact not found:", network_csv, "Run scripts/dataset_only.R first."))
}

if (analysis_type %in% c("topic_model", "both")) {
  if (!file.exists(topic_dataset_minimal_csv)) {
    stop(paste(
      "Required topic-model artifact not found:",
      topic_dataset_minimal_csv,
      "Run the topic-model notebook first to generate dataset_minimal.csv."
    ))
  }

  embeds_folder <- cfg_analysis$topic_model$embeds_folder
  if (is.null(embeds_folder) || !nzchar(embeds_folder)) {
    stop("config_analysis.yml topic_model$embeds_folder must be set for topic_model/both runs")
  }

  embed_dir <- file.path(output_folder_path, project_folder, filtered_folder, embeds_folder)
  required_embed_artifacts <- c("embeddings.npy", "embeddings_ids.json", "corpus.csv")
  for (artifact in required_embed_artifacts) {
    artifact_path <- file.path(embed_dir, artifact)
    if (!file.exists(artifact_path)) {
      stop(paste(
        "Required topic-model embeddings artifact not found:",
        artifact_path,
        "Run scripts/dataset_only.R first."
      ))
    }
  }
}

if (analysis_type %in% c("citation_network", "both")) {
  message("=== Step 1/4: Components ===")
  source("pipelines/analysis/citation_network/components.R")

  message("=== Step 2/4: Clustering ===")
  source("pipelines/analysis/citation_network/clustering.R")

  message("=== Step 3/4: Thresholding ===")
  source("pipelines/analysis/citation_network/thresholding.R")
}

message("=== Reports: Generate outputs for type_of_analysis = ", analysis_type, " ===")
source("pipelines/reports/generator.R")

analysis_folder <- file.path(output_folder_path, project_folder, analysis_id)
if (!dir.exists(analysis_folder)) {
  stop(paste("Expected analysis output folder not found:", analysis_folder))
}

message("=== Analysis pipeline completed successfully ===")
