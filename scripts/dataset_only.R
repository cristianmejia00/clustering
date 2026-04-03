# Dataset pipeline (required artifacts):
# 1) dataset_raw_cleaned.csv
# 2) citation network
# 3) embeddings (.npy + ids + corpus) for all embed profiles

if (!file.exists("pipelines/dataset/loader.R") || !file.exists("pipelines/dataset/network.R")) {
  stop("Run this script from repository root.")
}

if (!file.exists("config_dataset.yml")) {
  stop("Missing config_dataset.yml at repository root.")
}

message("=== Step 1/3: Load and clean dataset ===")
source("pipelines/dataset/loader.R")

message("=== Step 2/3: Build citation network ===")
source("pipelines/dataset/network.R")

message("=== Step 3/3: Compute embeddings for all profiles ===")
py_exec <- Sys.which("python")
if (py_exec == "") {
  py_exec <- Sys.which("python3")
}
if (py_exec == "") {
  stop("Python executable not found in PATH.")
}

status <- system2(py_exec, c("pipelines/dataset/build_embeddings.py"))
if (!identical(status, 0L)) {
  stop("Embedding generation failed. Check Python environment and pipelines/analysis/topic_model/requirements_embeds.txt.")
}

# Re-read config (prior scripts clear workspace with rm(list = ls()))
cfg <- yaml::read_yaml("config_dataset.yml")
source("utils/system_paths.R")
project_folder <- cfg$metadata$project_folder
filtered_folder <- cfg$embeds$from_filtered_dataset
embed_keys <- names(cfg$embeds)
embed_keys <- embed_keys[grepl("^e", embed_keys)]

for (ek in embed_keys) {
  embed_dir <- file.path(output_folder_path, project_folder, filtered_folder, ek)
  expected <- c("embeddings.npy", "embeddings_ids.json", "corpus.csv", "embeds_settings.json")
  missing <- expected[!file.exists(file.path(embed_dir, expected))]
  if (length(missing) > 0) {
    stop(paste0("Embedding artifacts missing for ", ek, " in ", embed_dir, ": ", paste(missing, collapse = ", ")))
  }
}

message("=== Dataset pipeline completed successfully ===")
