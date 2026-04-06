# Charts pipeline runner — generates all visualizations (SVG charts + UMAP scatter).
# Expects: reports pipeline has already been run (environ.rdata + rcs_merged.csv
# exist at each level).

if (!file.exists("pipelines/charts/generator.R")) {
  stop("Run this script from repository root.")
}

if (!file.exists("config_analysis.yml")) {
  stop("Missing config_analysis.yml at repository root.")
}

source("utils/libraries.R")
source("utils/system_paths.R")
source("utils/load_config.R")

settings <- load_config("config_analysis.yml") |> add_legacy_aliases()

message("=== Charts pipeline started ===")
source("pipelines/charts/generator.R")
message("=== Charts pipeline finished ===")
