# Enriched embeddings pipeline — re-encodes documents with cluster names prepended
# for improved subcluster spatial separation in UMAP.
#
# Expects: AI enrichment has been run (rcs_merged.csv at level0 and level1
# must contain a populated 'global_name' column).
#
# Produces: e01_enriched/ embeddings + fig_umap_scatter_enriched.svg

if (!file.exists("pipelines/dataset/build_enriched_embeddings.py")) {
  stop("Run this script from repository root.")
}

if (!file.exists("config_analysis.yml") || !file.exists("config_dataset.yml")) {
  stop("Missing config_analysis.yml or config_dataset.yml at repository root.")
}

source("utils/libraries.R")
source("utils/system_paths.R")
source("utils/load_config.R")

settings <- load_config("config_analysis.yml") |> add_legacy_aliases()

# ── Validate preconditions ──────────────────────────────────────────────────
recursive_level <- settings$params$recursive_level
if (is.null(recursive_level) || recursive_level < 1) {
  stop("enriched_embeds requires recursive_level >= 1 (current: ", recursive_level, ")")
}

# Build analysis root (citation_network path)
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

# Check rcs_merged.csv at both levels
for (lvl in c(0, recursive_level)) {
  rcs_path <- file.path(output_folder_reports, paste0("level", lvl), "rcs_merged.csv")
  if (!file.exists(rcs_path)) {
    stop("rcs_merged.csv not found at level ", lvl, ": ", rcs_path,
         "\n  Run the reports + AI pipeline first.")
  }
  rcs <- readr::read_csv(rcs_path, show_col_types = FALSE)
  if (!"global_name" %in% colnames(rcs)) {
    stop("global_name column missing in rcs_merged.csv at level ", lvl,
         "\n  Run the AI pipeline (with global_naming) first.")
  }
  has_names <- sum(!is.na(rcs$global_name) & nchar(trimws(rcs$global_name)) > 0)
  if (has_names == 0) {
    stop("No global_name values found in rcs_merged.csv at level ", lvl,
         "\n  Run the AI pipeline (with global_naming) first.")
  }
  message("Level ", lvl, ": ", has_names, " clusters with global names")
}

# ── Find Python interpreter (needs sentence_transformers from root .venv) ─────
py_exec <- file.path(".venv", "bin", "python3")
if (!file.exists(py_exec)) py_exec <- Sys.which("python3")
if (!nzchar(py_exec)) py_exec <- Sys.which("python")
if (!nzchar(py_exec)) {
  stop("Python not found. Install Python and set up .venv with sentence-transformers")
}

# ── Parse optional --force flag from command-line args ────────────────────────
cli_args <- commandArgs(trailingOnly = TRUE)
force_recompute <- "--force" %in% cli_args

# ── Step 1: Build enriched embeddings ────────────────────────────────────────
message("=== Building enriched embeddings (level ", recursive_level, ") ===")

embed_args <- c(
  "pipelines/dataset/build_enriched_embeddings.py",
  "--config-analysis", shQuote("config_analysis.yml"),
  "--config-dataset",  shQuote("config_dataset.yml"),
  "--level",           as.character(recursive_level)
)
if (force_recompute) embed_args <- c(embed_args, "--force")

embed_status <- system2(py_exec, args = embed_args, stdout = "", stderr = "")

if (embed_status != 0) {
  stop("build_enriched_embeddings.py exited with status ", embed_status)
}
message("Enriched embeddings step completed.")

# ── Step 2: Generate UMAP scatter from enriched embeddings (all levels) ──────
embeds_cfg <- yaml::read_yaml("config_dataset.yml")
enriched_dir <- file.path(
  settings$metadata$bibliometrics_folder,
  settings$metadata$project_folder,
  embeds_cfg$embeds$from_filtered_dataset,
  "e01_enriched"
)

subfolder_clusters <- if (settings$params$type_of_analysis == "citation_network") {
  "clusters"
} else {
  "topics"
}

seed_val <- if (!is.null(settings$params$seed)) settings$params$seed else 100
palette_path <- file.path(getwd(), "assets", "fukan_colors.json")

available_levels <- 0:recursive_level

for (level_report in available_levels) {
  message("=== Generating enriched UMAP scatter: level ", level_report, " ===")

  output_folder_level <- file.path(output_folder_reports, paste0("level", level_report))

  # Load environ.rdata if available, else build from dataset_minimal
  environ_path <- file.path(output_folder_level, "environ.rdata")
  if (file.exists(environ_path)) {
    load(environ_path, envir = e <- new.env())
    my_data <- e$myDataCorrect
  } else {
    dm_path <- file.path(output_folder_reports, "dataset_minimal.csv")
    if (!file.exists(dm_path)) {
      warning("Skipping level ", level_report,
              " — neither environ.rdata nor dataset_minimal.csv found")
      next
    }
    dm <- readr::read_csv(dm_path, show_col_types = FALSE)
    if (level_report == 0) {
      dm$X_C <- as.character(dm$level0)
    } else {
      sub_col <- paste0("subcluster_label", level_report)
      dm$X_C <- as.character(dm[[sub_col]])
    }
    my_data <- dm
  }

  doc_clusters_path <- file.path(output_folder_level, "doc_clusters_enriched.csv")
  readr::write_csv(
    data.frame(
      UT = as.character(my_data$UT),
      X_C = as.character(my_data$X_C)
    ),
    doc_clusters_path
  )

  rcs_path   <- file.path(output_folder_level, "rcs_merged.csv")
  output_svg <- file.path(output_folder_level, subfolder_clusters,
                          "fig_umap_scatter_enriched.svg")

  umap_status <- system2(
    py_exec,
    args = c(
      "pipelines/charts/umap_scatter.py",
      "--embeddings-dir", shQuote(enriched_dir),
      "--doc-clusters",   shQuote(doc_clusters_path),
      "--rcs",            shQuote(rcs_path),
      "--palette",        shQuote(palette_path),
      "--output",         shQuote(output_svg),
      "--seed",           as.character(seed_val),
      "--label-min",      "20",
      "--title",          shQuote(paste("Enriched Cluster Map - Level", level_report)),
      "--force"
    ),
    stdout = "", stderr = ""
  )

  if (umap_status != 0) {
    warning("UMAP scatter plot exited with status ", umap_status, " for level ", level_report)
  } else {
    message("Enriched UMAP saved: ", output_svg)
  }
}

message("=== Enriched embeddings pipeline finished ===")
