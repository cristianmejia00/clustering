# ==============================================================================
# pipelines/charts/generator.R
#
# Generates all chart visualizations for the analysis.
# Can run independently after the reports pipeline has produced rcs_merged.csv
# and environ.rdata at each level.
#
# Expects: settings, dataset already loaded in the environment
#          (sourced from scripts/charts_only.R or after reports pipeline)
# ==============================================================================

print("###################### charts/generator.R")

# ---------------------------------------------------------------------------
# Output folder based on analysis type
# ---------------------------------------------------------------------------
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

# ---------------------------------------------------------------------------
# Chart levels
# ---------------------------------------------------------------------------
available_levels <- if (settings$params$recursive_level > 0 &&
                        settings$params$type_of_analysis == "citation_network") {
  0:settings$params$recursive_level
} else {
  0
}

for (level_report_iteration in available_levels) {
  level_report <<- level_report_iteration
  print(paste("...Starting charts for level", level_report))

  output_folder_level <- file.path(
    output_folder_reports,
    paste0("level", level_report)
  )

  # Load the saved environment from the reports pipeline for this level.
  # This provides: myDataCorrect, rcs_merged, K, rn, etc.
  environ_path <- file.path(output_folder_level, "environ.rdata")
  if (!file.exists(environ_path)) {
    warning("Skipping level ", level_report,
            " — environ.rdata not found at: ", environ_path,
            ". Run the reports pipeline first.")
    next
  }
  load(environ_path, envir = .GlobalEnv)

  extension <- "svg"
  subfolder_dataset  <- "index_files/charts"
  subfolder_clusters <- "index_files/charts"

  ## ── Overlays (WOS data only) ───────────────────────────────────────────
  #if (settings$params$dataset_source == "wos" && "WC" %in% colnames(myDataCorrect)) {
  #  source(file.path(getwd(), "pipelines", "charts", "overlays.R"))
  #}

  # ── SVG Charts ─────────────────────────────────────────────────────────
  print("###################### SVG CHARTS")
  chart_scripts <- c(
    #"dataset_bars.R",
    #"dataset_trends.R",
    "cluster_stats.R",
    "cluster_scatterplots.R"
  )
  for (script in chart_scripts) {
    source(file.path(getwd(), "pipelines", "charts", script))
  }

  # ── Network chart (level 0 only) ──────────────────────────────────────
  # if (level_report == 0) {
  #   source(file.path(getwd(), "pipelines", "charts", "network_chart.R"))
  # }

  # ── UMAP scatter plot (Python — requires embeddings) ───────────────────
  embeds_folder <- settings$topic_model$embeds_folder
  if (is.null(embeds_folder)) embeds_folder <- "e01"

  embeddings_dir <- file.path(
    settings$metadata$bibliometrics_folder,
    settings$metadata$project_folder,
    settings$metadata$filtered_folder,
    embeds_folder
  )
  emb_file <- file.path(embeddings_dir, "embeddings.npy")

  if (file.exists(emb_file)) {
    print("###################### UMAP SCATTER (Python)")

    # Write document -> cluster mapping for this level
    doc_clusters_path <- file.path(output_folder_level, "doc_clusters.csv")
    readr::write_csv(
      data.frame(
        UT = as.character(myDataCorrect$UT),
        X_C = as.character(myDataCorrect$X_C)
      ),
      doc_clusters_path
    )

    rcs_path     <- file.path(output_folder_level, "rcs_merged.csv")
    palette_path <- file.path(getwd(), "assets", "fukan_colors.json")
    output_png   <- file.path(output_folder_level, subfolder_clusters,
                  "fig_umap_scatter.png")

    py_exec <- file.path("pipelines", "ai", ".venv", "bin", "python3")
    if (!file.exists(py_exec)) py_exec <- Sys.which("python3")
    if (!nzchar(py_exec)) py_exec <- Sys.which("python")

    if (nzchar(py_exec)) {
      seed_val <- if (!is.null(settings$params$seed)) settings$params$seed else 100
      umap_status <- system2(
        py_exec,
        args = c(
          "pipelines/charts/umap_scatter.py",
          "--embeddings-dir", shQuote(embeddings_dir),
          "--doc-clusters",   shQuote(doc_clusters_path),
          "--rcs",            shQuote(rcs_path),
          "--palette",        shQuote(palette_path),
          "--output",         shQuote(output_png),
          "--seed",           as.character(seed_val),
          "--label-min",      "20",
          "--title",          shQuote(paste("Cluster Map - Level", level_report))
        ),
        stdout = "", stderr = ""
      )
      if (umap_status != 0) {
        warning("UMAP scatter plot exited with status ", umap_status)
      }
    } else {
      message("Python not found - skipping UMAP scatter plot.")
    }
  } else {
    message("Embeddings not found at ", embeddings_dir,
            " - skipping UMAP scatter. Run the dataset pipeline first.")
  }
}

message("=== Charts pipeline completed ===")
