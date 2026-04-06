print("###################### reports/00_execute_and_reports.R")

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
# Report levels
# ---------------------------------------------------------------------------
time_started <- Sys.time()

available_levels <- if (settings$params$recursive_level > 0 &&
                        settings$params$type_of_analysis == "citation_network") {
  0:settings$params$recursive_level
} else {
  0
}

for (level_report_iteration in available_levels) {
  level_report <<- level_report_iteration
  print(paste("...Starting reports for level", level_report))

  output_folder_level <- file.path(
    output_folder_reports,
    paste0("level", level_report)
  )
  dir.create(output_folder_level, showWarnings = FALSE)

  # Load the right dataset scope for this level
  level_scope_file <- paste0("04_data_scope_level", level_report, ".R")
  source(file.path("pipelines", "dataset", "utils", "02_citation_network", level_scope_file))

  # Report output paths
  rn <- list(
    PROJECTarticlereport = file.path(output_folder_level, "article_report.csv"),
    PROJECTrcs           = file.path(output_folder_level, "rcs.csv"),
    PROJECTrcs2          = file.path(output_folder_level, "rcs2.csv"),
    PROJECTrcsviz        = file.path(output_folder_level, "rcs_viz.html"),
    PROJECTKeywords      = file.path(output_folder_level, "ALL_Cluster_keywords.rdata"),
    PROJECTKeywords_report = file.path(output_folder_level, "report_keyword.csv"),
    PROJECTenviron       = file.path(output_folder_level, "environ.rdata")
  )

  K <- length(unique(myDataCorrect$X_C))

  # ---------------------------------------------------------------------------
  # Reports
  # ---------------------------------------------------------------------------
  print("Article report")
  source(file.path(getwd(), "pipelines", "reports", "helpers", "01_document_report_with_abstract.R"))

  print("Clusters reports")
  source(file.path(getwd(), "pipelines", "reports", "helpers", "04_cluster_reports.R"))

  print("Computing RCS")
  source(file.path(getwd(), "pipelines", "reports", "helpers", "02_rcs.R"))

  # Overlays (WOS data only)
  if (settings$params$dataset_source == "wos" && "WC" %in% colnames(myDataCorrect)) {
    source(file.path(getwd(), "pipelines", "reports", "charts", "overlays.R"))
  }

  # ---------------------------------------------------------------------------
  # SVG Charts
  # ---------------------------------------------------------------------------
  print("###################### SVG CHARTS")
  extension <- "svg"
  subfolder_dataset  <- "index_files/charts"
  subfolder_clusters <- "index_files/charts"

  chart_scripts <- c(
    "dataset_bars.R",
    "dataset_trends.R",
    "cluster_stats.R",
    "cluster_labeled.R"
  )
  for (script in chart_scripts) {
    source(file.path(getwd(), "pipelines", "reports", "charts", script))
  }

  # ---------------------------------------------------------------------------
  # UMAP scatter plot (Python — requires embeddings from dataset pipeline)
  # ---------------------------------------------------------------------------
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
    output_svg   <- file.path(output_folder_level, subfolder_clusters,
                              "fig_umap_scatter.svg")

    py_exec <- file.path("pipelines", "ai", ".venv", "bin", "python3")
    if (!file.exists(py_exec)) py_exec <- Sys.which("python3")
    if (!nzchar(py_exec)) py_exec <- Sys.which("python")

    if (nzchar(py_exec)) {
      seed_val <- if (!is.null(settings$params$seed)) settings$params$seed else 100
      umap_status <- system2(
        py_exec,
        args = c(
          "pipelines/reports/charts/umap_scatter.py",
          "--embeddings-dir", shQuote(embeddings_dir),
          "--doc-clusters",   shQuote(doc_clusters_path),
          "--rcs",            shQuote(rcs_path),
          "--palette",        shQuote(palette_path),
          "--output",         shQuote(output_svg),
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

  # ---------------------------------------------------------------------------
  # Save environment
  # ---------------------------------------------------------------------------
  print("Saving image")
  save.image(rn$PROJECTenviron)

  time_taken <- Sys.time() - time_started
  print(time_taken)
}
