print("###################### cluster_labeled.R")

# RE-RUNNABLE: Re-execute this script after cluster naming to get labeled charts.
#
# Charts that use cluster names as labels:
#   - Scatter plots (PY×Z9, PY×size, size×Z9, and sentiment variants)
#   - LDA-style bubble charts (if lda.json exists)
#
# When cluster names are empty, labels fall back to cleaned cluster_code.
# When cluster names are set, labels show "code. name" (truncated to 27 chars).
# The label column is resolved ONCE via resolve_cluster_labels() from chart_utils.R.

source(file.path(getwd(), "pipelines", "reports", "charts", "chart_utils.R"))
library(ggrepel)
library(dplyr)

# ---------------------------------------------------------------------------
# Inputs
# ---------------------------------------------------------------------------
unit_of_analysis <- settings$params$unit_of_analysis
column_labels    <- settings$rp$column_labels

chart_palette <- load_chart_palette()
default_pal   <- chart_palette$full

dir.create(file.path(output_folder_level, subfolder_clusters), recursive = TRUE, showWarnings = FALSE)

# ---------------------------------------------------------------------------
# Resolve cluster labels (one place, one time)
# ---------------------------------------------------------------------------
resolved <- resolve_cluster_labels(rcs_merged, dataset, unit_of_analysis)
dataset     <- resolved$dataset
rcs_merged  <- resolved$rcs_merged

use_cluster_names <- any(rcs_merged$cluster_name != "")
print(paste("Cluster names available:", use_cluster_names))

# ---------------------------------------------------------------------------
# Derive main_cluster + colors
# ---------------------------------------------------------------------------
rcs_merged$main_cluster <- extract_main_cluster(rcs_merged$cluster_code)
rcs_merged$color_hex    <- assign_cluster_colors(rcs_merged$main_cluster, default_pal)

# ---------------------------------------------------------------------------
# Prepare working RCS: filter out -99 and 99 subclusters
# ---------------------------------------------------------------------------
rcs_plot <- rcs_merged %>%
  filter(!grepl("-99", cluster_code), !grepl("^99$", as.character(cluster_code)))

# For facet analysis: keep only facets with >= 10 documents
if (!(tolower(unit_of_analysis) %in% c("topic", "topics", "cluster", "clusters"))) {
  rcs_plot <- rcs_plot %>% filter(documents >= 10)
}


# ===========================================================================
# PART 1: Scatter plots
# ===========================================================================

#' Scatter plot with text labels, sized by a third variable
plot_scatter <- function(rcs_data,
                         x_col, y_col, size_col,
                         x_label = x_col, y_label = y_col) {
  p <- ggplot(rcs_data, aes(x = .data[[x_col]], y = .data[[y_col]])) +
    geom_point(aes(color = color_hex, size = .data[[size_col]])) +
    scale_color_identity() +
    xlab(x_label) +
    ylab(y_label) +
    geom_text_repel(aes(label = clean_cluster_code(X_C_name))) +
    theme_chart() +
    theme(legend.position = "none")
  p
}

# Define scatter plot specifications: x, y, size, x_label, y_label, filename
scatter_specs <- list(
  list("PY_Mean",   "Z9_Mean",   "documents", "Ave. Publication Year", "Ave. Citations",
       "fig_scatter_clusters_PY_x_Z9"),
  list("PY_Mean",   "documents", "Z9_Mean",   "Ave. Publication Year", "Documents",
       "fig_scatter_clusters_PY_x_size"),
  list("documents", "Z9_Mean",   "PY_Mean",   "Documents",             "Ave. Citations",
       "fig_scatter_clusters_size_x_Z9")
)

# Sentiment variants (conditional)
if ("sentiment_Mean" %in% colnames(rcs_merged)) {
  scatter_specs <- c(scatter_specs, list(
    list("PY_Mean",   "sentiment_Mean", "documents", column_labels["PY"],        column_labels["sentiment"],
         "fig_scatter_clusters_year_x_sentiment"),
    list("Z9_Mean",   "sentiment_Mean", "documents", column_labels["Z9"],        column_labels["sentiment"],
         "fig_scatter_clusters_Z9_x_sentiment"),
    list("documents", "sentiment_Mean", "documents", "Documents",                column_labels["sentiment"],
         "fig_scatter_clusters_size_x_sentiment")
  ))
}

for (spec in scatter_specs) {
  plot_scatter(rcs_plot, spec[[1]], spec[[2]], spec[[3]], spec[[4]], spec[[5]])
  ggsave(file.path(output_folder_level, subfolder_clusters,
                   glue("{spec[[6]]}.{extension}")))
}


# ===========================================================================
# PART 2: LDA-style bubble chart
# ===========================================================================

#' Bubble chart of cluster positions (from LDAvis coordinates)
plot_lda_bubbles <- function(rcs_data,
                             color_col = "PY_Mean",
                             color_label = "PY_Mean",
                             size_label = "Documents",
                             gradient_limits = range(rcs_data[[color_col]], na.rm = TRUE),
                             gradient_colors = c("red", "grey", "green")) {
  ggplot(rcs_data, aes(x = x, y = y, color = .data[[color_col]], size = documents), stroke = NA) +
    geom_vline(xintercept = mean(range(rcs_data$x)), color = "gray") +
    geom_hline(yintercept = mean(range(rcs_data$y)), color = "gray") +
    geom_point() +
    scale_size(name = size_label, range = c(2, 20)) +
    scale_color_gradientn(name = color_label,
                          colors = gradient_colors,
                          limits = gradient_limits) +
    geom_text_repel(aes(label = clean_cluster_code(X_C_name)),
                    size = 3, color = "black") +
    theme(panel.background = element_blank(),
          axis.title = element_blank(),
          axis.text  = element_blank(),
          axis.ticks = element_blank())
}

lda_json_path <- file.path(output_folder_level, "keyword_explorer", "lda.json")

if (file.exists(lda_json_path)) {
  print("-- Creating Static LDA-like viz")
  coords <- fromJSON(txt = lda_json_path)
  coords <- data.frame(
    cluster = coords$mdsDat$topics,
    x       = coords$mdsDat$x,
    y       = coords$mdsDat$y
  )

  lda_rcs <- rcs_plot %>%
    inner_join(coords, by = "cluster")

  # Remove cluster 99 if present and small dataset
  if (nrow(lda_rcs) <= 99 && 99 %in% lda_rcs$cluster_code) {
    lda_rcs <- filter(lda_rcs, cluster_code != 99)
  }

  # Years gradient
  plot_lda_bubbles(lda_rcs)
  ggsave(file.path(output_folder_level, subfolder_clusters,
                   glue("clusters_lda_years.{extension}")))

  # Sentiment gradient (conditional)
  if ("sentiment_Mean" %in% colnames(lda_rcs)) {
    plot_lda_bubbles(lda_rcs,
                     color_col = "sentiment_Mean",
                     color_label = "Sentiment",
                     gradient_limits = c(-1, 1))
    ggsave(file.path(output_folder_level, subfolder_clusters,
                     glue("clusters_lda_sentiment.{extension}")))
  }
}
