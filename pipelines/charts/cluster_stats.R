print("###################### cluster_stats.R")

# Cluster-level statistical charts:
#   - Cluster size bar charts (horizontal + vertical)
#   - Boxplots for numerical columns (PY, Z9, sentiment, score, etc.)
#   - Per-cluster top-5 bar charts for categorical columns
#
# Does NOT use cluster names — uses cluster codes/numbers only.

source(file.path(getwd(), "pipelines", "charts", "chart_utils.R"))
library(dplyr)
library(stringr)

# ---------------------------------------------------------------------------
# Inputs
# ---------------------------------------------------------------------------
dataset            <- myDataCorrect
column_labels      <- settings$rp$column_labels
numerical_reports  <- intersect(settings$rp$numerical_reports, colnames(dataset))
categorical_cols   <- settings$rp$categorical_long_reports

chart_palette <- load_chart_palette()
default_pal   <- chart_palette$full

# Derive main_cluster for coloring boxplots
rcs_merged$main_cluster <- extract_main_cluster(rcs_merged$cluster_code)
rcs_merged$color_hex    <- assign_cluster_colors(rcs_merged$main_cluster, default_pal)

# Ensure last main cluster gets grey
default_pal_bp <- default_pal
if (nlevels(rcs_merged$main_cluster) <= length(default_pal_bp)) {
  default_pal_bp[nlevels(rcs_merged$main_cluster)] <- "#d3d3d3"
}

# Output directories
dir.create(file.path(output_folder_level, subfolder_clusters), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(output_folder_level, subfolder_clusters, "by_clusters"), recursive = TRUE, showWarnings = FALSE)


# ===========================================================================
# PART 1: Cluster size bar charts
# ===========================================================================
stats_size <- rcs_merged %>%
  filter(!grepl("-99", cluster_code), !grepl("^99$", clean_cluster_code(as.character(cluster_code)))) %>%
  transmute(
    Cluster = clean_cluster_code(as.character(cluster_code)),
    Documents = documents,
    main_cluster = main_cluster,
    color_hex = color_hex
  ) %>%
  arrange(main_cluster, desc(Documents)) %>%
  mutate(Cluster = factor(Cluster, levels = rev(Cluster)))

bar_palette <- stats_size %>%
  distinct(main_cluster, color_hex) %>%
  arrange(main_cluster)
bar_palette <- setNames(bar_palette$color_hex, as.character(bar_palette$main_cluster))

if (extension != "svg") {
  write.csv(stats_size %>% select(Cluster, Documents), row.names = FALSE,
            file = file.path(output_folder_level, subfolder_clusters, "data_cluster_size.csv"))
}

ggplot(stats_size, aes(x = Documents, y = Cluster, fill = main_cluster)) +
  geom_bar(stat = "identity", alpha = 0.85) +
  scale_fill_manual(values = bar_palette) +
  labs(
    x     = "Number of Documents",
    y     = NULL,
    fill  = "Main Cluster",
    title = "Documents per Cluster"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position    = "right",
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_blank()
  )
ggsave(file.path(output_folder_level, subfolder_clusters, glue("fig_cluster_size_h.{extension}")))

stats_size_v <- stats_size %>%
  mutate(Cluster = factor(as.character(Cluster), levels = rev(levels(Cluster))))

ggplot(stats_size_v, aes(x = Cluster, y = Documents, fill = main_cluster)) +
  geom_bar(stat = "identity", alpha = 0.85) +
  scale_fill_manual(values = bar_palette) +
  labs(
    x     = "Cluster",
    y     = "Number of Documents",
    fill  = "Main Cluster",
    title = "Documents per Cluster"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position  = "right",
    panel.grid.minor = element_blank(),
    axis.text.x      = element_text(angle = 90, vjust = 0.5, hjust = 1)
  )
ggsave(file.path(output_folder_level, subfolder_clusters, glue("fig_cluster_size_v.{extension}")))


# ===========================================================================
# PART 2: Boxplots for numerical columns
# ===========================================================================

#' Boxplot of a numerical column grouped by cluster, colored by main cluster
plot_boxplots <- function(df, value_column, category_column,
                          value_label = value_column,
                          category_label = category_column) {
  # Compute sorting order: median first, mean as tiebreaker
  agg <- df %>%
    group_by(cat = .data[[category_column]]) %>%
    summarise(med = median(.data[[value_column]], na.rm = TRUE),
              mn  = mean(.data[[value_column]], na.rm = TRUE),
              .groups = "drop") %>%
    arrange(med, mn)

  long <- df %>%
    select(category = all_of(category_column), values = all_of(value_column)) %>%
    mutate(
      category = factor(clean_cluster_code(as.character(category)),
                        levels = clean_cluster_code(as.character(agg$cat))),
      main_cluster = extract_main_cluster(as.character(category))
    )

  bp <- ggplot(long, aes(x = category, y = values, fill = main_cluster)) +
    geom_boxplot(width = 0.7) +
    xlab(category_label) +
    ylab(value_label) +
    scale_fill_manual(values = default_pal_bp) +
    theme_chart()

  K <- n_distinct(df[[category_column]])
  if (K > 20) {
    bp <- bp + theme(axis.text.x = element_text(size = 6, angle = 90, vjust = 0.5, hjust = 1))
  }
  bp
}

for (col in numerical_reports) {
  plot_boxplots(dataset,
                value_column = col,
                category_column = "cluster_code",
                value_label = column_labels[col],
                category_label = "Clusters")
  ggsave(file.path(output_folder_level, subfolder_clusters,
                   glue("fig_clusters_{col}_boxplot.{extension}")))
}


# ===========================================================================
# PART 3: Per-cluster top-5 bar charts for categorical columns
# ===========================================================================

# Load all report CSVs upfront
charts_datasets <- list()
for (col in categorical_cols) {
  report_path <- file.path(output_folder_level, glue("report_{col}.csv"))
  if (file.exists(report_path)) {
    charts_datasets[[col]] <- read.csv(report_path)
  }
}

# Keywords report (special column positions)
kw_path <- file.path(output_folder_level, "report_keyword.csv")
if (file.exists(kw_path)) {
  kw_df <- read.csv(kw_path)
  kw_df <- kw_df %>%
    filter(type == "probable") %>%
    rename(Term = term, Freq = freq, Cluster = cluster, Type = type, Normalized = normalized)
  charts_datasets[["Keywords"]] <- kw_df
}

#' Bar chart for a single cluster's top items in a categorical column
plot_cluster_data <- function(plot_data, cluster_number,
                              col_position = 2,
                              item_label = "Item",
                              document_label = "Documents") {
  cluster_data <- plot_data %>%
    filter(Cluster == cluster_number) %>%
    filter(!is.na(.data[[names(.)[1]]]),
           .data[[names(.)[1]]] != "",
           .data[[names(.)[1]]] != " ",
           .data[[names(.)[1]]] != "NA") %>%
    mutate(across(1, ~ tolower(.) %>% substr(1, 20)))

  # Handle duplicate truncated labels
  col1 <- names(cluster_data)[1]
  dups <- duplicated(cluster_data[[col1]])
  cluster_data[[col1]][dups] <- paste0(cluster_data[[col1]][dups], " ")

  # Factor levels ordered by value
  lvls <- cluster_data[[col1]][order(cluster_data[[col_position]])]
  lvls <- lvls[!is.na(lvls)]
  labels <- ifelse(nchar(lvls) >= 20, paste0(lvls, "..."), lvls)
  cluster_data[[col1]] <- factor(cluster_data[[col1]], levels = lvls, labels = labels)

  plot_rows <- cluster_data %>% slice_head(n = 5)
  y_max <- max(plot_data[[col_position]], na.rm = TRUE)

  ggplot(plot_rows, aes(x = .data[[col1]], y = .data[[names(plot_rows)[col_position]]])) +
    geom_bar(stat = "identity", width = 0.7, fill = "deepskyblue3") +
    scale_y_continuous(name = document_label, limits = c(0, y_max)) +
    scale_x_discrete(name = item_label) +
    coord_flip() +
    theme_chart()
}

# Generate per-cluster charts
for (col_name in names(charts_datasets)) {
  report_df <- charts_datasets[[col_name]]
  clusters_n <- sort(unique(report_df$Cluster))
  clusters_n <- clusters_n[clusters_n != 0]
  char_size <- floor(log10(max(clusters_n))) + 1

  for (cl in clusters_n) {
    if (tolower(col_name) %in% c("keyword", "keywords")) {
      plot_cluster_data(report_df, cl, col_position = 5,
                        item_label = column_labels[col_name],
                        document_label = "TFIDF")
    } else {
      plot_cluster_data(report_df, cl, item_label = column_labels[col_name])
    }

    out_file <- file.path(output_folder_level, subfolder_clusters, "by_clusters",
                          glue("fig_{str_pad(cl, char_size, 'left', '0')}_{col_name}_.{extension}"))
    ggsave(filename = out_file, width = 1000, height = 1000, units = "px")
  }
}
