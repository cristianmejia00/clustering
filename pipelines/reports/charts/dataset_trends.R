print("###################### dataset_trends.R")

# Yearly trend line charts: cluster trends over time and categorical column trends.
# Does NOT use cluster names — uses X_C codes only.
#
# Performance note: The original summarize_two_columns() used nested sapply row-scans
# to match multi-value cells (O(n*k)). This version expands multi-value columns once
# via separate_longer_delim() and uses dplyr joins/group_by for O(n) aggregation.

source(file.path(getwd(), "pipelines", "reports", "charts", "chart_utils.R"))
library(dplyr)
library(tidyr)
library(scales)

# ---------------------------------------------------------------------------
# Inputs
# ---------------------------------------------------------------------------
dataset          <- dataset
categorical_cols <- settings$rp$categorical_long_reports
column_labels    <- settings$rp$column_labels

chart_palette <- load_chart_palette()
colorblind_pal <- chart_palette$colorblind

# ---------------------------------------------------------------------------
# Core functions
# ---------------------------------------------------------------------------

#' Summarize the cross-tabulation of two columns, where either may be multi-value
#' (semicolon-separated). Returns a long data frame with counts and proportions.
#'
#' @param df Data frame
#' @param a_column Name of the category column (rows)
#' @param b_column Name of the sub-category column (columns)
#' @param a_top Max categories to keep (0 = all)
#' @param b_top Max sub-categories to keep (0 = all)
#' @param a_include Character vector of categories to include (empty = all)
#' @param a_exclude Character vector of categories to exclude
#' @param b_include Character vector of sub-categories to include (empty = all)
#' @param b_exclude Character vector of sub-categories to exclude
#' @param a_decreasing Sort categories by total count descending
#' @param b_decreasing Sort sub-categories by total count descending
#' @return Data frame: category, sub_category, value, value_percent_category, value_percent_sub
summarize_two_columns <- function(df,
                                  a_column,
                                  b_column,
                                  a_top = 20,
                                  b_top = 20,
                                  a_include = character(0),
                                  a_exclude = character(0),
                                  b_include = character(0),
                                  b_exclude = character(0),
                                  a_decreasing = TRUE,
                                  b_decreasing = TRUE) {
  # Expand multi-value columns into long form
  long <- df %>%
    select(all_of(c(a_column, b_column))) %>%
    mutate(across(everything(), as.character)) %>%
    separate_longer_delim(all_of(a_column), delim = "; ") %>%
    separate_longer_delim(all_of(b_column), delim = "; ") %>%
    filter(!is.na(.data[[a_column]]), !is.na(.data[[b_column]]))

  # Apply include/exclude filters
  if (length(a_include) > 0) long <- filter(long, .data[[a_column]] %in% a_include)
  if (length(a_exclude) > 0) long <- filter(long, !(.data[[a_column]] %in% as.character(a_exclude)))
  if (length(b_include) > 0) long <- filter(long, .data[[b_column]] %in% as.character(b_include))
  if (length(b_exclude) > 0) long <- filter(long, !(.data[[b_column]] %in% as.character(b_exclude)))

  # Compute top-N for each axis (for filtering)
  a_totals <- long %>% count(.data[[a_column]], name = "n") %>% arrange(desc(n))
  b_totals <- long %>% count(.data[[b_column]], name = "n") %>% arrange(desc(n))

  if (a_top > 0) {
    top_a <- a_totals %>% slice_head(n = a_top) %>% pull(1)
    long <- filter(long, .data[[a_column]] %in% top_a)
  }
  if (b_top > 0) {
    top_b <- b_totals %>% slice_head(n = b_top) %>% pull(1)
    long <- filter(long, .data[[b_column]] %in% top_b)
  }

  # Cross-tabulate
  cross <- long %>%
    count(.data[[a_column]], .data[[b_column]], name = "value") %>%
    rename(category = 1, sub_category = 2)

  # Compute proportions
  cat_totals <- cross %>% group_by(category) %>% summarise(cat_total = sum(value))
  sub_totals <- cross %>% group_by(sub_category) %>% summarise(sub_total = sum(value))

  result <- cross %>%
    left_join(cat_totals, by = "category") %>%
    left_join(sub_totals, by = "sub_category") %>%
    mutate(
      value_percent_category = round(value / cat_total, 4),
      value_percent_sub      = round(value / sub_total, 4)
    ) %>%
    select(category, sub_category, value, value_percent_category, value_percent_sub)

  # Remove NAs from results
  result <- filter(result, !is.na(category), !is.na(sub_category))

  result
}

#' Assign colors to categories, optionally using a named or unnamed palette
create_palette <- function(summary_df, legend_palette) {
  needed <- length(levels(factor(summary_df$category)))
  chart_pal <- hue_pal()(needed)
  names(chart_pal) <- levels(factor(summary_df$category))

  if (length(legend_palette) == 0) return(chart_pal)

  if (!all(is.na(names(legend_palette)))) {
    matched <- legend_palette[names(legend_palette) %in% names(chart_pal)]
    if (length(matched) > 0) chart_pal[names(matched)] <- matched
  } else {
    n <- min(length(legend_palette), length(chart_pal))
    chart_pal[seq_len(n)] <- legend_palette[seq_len(n)]
  }
  chart_pal
}

#' Line plot of normalized values (% of sub-category) over time
plot_percent_line <- function(summary_df,
                              a_column_label,
                              b_column_label,
                              zero_to_one = TRUE,
                              a_decreasing = TRUE,
                              b_decreasing = TRUE,
                              legend_palette = list()) {
  if (b_decreasing) {
    sub_order <- summary_df %>%
      group_by(sub_category) %>% summarise(s = sum(value, na.rm = TRUE)) %>%
      arrange(desc(s)) %>% pull(sub_category)
    summary_df$sub_category <- factor(summary_df$sub_category, levels = sub_order)
  }
  if (a_decreasing) {
    cat_order <- summary_df %>%
      group_by(category) %>% summarise(s = sum(value, na.rm = TRUE)) %>%
      arrange(desc(s)) %>% pull(category)
    summary_df$category <- factor(summary_df$category, levels = cat_order)
  }

  n_cat <- length(unique(summary_df$category))
  chart_pal <- create_palette(summary_df, legend_palette)

  p <- ggplot(summary_df, aes(x = sub_category, y = value_percent_sub,
                               group = category, color = category, shape = category)) +
    geom_line(linewidth = 0.7) +
    geom_point() +
    scale_shape_manual(values = seq(0, n_cat)) +
    scale_color_manual(values = chart_pal) +
    labs(x = b_column_label, y = "% of documents",
         color = a_column_label, shape = a_column_label,
         title = glue("Percentage of {a_column_label} by {b_column_label}")) +
    theme_chart()

  if (zero_to_one) p <- p + scale_y_continuous(limits = c(0, 1))
  p
}

# ---------------------------------------------------------------------------
# Cluster yearly trends
# ---------------------------------------------------------------------------
dataset$DE <- tolower(dataset$DE)
PY_max <- min(max(dataset$PY, na.rm = TRUE), settings$rp$most_recent_year)
PY_min <- PY_max - 10 + 1

cluster_year <- summarize_two_columns(
  df = dataset,
  a_column = "X_C",
  a_exclude = c(99),
  b_column = "PY",
  b_include = as.character(PY_min:PY_max),
  b_decreasing = FALSE,
  b_top = 0
)

plot_percent_line(cluster_year,
                  a_column_label = "Cluster",
                  b_column_label = "Year",
                  b_decreasing = FALSE,
                  zero_to_one = FALSE,
                  legend_palette = colorblind_pal)
ggsave(file.path(output_folder_level, subfolder_dataset,
                 glue("fig_yearly_trends_clusters.{extension}")),
       dpi = 300)

# ---------------------------------------------------------------------------
# Categorical column yearly trends
# ---------------------------------------------------------------------------
available_charts <- intersect(categorical_cols, colnames(dataset))

for (col in available_charts) {
  trend <- summarize_two_columns(
    df = dataset,
    a_column = col,
    b_column = "PY",
    b_include = as.character(PY_min:PY_max),
    b_decreasing = FALSE,
    b_top = 0
  )
  plot_percent_line(trend,
                    a_column_label = column_labels[col],
                    b_column_label = "Year",
                    b_decreasing = FALSE,
                    zero_to_one = FALSE,
                    legend_palette = colorblind_pal)
  ggsave(file.path(output_folder_level, subfolder_dataset,
                   glue("fig_yearly_trends_{col}.{extension}")))
}
