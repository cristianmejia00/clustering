print("###################### overlays.R")

# Create overlay maps in the style of VosViewer.
# Based on the approach from Leydesdorff: https://www.leydesdorff.net/wc15/index.htm
# Input: overlay_master (from assets/) and WC_per_cluster_counts (report CSV)

library(tools)
library(ggrepel)
library(ggplot2)
library(glue)

load(file.path(getwd(), "assets", "overlay_master.rdata"))
WC_per_cluster_counts <- read.csv(
  file.path(output_folder_level, "report_WC_frequencies.csv"),
  check.names = FALSE
)

# Add totals row (represents "all clusters" — cluster 0 by convention)
WC_per_cluster_counts[nrow(WC_per_cluster_counts) + 1, ] <- as.list(unname(colSums(WC_per_cluster_counts)))

# Normalize labels
colnames(WC_per_cluster_counts) <- toTitleCase(colnames(WC_per_cluster_counts))
overlay_master$label <- toTitleCase(overlay_master$label)

dir.create(file.path(output_folder_level, "overlays"), showWarnings = FALSE)


#' Build the overlay data for a single row of WC_per_cluster_counts
#' @param a_data_frame Single-row data frame of WC frequencies
#' @param cls Cluster field to use: 18 or 5
#' @return Data frame with overlay positions, weights, and labels
get_overlay_data <- function(a_data_frame, cls = 18) {
  valid_wc <- intersect(colnames(a_data_frame), overlay_master$label)
  valid_df <- a_data_frame[, valid_wc, drop = FALSE]

  copy_master <- overlay_master
  idx <- match(colnames(valid_df), overlay_master$label)
  copy_master$weight[idx] <- as.integer(valid_df[1, ])

  # Keep only categories with non-zero weight

  copy_master <- copy_master[copy_master$weight > 0, ]
  copy_master <- copy_master[order(copy_master$weight, decreasing = TRUE), ]

  # Show labels only for top 15
  if (nrow(copy_master) > 15) {
    copy_master$label[16:nrow(copy_master)] <- ""
  }

  cluster_col <- if (cls == 18) "cluster18" else "cluster5"
  drop_col    <- if (cls == 18) "cluster5"  else "cluster18"
  copy_master[[drop_col]] <- NULL
  names(copy_master)[names(copy_master) == cluster_col] <- "cluster"
  copy_master$cluster <- as.character(copy_master$cluster)

  copy_master
}


#' Generate and save an overlay image for a given cluster row index
#' @param row_index Row index in WC_per_cluster_counts
get_overlay_image <- function(row_index) {
  temp_df <- WC_per_cluster_counts[row_index, , drop = FALSE]
  overlay <- get_overlay_data(temp_df, cls = 5)

  p <- ggplot(overlay, aes(x = x, y = y, colour = cluster, size = weight)) +
    geom_point() +
    geom_text_repel(aes(label = label, colour = cluster), max.overlaps = 20) +
    scale_color_brewer(palette = "Set1") +
    xlim(min(overlay_master$x), max(overlay_master$x)) +
    ylim(min(overlay_master$y), max(overlay_master$y)) +
    theme(
      panel.background = element_rect(fill = "#ffffff", color = "#ffffff"),
      plot.background  = element_rect(fill = "#ffffff", color = "#ffffff"),
      axis.title       = element_blank(),
      axis.text        = element_blank(),
      axis.ticks       = element_blank(),
      legend.position  = "none"
    )

  # Last row = all clusters → save as cluster 0
  cluster_id <- if (row_index == nrow(WC_per_cluster_counts)) 0 else row_index
  ggsave(file.path(output_folder_level, "overlays",
                   glue("{cluster_id}.{extension}")),
         plot = p)
}


# Generate one overlay per cluster
for (i in seq_len(nrow(WC_per_cluster_counts))) {
  get_overlay_image(i)
}
