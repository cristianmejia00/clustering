# ============================================================================
# IMPROVED NETWORK VISUALIZATION CODE
# Enhanced version with better visual aesthetics
# ============================================================================
# 
# This improved version includes:
# - Higher resolution output (1920x1200)
# - Better edge transparency and thickness control
# - Smoother anti-aliased rendering
# - Improved color saturation and contrast
# - Better cluster separation using DrL layout
# - Optimized performance
# - All outputs saved to specified folder
#
# REQUIREMENTS:
# - igraph package
# - dplyr package  
# - data.table package
# - plyr package
#
# INPUTS:
# - network: edge list data frame
# - myDataCorrect: data frame with columns X_N (node ID) and X_C/level0 (cluster ID)
#
# ============================================================================

library(igraph)
library(dplyr)
library(data.table)
library(plyr)
library(readr)
# ============================================================================
# STEP 1: Graph Preparation
# ============================================================================

# Load network
network <- read_csv("~/Library/CloudStorage/GoogleDrive-cristianmejia00@gmail.com/My Drive/Bibliometrics_Drive/Q340_alternative_meat/a01_cn__f01_dc__c01_lv/network_comp.csv")

# Create graph from edge list
g1 <- graph_from_data_frame(network, directed = FALSE)

# List of clusters
id_com <- sort(unique(myDataCorrect$X_C))

# List of cluster labels
if (level_report == 1) {
  cluster_labels <- myDataCorrect %>% 
    select(X_C, subcluster_label1) %>%
    dplyr::distinct() %>%
    arrange(X_C) %>%
    pull(subcluster_label1)
} else {
  cluster_labels <- id_com %>% as.character()
  cluster_labels[length(cluster_labels)] <- "99---"
}

# List of main clusters
if (level_report == 1) {
  main_clusters <- sapply(strsplit(cluster_labels, "-"), function(x) {x[[1]]})
  main_clusters <- gsub("99", as.character(length(unique(main_clusters))), main_clusters)
  main_clusters <- as.numeric(main_clusters)
} else {
  main_clusters <- id_com
}

# Create correspondence table for nodes and clusters
node_cluster <- myDataCorrect[,c("X_N", "X_C")]

# Sort from last to first cluster
node_cluster <- node_cluster[order(node_cluster$X_C, decreasing = TRUE),]

# Filter and reorder graph
nodes_database <- node_cluster$X_N 
nodes_g1 <- names(V(g1)) %>% as.numeric()

g1 <- induced_subgraph(g1, which(nodes_g1 %in% nodes_database))
vv <- V(g1) %>% names %>% as.numeric
idx <- match(vv, node_cluster$X_N)
g1 <- permute(g1, idx)

# Verify correct assignment
cat("Node-cluster assignment correct:", all(as.numeric(names(V(g1))) == node_cluster$X_N), "\n")
stopifnot(all(as.numeric(names(V(g1))) == node_cluster$X_N))

# ============================================================================
# STEP 2: Compute Improved Layout
# ============================================================================

cat("Computing DrL layout for global structure...\n")
coords_igraph_drl <- layout_with_drl(g1)

# Create coordinate data frame
coords_all_df <- data.frame('node' = V(g1)$name %>% as.numeric(),
                            'x'= coords_igraph_drl[,1],
                            'y'= coords_igraph_drl[,2])

coords_all_df <- merge(coords_all_df, 
                       myDataCorrect[,c('X_N', 'X_C')], 
                       by.x = 'node', 
                       by.y = 'X_N', 
                       all.x = TRUE, 
                       all.y = FALSE)

# Compute cluster centroids
coords_all_centers <- coords_all_df %>% 
  dplyr::group_by(X_C) %>% 
  dplyr::summarize(mean_x = mean(x, na.rm=TRUE),
            mean_y = mean(y, na.rm=TRUE))

coords_all_centers$X_C[coords_all_centers$X_C == 99] <- nrow(coords_all_centers)

# Normalize centroids with min-max scaling for better distribution
normalize_minmax <- function(x, new_min = -2.5, new_max = 2.5) {
  (x - min(x)) / (max(x) - min(x)) * (new_max - new_min) + new_min
}

coords_all_centers$mean_x <- normalize_minmax(coords_all_centers$mean_x)
coords_all_centers$mean_y <- normalize_minmax(coords_all_centers$mean_y)

# ============================================================================
# STEP 3: Per-Cluster Layout with Better Separation
# ============================================================================

cat("Computing per-cluster layouts...\n")
coords_special <- lapply(c(id_com), function(i) {
  if (i %% 10 == 0) cat("  Processing cluster", i, "of", length(id_com), "\n")
  
  this_cluster_ids <- myDataCorrect$X_N[myDataCorrect$X_C == i] %>% as.character()
  gtmp <- induced_subgraph(g1, this_cluster_ids)
  
  # Use Fruchterman-Reingold for better intra-cluster layout
  #gtmp_coords <- layout_with_drl(gtmp) %>% as.data.frame()
  gtmp_coords <- layout_with_fr(gtmp, niter = 500) %>% as.data.frame()
  colnames(gtmp_coords) <- c('x','y')
  
  # Treat outliers with IQR method
  treat_outliers <- function(x) {
    q <- quantile(x, c(0.25, 0.75), na.rm = TRUE)
    iqr <- q[2] - q[1]
    lower <- q[1] - 1.5 * iqr
    upper <- q[2] + 1.5 * iqr
    x[x < lower] <- lower
    x[x > upper] <- upper
    return(x)
  }
  
  gtmp_coords$x <- treat_outliers(gtmp_coords$x)
  gtmp_coords$y <- treat_outliers(gtmp_coords$y)
  
  # # Scale and position based on centroid with increased separation
  gtmp_coords_df <- data.frame('node' = V(gtmp)$name,
                                'x' = scale(gtmp_coords[,1]) * 0.8 + 
                                 coords_all_centers$mean_x[coords_all_centers$X_C == i] * 0.5,
                                'y' = scale(gtmp_coords[,2]) * 0.8 + 
                                  coords_all_centers$mean_y[coords_all_centers$X_C == i] * 0.5)
  return(gtmp_coords_df)
}) %>% rbind.fill()

coords_special2 <- coords_special[match(V(g1)$name, coords_special$node),]
coords_special2 <- coords_special2[,c('x','y')]
rownames(coords_special2) <- NULL
coords_special2 <- as.matrix(coords_special2)

# Set coordinates
V(g1)$x <- coords_special2[,1]
V(g1)$y <- coords_special2[,2]

# Sometime we may want to plot the level0 without adjustements.
# In such cases Kamada Kawai algorithm may work better.
# coords_igraph_drl <- layout_with_kk(g1)
# V(g1)$x <- coords_igraph_drl[,1]
# V(g1)$y <- coords_igraph_drl[,2]

xlim <- c(min(V(g1)$x), max(V(g1)$x))
ylim <- c(min(V(g1)$y), max(V(g1)$y))

# ============================================================================
# STEP 4: Enhanced Color Palette
# ============================================================================

# Fukan color palette with enhanced saturation
fukan_colors <- c("#ff0f1a","#2270e7","#f0e810","#ff8103","#5040e1",
                  "#26cc3a","#ff058e","#9cb8c2","#fffdd0","#c41070")
fukan_colors_extended <- c(fukan_colors, 
                           "#5AFB5A", "#BEAED4", "#FDC086", "#99FDFF", 
                           "#C430FF", "#E4DBE0", "#BF5B17", "#888888")
color_palette <- rep_len(fukan_colors_extended, length(id_com))

# ============================================================================
# STEP 5: Create Cluster Subgraphs
# ============================================================================

g_by_cluster <- lapply(id_com, function(i) {
  this_cluster_ids <- myDataCorrect$X_N[myDataCorrect$X_C == i] %>% as.character()
  gtmp <- induced_subgraph(g1, this_cluster_ids)
  # Full opacity for intra-cluster edges
  E(gtmp)$color <- paste(color_palette[main_clusters[i]], "DD", sep = "")
  if (i == length(id_com)) {
    E(gtmp)$color <- "#00000000"  # Last cluster transparent
  }
  return(gtmp)
})

# ============================================================================
# STEP 6: Improved Plotting Function
# ============================================================================

myplot_improved <- function(network, edge_width = 0.4, vertex_size = 0, ...) {
  plot(network, 
       rescale = FALSE,
       xlim = xlim,
       ylim = ylim,
       layout = matrix(c(V(network)$x, V(network)$y), ncol = 2), 
       vertex.size = vertex_size, 
       vertex.color = NA, 
       vertex.frame.color = NA, 
       vertex.label = NA,
       edge.width = edge_width,
       edge.curved = 0.1,  # Slight curve for better aesthetics
       ...)
}

# ============================================================================
# STEP 7: Generate Visualizations
# ============================================================================

cat("\n=======================================================\n")
cat("GENERATING IMPROVED VISUALIZATIONS\n")
cat("=======================================================\n\n")

# 1. Full colored network (high resolution)
cat("Creating full network visualization...\n")
png(file = file.path(output_folder_level, "network_full_colored.png"), 
    width = 1920, height = 1200, res = 150, antialias = "default")
par(bg = "black", mar = c(0,0,0,0))
myplot_improved(g_by_cluster[[length(g_by_cluster)]], edge_width = 0.3)
for (i in rev(id_com[1:(length(id_com) - 1)])) {
  gtmp <- g_by_cluster[[i]]
  myplot_improved(gtmp, add = TRUE, edge_width = 0.35)
}
dev.off()
cat("  Saved: network_full_colored.png\n")

# 2. Full network with better edge visibility
cat("Creating full network with enhanced edges...\n")
png(file = file.path(output_folder_level, "network_full_enhanced.png"), 
    width = 1920, height = 1200, res = 150, antialias = "default")
par(bg = "black", mar = c(0,0,0,0))
myplot_improved(g_by_cluster[[length(g_by_cluster)]], edge_width = 0.3)
for (i in rev(id_com[1:(length(id_com) - 1)])) {
  gtmp <- g_by_cluster[[i]]
  # Thicker edges with higher opacity for better visibility
  E(gtmp)$color <- paste(color_palette[main_clusters[i]], "FF", sep = "")
  myplot_improved(gtmp, add = TRUE, edge_width = 0.5)
}
dev.off()
cat("  Saved: network_full_enhanced.png\n")

# 3. Individual cluster visualizations with gray background
cat("\nCreating individual cluster visualizations...\n")
if (level_report == 0) {
  n_clusters_to_plot <- 1:min(length(id_com), 15)
}
if (level_report == 1) {
  n_clusters_to_plot <- which(!grepl("99---$", cluster_labels))
}

for (cc in n_clusters_to_plot) {
  if (cc %% 5 == 0) cat("  Processing cluster", cc, "of", n_clusters_to_plot, "\n")
  
  filename <- file.path(output_folder_level, paste0("cluster_", cluster_labels[cc], "_highlighted_v2.png"))
  png(file = filename, width = 1920, height = 1200, res = 150, antialias = "default")
  par(bg = "#1a1a1a", mar = c(0,0,0,0))
  
  # Plot base (last cluster - transparent)
  myplot_improved(g_by_cluster[[length(g_by_cluster)]], edge_width = 0.25)
  
  # Plot all other clusters in gray
  for (i in rev(id_com)) {
    if ((i != cc) && (i != length(id_com))) {
      gtmp <- g_by_cluster[[i]]
      myplot_improved(gtmp, add = TRUE, 
                      edge.color = adjustcolor("grey50", alpha.f = 0.25),
                      edge_width = 0.25)
    }
  }
  
  # Plot highlighted cluster with full color
  gtmp <- g_by_cluster[[cc]]
  E(gtmp)$color <- paste(color_palette[main_clusters[cc]], "FF", sep = "")
  myplot_improved(gtmp, add = TRUE, edge_width = 0.6)
  
  dev.off()
}
cat("  Saved", n_clusters_to_plot, "cluster highlight visualizations\n")