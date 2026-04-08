# pipelines/charts/network_chart.R
#
# Draws the full citation network as an SVG. Edges only (zero-size nodes),
# uniform edge width, each edge colored by the higher cluster number of its
# two incident nodes. Edges are rendered cluster-by-cluster (1 → n) so that
# smaller high-numbered clusters are painted on top.
#
# Layout algorithm is read from config_analysis.yml → citation_network → layout
# Supported: "drl" (default) or "kk" (Kamada-Kawai)
#
# Expects (from environ.rdata loaded by generator.R or standalone):
#   network, myDataCorrect, rcs_merged (with cluster_code, global_name)
#   settings (with $cno for citation_network options)
#
# Outputs:
#   level0/clusters/fig_network.png
#   level0/clusters/network_{layout}_coords.csv  (cached layout coordinates)

print("###################### network_chart.R")

source(file.path(getwd(), "pipelines", "charts", "chart_utils.R"))

# ---------------------------------------------------------------------------
# Configuration
# ---------------------------------------------------------------------------
chart_palette <- load_chart_palette()
pal           <- chart_palette$full           # 18 colors (base + extended)
edge_alpha    <- "66"                         # hex alpha (40% opacity)
edge_width    <- 0.3
fig_width     <- 14                           # inches
fig_height    <- 10
output_dir    <- file.path(output_folder_level, "clusters")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Layout algorithm from config (default: "drl")
layout_algo <- tryCatch(
  tolower(settings$cno$layout),
  error = function(e) "drl"
)
if (is.null(layout_algo) || length(layout_algo) == 0 || !layout_algo %in% c("drl", "kk")) {
  layout_algo <- "kk"
}
message("  Network layout algorithm: ", layout_algo)

# ---------------------------------------------------------------------------
# Build the igraph object from the edge-list loaded in environ.rdata
# ---------------------------------------------------------------------------
g1 <- igraph::graph_from_data_frame(network, directed = FALSE)

# Keep only nodes that exist in the dataset (some edges may reference orphans)
nodes_in_data <- as.character(myDataCorrect$X_N)
g1 <- igraph::induced_subgraph(g1, igraph::V(g1)$name[igraph::V(g1)$name %in% nodes_in_data])

# Build node -> cluster lookup
node_cluster <- data.frame(
  X_N = as.character(myDataCorrect$X_N),
  X_C = as.integer(myDataCorrect$X_C),
  stringsAsFactors = FALSE
)

# ---------------------------------------------------------------------------
# Layout (cached)
# ---------------------------------------------------------------------------
coords_path <- file.path(output_dir, paste0("network_", layout_algo, "_coords.csv"))
needs_compute <- TRUE

if (file.exists(coords_path)) {
  message("  Loading cached ", toupper(layout_algo), " coordinates: ", coords_path)
  coords_df <- readr::read_csv(coords_path, show_col_types = FALSE)
  idx <- match(igraph::V(g1)$name, as.character(coords_df$node))
  if (!any(is.na(idx))) {
    coords_df  <- coords_df[idx, ]
    coords_mat <- as.matrix(coords_df[, c("x", "y")])
    needs_compute <- FALSE
  } else {
    message("  Cached coords do not match current graph -- recomputing")
  }
}

if (needs_compute) {
  message("  Computing ", toupper(layout_algo), " layout (this may take a while)...")
  coords_mat <- if (layout_algo == "kk") {
    igraph::layout_with_kk(g1)
  } else {
    igraph::layout_with_drl(g1)
  }
  coords_df <- data.frame(
    node = igraph::V(g1)$name,
    x    = coords_mat[, 1],
    y    = coords_mat[, 2]
  )
  readr::write_csv(coords_df, coords_path)
  message("  Saved ", toupper(layout_algo), " coordinates: ", coords_path)
}

igraph::V(g1)$x <- coords_mat[, 1]
igraph::V(g1)$y <- coords_mat[, 2]

# ---------------------------------------------------------------------------
# Cluster ordering & color palette
# ---------------------------------------------------------------------------
cluster_ids <- sort(unique(node_cluster$X_C))
n_clusters  <- length(cluster_ids)

# Last cluster always gets grey (replicates the fukan convention)
color_map <- rep_len(pal, n_clusters)
color_map[n_clusters] <- "#d3d3d3"
names(color_map) <- as.character(cluster_ids)

last_cid <- max(cluster_ids)

# ---------------------------------------------------------------------------
# Edge coloring: color of the higher-numbered incident cluster (pmax)
# ---------------------------------------------------------------------------
vtx_cluster <- node_cluster$X_C[match(igraph::V(g1)$name, node_cluster$X_N)]

el <- igraph::ends(g1, igraph::E(g1))
c1 <- vtx_cluster[match(el[, 1], igraph::V(g1)$name)]
c2 <- vtx_cluster[match(el[, 2], igraph::V(g1)$name)]
edge_cluster <- pmax(c1, c2, na.rm = TRUE)

edge_hex <- paste0(color_map[as.character(edge_cluster)], edge_alpha)
# Last cluster edges fully transparent
edge_hex[edge_cluster == last_cid] <- "#00000000"
# NA edges (orphans) transparent
edge_hex[is.na(edge_hex)] <- "#00000000"

igraph::E(g1)$color         <- edge_hex
igraph::E(g1)$edge_cluster  <- edge_cluster

# ---------------------------------------------------------------------------
# Edge-based subgraphs for layered rendering (1 → n)
#   Each subgraph contains ALL edges assigned to that cluster (via pmax),
#   preserving the global layout coordinates.
# ---------------------------------------------------------------------------
g_by_cluster <- lapply(cluster_ids, function(cid) {
  eidx <- which(igraph::E(g1)$edge_cluster == cid)
  if (length(eidx) == 0) return(NULL)
  igraph::subgraph.edges(g1, eidx, delete.vertices = FALSE)
})
names(g_by_cluster) <- as.character(cluster_ids)

# ---------------------------------------------------------------------------
# Cluster labels -- resolve from rcs_merged (prefer global_name)
# ---------------------------------------------------------------------------
label_df <- data.frame(
  cluster = cluster_ids,
  label   = vapply(cluster_ids, function(cid) {
    row <- rcs_merged[rcs_merged$cluster == cid, , drop = FALSE]
    if (nrow(row) == 0) return(as.character(cid))
    row <- row[1, ]
    code <- clean_cluster_code(as.character(
      if ("cluster_code" %in% names(row)) row$cluster_code else cid
    ))
    gn <- if ("global_name" %in% names(row)) as.character(row$global_name) else ""
    gn <- trimws(gn)
    if (!is.na(gn) && nzchar(gn) && tolower(gn) != "nan") {
      return(paste0(code, ". ", gn))
    }
    cn <- if ("cluster_name" %in% names(row)) as.character(row$cluster_name) else ""
    cn <- trimws(cn)
    if (!is.na(cn) && nzchar(cn) && tolower(cn) != "nan") {
      return(paste0(code, ". ", cn))
    }
    code
  }, character(1)),
  stringsAsFactors = FALSE
)

# Compute centroid per cluster for label placement (based on node positions)
centroid_df <- do.call(rbind, lapply(cluster_ids, function(cid) {
  vidx <- which(vtx_cluster == cid)
  data.frame(
    cluster = cid,
    cx      = mean(igraph::V(g1)$x[vidx], na.rm = TRUE),
    cy      = mean(igraph::V(g1)$y[vidx], na.rm = TRUE)
  )
}))

label_df <- merge(label_df, centroid_df, by = "cluster")
# Exclude last cluster (misc) from labels
label_df <- label_df[label_df$cluster != last_cid, , drop = FALSE]
# Truncate long labels
label_df$label <- substr(label_df$label, 1, 40)

# ---------------------------------------------------------------------------
# Plot helper
# ---------------------------------------------------------------------------
xlim <- range(igraph::V(g1)$x)
ylim <- range(igraph::V(g1)$y)

my_plot <- function(net, ...) {
  igraph::plot.igraph(
    net,
    rescale            = FALSE,
    xlim               = xlim,
    ylim               = ylim,
    layout             = cbind(igraph::V(net)$x, igraph::V(net)$y),
    vertex.size        = 0,
    vertex.color       = NA,
    vertex.frame.color = NA,
    vertex.label       = NA,
    edge.width         = edge_width,
    edge.arrow.size    = 0,
    ...
  )
}

# ---------------------------------------------------------------------------
# Render PNG (PNG is used instead of SVG because networks with tens of
# thousands of edges produce SVG files that exhaust memory)
# ---------------------------------------------------------------------------
output_png <- file.path(output_dir, "fig_network.png")
message("  Rendering network chart: ", output_png)

png(output_png, width = fig_width, height = fig_height, units = "in",
    res = 300, bg = "black")
par(mar = c(0, 0, 0, 0))

# Draw clusters 1 → n in ascending order.
# Larger clusters (lower numbers) go first → smaller clusters end up on top.
first_plot <- TRUE
for (cid in cluster_ids) {
  gsub <- g_by_cluster[[as.character(cid)]]
  if (is.null(gsub)) next
  my_plot(gsub, add = !first_plot)
  first_plot <- FALSE
}

# Overlay cluster labels at centroids
text(
  label_df$cx, label_df$cy,
  labels = label_df$label,
  col    = "white",
  cex    = 0.75,
  font   = 2
)

dev.off()
message("  Network chart saved: ", output_png)
