# 20250419

# Gets the X and Y coordinates the clusters and subclusters as if they where a "topic model" chart.
# This is is, the citation network is collapsed by cluster and subclusters.
# Then we get the layout of these networks are computed using a network layout algorithm.
# Finally we save the x and y coordinate files.
# To plot the network we need to transfer the x and y coordinates manually in Excel or R to the respective RCS.
# Then use the plotting code. 

network <- read_csv(glue("~/Library/CloudStorage/GoogleDrive-cristianmejia00@gmail.com/My Drive/Bibliometrics_Drive/{settings$metadata$project_folder}/f01/direct_citation/network.csv"))

#network <- read.csv("file.ncol", sep = " ", header = FALSE, stringsAsFactors = FALSE)
g1 <- graph_from_data_frame(network, directed = FALSE)
vcount(g1)

########################################################################
## Plotting preparation

# List of clusters
id_com <- sort(unique(dataset$level0))

# Change last cluster, 99, to natural number 
id_com[[length(id_com)]] <- length(id_com)

# Create correspondence table for nodes and clusters
node_cluster <- myDataCorrect[,c("X_N", "level0", "subcluster_label1")]
setnames(node_cluster, c("level0", "subcluster_label1"), c("cluster", "subcluster"))

########################################################################
# Sort the graph vertices in same order as node_cluster
# There are differences in the count of nodes. After inspecting them I reached these conclusions:
# As long as the dataset has the lowest number seems to be no problem
# Need to check how I built the ncol_file. As I did not filter the network only for the nodes in the dataset. It has other nodes too.
nodes_database <- node_cluster$X_N
nodes_g1 <- names(V(g1)) %>% as.numeric()

g1 <- induced_subgraph(g1, which(nodes_g1 %in% nodes_database))
vcount(g1)
vv <- V(g1) %>% names %>% as.numeric #Order of nodes in the graph
idx <- match(vv, node_cluster$X_N) #Index to reorder graph
g1 <- permute(g1, idx)

# Verify the assignation is correct. It must be TRUE
all(as.numeric(names(V(g1))) == node_cluster$X_N)

# =================================
# Add cluster and subcluster labels
V(g1)$cluster <- as.character(node_cluster$cluster)
V(g1)$subcluster <- as.character(node_cluster$subcluster)

table(V(g1)$cluster)
table(as.character(node_cluster$cluster))

# =================================
# Function to create a network of clusters using contract()
create_cluster_network <- function(g, cluster_attr = "cluster") {
  # Create a mapping vector where each vertex is assigned its cluster number
  cluster_names <- vertex_attr(g, cluster_attr)
  
  # mapping
  # Get unique cluster names
  unique_clusters <- unique(cluster_names)
  
  # Create a mapping from cluster names to numeric IDs
  cluster_mapping <- match(cluster_names, unique_clusters)
  
  # Contract the graph based on this mapping
  # This merges vertices with the same cluster value
  g_cluster <- contract(g, 
                        mapping = cluster_mapping, 
                        vertex.attr.comb = list(name = "first", "ignore"))
  
  # Set the vertex names to be the cluster names
  V(g_cluster)$name <- unique(cluster_names)
  
  # Remove self-loops (connections within the same cluster)
  g_cluster <- simplify(g_cluster, 
                        remove.multiple = TRUE,
                        edge.attr.comb = list(weight = "sum", "ignore"),
                        remove.loops = TRUE)
  
  return(g_cluster)
}

# Assuming g1 is your network with the "cluster" attribute
cluster_network <- create_cluster_network(g1, cluster_attr = "cluster")
subcluster_network <- create_cluster_network(g1, cluster_attr = "subcluster")

##############################
# Layout cluster
coords <- layout_with_kk(cluster_network)
V(cluster_network)$kk_x <- coords[,1]
V(cluster_network)$kk_y <- coords[,2]

coords <- layout_with_drl(cluster_network)
V(cluster_network)$drl_x <- coords[,1]
V(cluster_network)$drl_y <- coords[,2]

##############################
# Layout subcluster
sub_coords <- layout_with_kk(subcluster_network)
V(subcluster_network)$kk_x <- sub_coords[,1]
V(subcluster_network)$kk_y <- sub_coords[,2]

sub_coords <- layout_with_drl(subcluster_network)
V(subcluster_network)$drl_x <- sub_coords[,1]
V(subcluster_network)$drl_y <- sub_coords[,2]

sub_coords <- layout_with_mds(subcluster_network)
V(subcluster_network)$mds_x <- sub_coords[,1]
V(subcluster_network)$mds_y <- sub_coords[,2]

sub_coords <- layout_with_graphopt(subcluster_network)
V(subcluster_network)$gh_x <- sub_coords[,1]
V(subcluster_network)$gh_y <- sub_coords[,2]

sub_coords <- layout_with_lgl(subcluster_network)
V(subcluster_network)$lgl_x <- sub_coords[,1]
V(subcluster_network)$lgl_y <- sub_coords[,2]

sub_coords <- layout_with_gem(subcluster_network)
V(subcluster_network)$gem_x <- sub_coords[,1]
V(subcluster_network)$gem_y <- sub_coords[,2]

sub_coords <- layout_with_fr(subcluster_network)
V(subcluster_network)$fr_x <- sub_coords[,1]
V(subcluster_network)$fr_y <- sub_coords[,2]

# Save
coords_df <- igraph::as_data_frame(cluster_network, what = "vertices")
sub_coords_df <- igraph::as_data_frame(subcluster_network, what = "vertices")

#write.csv(sub_coords_df, file="subcoords_df.csv", row.names = FALSE)

##############################
# Layout subcluster
rcs_merged$drl_x <- rcs_merged$drl_y <- NULL 
rcs_merged$kk_x <- rcs_merged$kk_y <- NULL 
rcs_merged <- merge(rcs_merged, sub_coords_df, 
      by.x = "cluster_code", by.y = "name",
      all.x = TRUE, all.y = FALSE)
