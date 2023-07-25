# Citation Network Clustering

# # Select and unify parameters
# # Get the clusters collecting 90% of papers or the top 10, whatever is the smallest number.
# table(dataset$X_C) %>% sort(decreasing = TRUE) %>% prop.table %>% cumsum %>% plot
# table(dataset$X_C) %>% sort(decreasing = TRUE) %>% prop.table %>% cumsum
# table(dataset$X_C) %>% sort(decreasing = TRUE) %>%  plot()
# threshold <- 0.95 #largest clusters contain 90% of articles

# The following is used when in `settings` we chose an Integer for the exact number of cluster.
# Here, we convert the integer to a value between 0 and 1 representing the PROPORTION of clusters to take
# This is needed because the clustering algorithm is designed to take a value from 0 to 1.
if (settings$cno$threshold > 1) {
  tmp_prop <- table(dataset$X_C) %>%
    sort(decreasing = TRUE) %>%
    prop.table() %>%
    cumsum()
  tmp_prop <- tmp_prop[settings$cno$threshold]
  settings$cno$threshold <- tmp_prop + tmp_prop * 0.0001
}
# settings$cno$threshold <- 27
# tmp_prop <- table(dataset$X_C) %>% sort(decreasing = TRUE) %>% prop.table %>% cumsum
# tmp_prop <- tmp_prop[settings$cno$threshold]
# settings$cno$threshold <- tmp_prop + tmp_prop * 0.0001
# settings$cno$threshold

#####################################################
# If any of these is TRUE, then we need a network object.
if (!settings$cno$using_initial_column_C_from_fukan | settings$params$recursive_level > 0) {
  print("Clustering will be performed using the provided network")
  #####################################################
  # open network file.
  if (settings$cno$using_mission_pairs_from_fukan) {
    # The network objects is based on "mission.pairs.tsv" from Fukan's NEWMAN results
    g1 <- graph_from_data_frame(network, directed = FALSE)
  } else {
    g1 <- read.graph(network, format = "ncol")
  }

  #####################################################
  # If we computed our network
  if (!settings$cno$using_mission_pairs_from_fukan) {
    ggt <- decompose(g1, min.vertices = 100)
    ttt <- sapply(ggt, vcount) %>% which.max()
    g1 <- ggt[[ttt]]

    valid_vertices <- V(g1) %>%
      names() %>%
      as.numeric()

    orphans <- dataset[which(!dataset$X_N %in% valid_vertices), ]
    dataset <- dataset[which(dataset$X_N %in% valid_vertices), ]

    # Order dataset to the order of nodes in the network
    dataset <- dataset[match(valid_vertices, dataset$X_N), ]

    # add missing columns from Fukan
    dataset$X_D <- degree(g1, mode = "in")
    dataset$X_E <- degree(g1, mode = "all")
    dataset$X_C <- 9999

    # Remove objects
    rm(ggt, ttt, valid_vertices)
  }

  #####################################################
  # Verify some properties of the graph
  network_description <- list(
    is_directed = is.directed(g1), # Confirms that the network is directed
    is_weighted = is.weighted(g1), # Confirms that the network is weighted
    edges = ecount(g1), # Return the number of edges
    nodes = vcount(g1) # Return the number of vertices
  )


  # Choose initial clustering solution.
  # We either create the initial column _C or use that from Fukan.
  if (settings$cno$algor == "louvain" | settings$cno$algor == "infomap") {
    if (!settings$cno$using_initial_column_C_from_fukan) {
      source("02_citation_network/02_louvain_Infomap.R")
    }
  }
} else {
  print("No clustering or recursive clustering needed. We use the provided column X_C")
}


####################################################
# Execute
# Computes clusters and subclusters
####################################################
# Recursive clustering
source("02_citation_network/03_recursive_clustering_WOS.R")
