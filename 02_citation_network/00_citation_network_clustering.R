# Citation Network Clustering

#####################################################
# open network file.
if (cno$using_mission_pairs_from_fukan) {
  # The network objects is based on "mission.pairs.tsv" from Fukan's NEWMAN results
  g1 <- graph_from_data_frame(network, directed = FALSE)
} else {
  g1 <- read.graph(network, format = "ncol")
}

#####################################################
# If we computed our network
if (!cno$using_mission_pairs_from_fukan) {
  ggt <- decompose(g1, min.vertices = 100)
  ttt <-  sapply(ggt, vcount) %>% which.max()
  g1 <- ggt[[ttt]]
  
  valid_vertices <- V(g1) %>% names %>% as.numeric
  
  orphans <- dataset[which(!dataset$X_N %in% valid_vertices),]
  dataset <- dataset[which(dataset$X_N %in% valid_vertices),]
  
  # Order dataset to the order of nodes in the network
  dataset <- dataset[match(valid_vertices, dataset$X_N),]
  
  # add missing columns from Fukan
  dataset$X_D <- degree(g1, mode = "in")
  dataset$X_E <- degree(g1, mode = "all")
  dataset$X_C <- 9999
  
  #Remove objects
  rm(ggt, ttt, valid_vertices)
}

#####################################################
# Verify some properties of the graph
network_description <- list(
  is_directed = is.directed(g1), #Confirms that the network is directed
  is_weighted = is.weighted(g1), #Confirms that the network is weighted
  edges = ecount(g1), #Return the number of edges
  nodes = vcount(g1) #Return the number of vertices
)


# Choose initial clustering solution.
# We either create the initial column _C or use that from Fukan.
if (cno$algor == "louvain" | cno$algor == "infomap") {
  if (!cno$using_initial_column_C_from_fukan) {source("02_citation_network/02_louvain_Infomap.R")}}


# # Select and unify parameters
# # Get the clusters collecting 90% of papers or the top 10, whatever is the smallest number.
# table(dataset$X_C) %>% sort(decreasing = TRUE) %>% prop.table %>% cumsum %>% plot
# table(dataset$X_C) %>% sort(decreasing = TRUE) %>% prop.table %>% cumsum
# table(dataset$X_C) %>% sort(decreasing = TRUE) %>%  plot()
# threshold <- 0.95 #largest clusters contain 90% of articles 
if (cno$threshold > 1) {
  tmp_prop <- table(dataset$X_C) %>% sort(decreasing = TRUE) %>% prop.table %>% cumsum
  tmp_prop <- tmp_prop[cno$threshold]
  cno$threshold <- cno$threshold + cno$threshold * 0.1
}


####################################################
# Execute
# Computes clusters and subclusters
####################################################
# Recursive clustering
source("02_citation_network/03_recursive_clustering_WOS.R")


