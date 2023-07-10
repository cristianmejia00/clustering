# 20180529
# Get paths of each leaf of the dendrogram to the root.

# This file needs as input 
library(plyr)
library(dendextend)

# Thransform hclust object to dendrogram object
dendro <- as.dendrogram(clusterWC)

# Find the leaves at each level
subtrees <- partition_leaves(dendro)
leaves <- subtrees[[1]]

# Helper function to find the path from the leaves to the root.
pathRoutes <- function(leaf) {
  which(sapply(subtrees, function(x) leaf %in% x))
}

# Find the paths using the helper function
# Outputs a list o vectors
paths <- lapply(leaves, pathRoutes)

# transform to string
paths_string <- sapply(paths, function(x) {
  paste(x, collapse = "-")
})

# Put in data frame object
paths_df <- data.frame(topic = clusterWC$order, 
                       "large_cluster" = unname(clusterGroups[clusterWC$order]), 
                       paths_string)


# Write file
write.csv(paths_df, file = "dendrogram_paths.csv", row.names = FALSE)
getwd()
