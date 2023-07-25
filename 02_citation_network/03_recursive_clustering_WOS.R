# 20170913
# Automatic recursive clustering

# the problem with large networks in fukan system, and Newman algorithm is that comunnities are aggregated in few
# large clusters, followed by a long tail of clusters with no papers.
# Then it is needed to recursively cluster to observe better granularity of the field.

# Fukan system allows to recursively cluster. however it is a pain to navigate throw all that data
# Here, I develop a code to automatically recursively cluster such dataset

# First we take the fukan system solution of the first level
# i.e read the mission.facet.all

# Keep only index, cluster, and ID columns
dataset_minimal <- dataset[, c("X_N", "X_C", "UT")]

###############################
# Function Selector
# Figure out the number of clusters that accumulates the threshold OR
# The number of clusters having more papers than a set minimum
# Takes a comunity vector, and the percentage threshold of papers
# New cluster selector (Changed on 20190611)
cl_selector <- function(a_com, threshold, size_lower_limit, max_cluster) {
  # Get the number of clusters which cum sum is below the threshold
  test <- sort(table(a_com), decreasing = TRUE)
  test2 <- cumsum(test) / length(a_com)
  below_threshold <- length(test2[test2 <= threshold])
  # Get the number of clusters having more papers than size_lower_limit
  larger_than_lower_limit <- length(test[test >= size_lower_limit])
  # Return the minimum of the following values
  return(min(below_threshold, larger_than_lower_limit, max_cluster))
}

# Function replacement
# unify the rest of small clusters into a cluster called "cluster 99"
# Takes a comunity vector, and the number of clusters obtained with cl_selector
# New unifyier  (Changed on 20190611)
unifyer <- function(a_com, cl_threshold) {
  # aggregate small cluster into cluster "99"
  new_clusters <- sapply(a_com, function(x) {
    if (x > cl_threshold) {
      99
    } else {
      (x)
    }
  })
  # But if all are cluster 99, it means we did not make any subclustering
  if (all(new_clusters == 99)) {
    new_clusters <- rep(0, length(a_com))
  }
  # Return the vector
  return(new_clusters)
}


# Make comunities in descending order
clusterize <- function(a_network, algorithm = "louvain") {
  net <- simplify(a_network)
  if (algorithm == "louvain") {
    net <- cluster_louvain(net)
  }
  if (algorithm == "newman") {
    net <- cluster_fast_greedy(net)
  }
  if (algorithm == "infomap") {
    net <- cluster_infomap(net)
  }
  com <- membership(net)
  ordered <- table(com) %>%
    sort(decreasing = TRUE) %>%
    names() %>%
    as.numeric()
  repl <- sapply(com, function(x) {
    which(ordered == x)
  })
  names(repl) <- names(com)
  return(repl)
}

# Compute number of vertex and edges within each community
# Create a summary with information of each cluster
v_and_e <- function(a_network, a_com) {
  n_clusters <- sort(unique(a_com))
  vert <- sapply(n_clusters, function(x) {
    return(sum(a_com == x))
  })
  edg <- sapply(n_clusters, function(x) {
    subg1 <- induced_subgraph(a_network, which(a_com == x))
    return(ecount(subg1))
  })

  df <- data.frame(cbind(vert, edg))
  return(df)
}

########################################################################################
# LEVEL 0 ##############################################################################
# Assign fukan clusters to nodes
cl_threshold <- cl_selector(dataset_minimal$X_C, threshold = settings$cno$threshold, size_lower_limit = settings$cno$size_lower_limit, max_cluster = settings$cno$max_clusters)
dataset_minimal$level0 <- unifyer(dataset_minimal$X_C, cl_threshold)
plot(sort(table(dataset_minimal$X_C)))


if (settings$params$recursive_level > 0) {
  # Order dataset_minimal to the order of nodes in the network
  dataset_minimal <- dataset_minimal[match(as.integer(names(V(g1))), dataset_minimal$X_N), ]

  ########################################################################################
  # LEVEL 0 ##############################################################################
  V(g1)$level0 <- dataset_minimal$level0

  ########################################################################################
  # LEVEL 1 ##############################################################################
  level1 <- rep(0, nrow(dataset))

  # Resolution limits
  edges0 <- ecount(g1)
  edges_level1 <- v_and_e(g1, V(g1)$level0)
  resol_limit1 <- edges_level1$edg - sqrt(2 * edges0)

  ## Note: for the first level is a good idea to subcluster the very first cluster 99.
  # for (i in 1:cl_threshold) {
  for (i in c(1:cl_threshold, 99)) {
    subg <- induced.subgraph(g1, which(V(g1)$level0 == i))
    # if (vcount(subg) > settings$cno$size_limit && resol_limit1[i] > 0) {
    if (vcount(subg) > settings$cno$size_limit) {
      communi <- clusterize(subg, algorithm = settings$cno$algor)
      temp_threshold <- cl_selector(communi, threshold = settings$cno$threshold, size_lower_limit = settings$cno$size_lower_limit, max_cluster = settings$cno$max_clusters)
      new_communi <- unifyer(communi, temp_threshold)
      level1[match(as.integer(names(V(subg))), dataset_minimal$X_N)] <- new_communi
    }
  }

  dataset_minimal$level1 <- level1
  subclusters_label <- paste(V(g1)$level0, level1, sep = "-")
  table(subclusters_label)
  V(g1)$level1 <- dataset_minimal$subcluster_label1 <- subclusters_label

  ##                                                                                    ##
  ########################################################################################
  # LEVEL 2 ##############################################################################
  level2 <- rep(0, nrow(dataset))

  # Resolution limit
  edges_level2 <- v_and_e(g1, V(g1)$level1)
  resol_limit2 <- sapply(1:nrow(edges_level2), function(x) {
    a <- edges_level2$edg[x]
    b <- as.integer(strsplit(row.names(edges_level2[x, ]), split = "-")[[1]][1])
    c <- sqrt(2 * edges_level1$edg[b])
    return(a - c)
  })
  names(resol_limit2) <- row.names(edges_level2)

  # Cluster selection
  selected <- row.names(edges_level2)
  selected <- selected[grep("-0|99", selected, invert = TRUE)] # Do not subcluster the aggregated subclusters (that finish)

  for (i in selected) {
    subg <- induced.subgraph(g1, which(V(g1)$level1 == i))
    if ((vcount(subg) > settings$cno$size_limit) && (resol_limit2[i] > 0)) {
      communi <- clusterize(subg, algorithm = settings$cno$algor)
      temp_threshold <- cl_selector(communi, threshold = settings$cno$threshold, size_lower_limit = settings$cno$size_lower_limit, max_cluster = settings$cno$max_clusters)
      print(temp_threshold)
      new_communi <- unifyer(communi, temp_threshold)
      level2[match(as.integer(names(V(subg))), dataset_minimal$X_N)] <- new_communi
    }
  }

  dataset_minimal$level2 <- level2
  subclusters_label <- paste(V(g1)$level1, level2, sep = "-")
  table(subclusters_label)
  V(g1)$level2 <- dataset_minimal$subcluster_label2 <- subclusters_label


  ###                                                                                  ###
  ########################################################################################
  # LEVEL 3 ##############################################################################
  level3 <- rep(0, nrow(dataset))

  # Resolution limit
  edges_level3 <- v_and_e(g1, V(g1)$level2)
  resol_limit3 <- sapply(1:nrow(edges_level3), function(x) {
    a <- edges_level3$edg[x]
    b <- paste(strsplit(row.names(edges_level3[x, ]), split = "-")[[1]][1:2], collapse = "-")
    c <- sqrt(2 * edges_level2[b, ]$edg)
    return(a - c)
  })
  names(resol_limit3) <- row.names(edges_level3)


  # Cluster selection
  selected <- row.names(edges_level3)
  selected <- selected[grep("-0|-99", selected, invert = TRUE)] # Do not subcluster the aggregated subclusters (that finish)

  for (i in selected) {
    subg <- induced.subgraph(g1, which(V(g1)$level2 == i))
    if (vcount(subg) > settings$cno$size_limit && resol_limit3[i] > 0) {
      communi <- clusterize(subg, algorithm = settings$cno$algor)
      temp_threshold <- cl_selector(communi, threshold = settings$cno$threshold, size_lower_limit = settings$cno$size_lower_limit, max_cluster = cnoo$max_clusters)
      new_communi <- unifyer(communi, temp_threshold)
      level3[match(as.integer(names(V(subg))), dataset_minimal$X_N)] <- new_communi
    }
  }

  dataset_minimal$level3 <- level3
  subclusters_label <- paste(V(g1)$level2, level3, sep = "-")
  table(subclusters_label)
  length(table(subclusters_label))
  V(g1)$level3 <- dataset_minimal$subcluster_label3 <- subclusters_label

  #######################################################################
  #######################################################################
  #######################################################################
  plot(sort(table(subclusters_label)))
  sort(table(subclusters_label))

  dataset_minimal$cl99 <- grepl("99", subclusters_label) # Marks all cluster with 99
  dataset_minimal$cl_99 <- grepl("-99", subclusters_label) # Marks RECURSIVE cluster with 99, while untouching the clusters of the first level

  #######################################################################
  # Add subclusters to the original dataset
  # setnames(dataset_minimal, "_N", "XN")
  # setnames(dataset, "_N", "XN")
  dataset <- merge(dataset_minimal[, c(1, 4:12)], dataset, by = "X_N")
  # setnames(dataset_minimal, "XN", "_N")
  # setnames(dataset, "XN", "_N")

  ####                                                                                ####
  ########################################################################################
  # LEVEL 3 ##############################################################################
  # Resolution limit
  edges_level4 <- v_and_e(g1, V(g1)$level3)

  plot(sort(edges_level4$vert))
  max(edges_level4$vert)
  edges_level4[which.max(edges_level4$vert), ]

  edges_level4[order(edges_level4$vert, decreasing = TRUE), ][1:20, ]

  #######################################################################################
  # Character correction
  # To avoid format change when opening in excel
  dataset$subcluster_label1 <- paste(dataset$subcluster_label1, "---", sep = "")
  dataset$subcluster_label2 <- paste(dataset$subcluster_label2, "--", sep = "")
}
