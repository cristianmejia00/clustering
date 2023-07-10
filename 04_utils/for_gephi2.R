# Network influence of selected subclusters

###############################

ncol_names <- as_edgelist(g1, names = TRUE)

nodes_conver <- data.frame("node_name" = V(g1)$name,
                           "node_cluster" = V(g1)$level1)

ncol_cluster <- data.frame("from" = nodes_conver$node_cluster[match(ncol_names[,1],nodes_conver$node_name)],
                           "to" = nodes_conver$node_cluster[match(ncol_names[,2],nodes_conver$node_name)])
ncol_cluster$weight <- 1

############################
nodes_cluster_network <- table(V(g1d)$level1) %>% as.data.frame()
colnames(nodes_cluster_network) <- c("id", "size")
# Add Gephi extra columns
nodes_cluster_network$Label <- nodes_cluster_network$id
nodes_cluster_network$Subcluster <- nodes_cluster_network$id
nodes_cluster_network$Cluster <- gsub("-.*$", "", nodes_cluster_network$id)
write.csv(nodes_cluster_network, "subcluster_network_nodes.csv", row.names = FALSE)

############################
#ncol_cluster_network <- as_edgelist(g1) %>% as.data.frame()
# ncol_cluster_network$weight <- unlist(E(g_l0)$weight)

# Aggregate edges
ncol_cluster_network <- ncol_cluster %>% group_by(from, to) %>% summarise(weight=sum(weight))
# Remove self loops
ncol_cluster_network <- ncol_cluster_network[ncol_cluster_network$from != ncol_cluster_network$to,]
# Gephi colnames
colnames(ncol_cluster_network) <- c("Source", "Target", "Weight")


# Save
write.csv(ncol_cluster_network, "subcluster_network_all_edges.csv", row.names = FALSE)

allowed_clusters <- c("4-1",
  "4-3",
  "4-4",
  "4-5",
  "4-6",
  "4-7",
  "4-9",
  "4-10")
  

############################
ncol_cluster_from_network_trimmed <- ncol_cluster_network[ncol_cluster_network$Source %in% allowed_clusters & ncol_cluster_network$Weight >= 10, ]
write.csv(ncol_cluster_from_network_trimmed, "subcluster_network_from_trimmed_edges.csv", row.names = FALSE)

ncol_cluster_to_network_trimmed <- ncol_cluster_network[ncol_cluster_network$Target %in% allowed_clusters & ncol_cluster_network$Weight >= 10, ]
write.csv(ncol_cluster_to_network_trimmed, "subcluster_network_to_trimmed_edges.csv", row.names = FALSE)



journals <- table(myDataCorrect$SO) %>% sort(decreasing = TRUE) %>% as.data.frame()
journals$rank <- c(1:nrow(journals))
frontiers <- journals[grepl("^frontiers in", tolower(journals$Var1)),]

write.csv(frontiers, "frontiers.csv", row.names = FALSE)
table(myDataCorrect$PY)
