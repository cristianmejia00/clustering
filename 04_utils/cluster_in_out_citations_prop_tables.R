



ncol_names <- as_edgelist(g1, names = TRUE)
ncol_ids <- as_edgelist(g1, names = FALSE)

vcount(g1)
max(c(network$V1, network$V2))
max(c(ncol_names[,1], ncol_names[,2]))
max(c(ncol_ids[,1], ncol_ids[,2]))    

vertex_attr_names(g1)
V(g1)$level0 %>% unique %>% sort

nodes_conver <- data.frame("node_name" = V(g1)$name,
                           "node_cluster" = V(g1)$level0)

ncol_cluster <- data.frame("from" = nodes_conver$node_cluster[match(ncol_names[,1],nodes_conver$node_name)],
                           "to" = nodes_conver$node_cluster[match(ncol_names[,2],nodes_conver$node_name)])
ncol_cluster$weight <- 1

# Verify "From" to "To"
test <- ncol_cluster[ncol_cluster[,1] == "5" & ncol_cluster[,2] == "3",]

# citation matrix cluster to cluster. Rowwise means from cluster x to cluster y
cluster_citation_matrix <- table(ncol_cluster$from, ncol_cluster$to) %>% as.matrix()

#########################################################################
# outgoing citation prop matrix
cluster_citation_out_prop_matrix <- cluster_citation_matrix
diag(cluster_citation_out_prop_matrix) <- 0

# Verifications.
# This must be TRUE
sum(cluster_citation_out_prop_matrix[2,]) == rowSums(cluster_citation_out_prop_matrix)[2]
# This will appear later
tmp <- cluster_citation_out_prop_matrix[2,] / rowSums(cluster_citation_out_prop_matrix)[2]
sum(tmp)

# Cluster 
cluster_citation_out_prop_matrix <- cluster_citation_out_prop_matrix / rowSums(cluster_citation_out_prop_matrix)

# Verify. This must be TRUE
sum(cluster_citation_out_prop_matrix[1,]) == 1
all(cluster_citation_out_prop_matrix[2,] == tmp)

# Save matrix
write.csv(cluster_citation_out_prop_matrix, file="cluster_outcitations_prop_table.csv", row.names = FALSE)
getwd()

#########################################################################
# ingoing citation prop matrix
cluster_citation_in_prop_matrix <- t(cluster_citation_matrix)
diag(cluster_citation_in_prop_matrix) <- 0

# Verifications.
# This must be TRUE
sum(cluster_citation_in_prop_matrix[2,]) == rowSums(cluster_citation_in_prop_matrix)[2]
# This will appear later
tmp <- cluster_citation_in_prop_matrix[2,] / rowSums(cluster_citation_in_prop_matrix)[2]
sum(tmp)

# Cluster 
cluster_citation_in_prop_matrix <- cluster_citation_in_prop_matrix / rowSums(cluster_citation_in_prop_matrix)

# Verify. This must be TRUE
sum(cluster_citation_in_prop_matrix[1,]) == 1
all(cluster_citation_in_prop_matrix[2,] == tmp)

# Save matrix
write.csv(cluster_citation_in_prop_matrix, file="cluster_incitations_prop_table.csv", row.names = FALSE)
getwd()
