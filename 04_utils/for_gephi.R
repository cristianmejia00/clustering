vertex_attr_names(g1)
edge_attr_names(g1)

###############################
# Contracting a directed network does not work because in and out edges are semmed up!
# Do not use this approach and watch out for future mistakes like this.
# # Add dummy edge weight
# g1d <- as.directed(g1)
# E(g1d)$weight <- 1
# g_l0 <- contract(g1d, V(g1d)$level0, vertex.attr.comb=list(weight="sum", "ignore"))
# g_l0 <- simplify(g_l0, remove.multiple = TRUE, remove.loops = TRUE)
# ?simplify
# vcount(g_l0)
# ecount(g_l0)
# is_directed(g_l0)
# vertex_attr_names(g_l0)
# edge_attr_names(g_l0)
# #plot(g_l0, layout=layout_with_kk)

############################
nodes_cluster_network <- table(V(g1d)$level0) %>% as.data.frame()
colnames(nodes_cluster_network) <- c("id", "size")
write.csv(nodes_cluster_network, "nodes_cluster_network.csv", row.names = FALSE)

############################
#ncol_cluster_network <- as_edgelist(g1) %>% as.data.frame()
# ncol_cluster_network$weight <- unlist(E(g_l0)$weight)

ncol_cluster_network <- ncol_cluster %>% group_by(from, to) %>% summarise(weight=sum(weight))
ncol_cluster_network <- ncol_cluster_network[ncol_cluster_network$from != ncol_cluster_network$to,]
colnames(ncol_cluster_network) <- c("Source", "Target", "Weight")
write.csv(ncol_cluster_network, "cluster_network_all_edges.csv", row.names = FALSE)

############################
ncol_cluster_network_trimmed <- ncol_cluster_network[(ncol_cluster_network$Source %in% c(4,7) | ncol_cluster_network$Target %in% c(4,7)) & ncol_cluster_network$Weight >= 10, ]
write.csv(ncol_cluster_network_trimmed, "cluster_network_trimmed_edges.csv", row.names = FALSE)
