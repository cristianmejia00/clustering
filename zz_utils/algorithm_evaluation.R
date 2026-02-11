library(readr)
network <- read_csv("~/Library/CloudStorage/GoogleDrive-cristianmejia00@gmail.com/My Drive/Bibliometrics_Drive/Q10_brain_health_ts_20250501/a01_cn__f01_dc__c01_lv/network_comp.csv")

# Create graph
g1 <- graph_from_data_frame(network, directed = TRUE)

# Louvain
c_lou2 <- cluster_louvain(as_undirected(g1), resolution = 2)
c_lou1.5 <- cluster_louvain(as_undirected(g1), resolution = 1.5)
c_lou1 <- cluster_louvain(as_undirected(g1), resolution = 1)
c_lou0.5 <- cluster_louvain(as_undirected(g1), resolution = 0.5)
c_lou0.1 <- cluster_louvain(as_undirected(g1), resolution = 0.1)

max(unique(c_lou2$membership))
modularity(c_lou2)

max(unique(c_lou1.5$membership))
modularity(c_lou1.5)

max(unique(c_lou1$membership))
modularity(c_lou1)

max(unique(c_lou0.5$membership))
modularity(c_lou0.5)

max(unique(c_lou0.1$membership))
modularity(c_lou0.1)


# Walktrap
c_walk3 <- cluster_walktrap(g1, steps = 3)
max(unique(c_walk3$membership))
modularity(c_walk3)

c_walk4 <- cluster_walktrap(g1, steps = 4)
max(unique(c_walk4$membership))
modularity(c_walk4)

c_walk5 <- cluster_walktrap(g1, steps = 5)
max(unique(c_walk5$membership))
modularity(c_walk5)

# infomap
c_info <- cluster_infomap(as_undirected(g1))
max(unique(c_info$membership))
modularity(c_info)

# Fast Greedy
c_fg <- cluster_fast_greedy(as_undirected(g1))
max(unique(c_fg$membership))
modularity(c_fg)

