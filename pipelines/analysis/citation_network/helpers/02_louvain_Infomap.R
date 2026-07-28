# Depends on `00_citation_network_clustering.R`
# Objects needed:
# - g1
# - dataset
# - network_description

#####################################################################################
print("Executing: pipelines/dataset/utils/02_citation_network/02_louvain_infomap.R")

if (settings$cno$clustering$algorithm == "louvain") {
  com <- cluster_louvain(as.undirected(g1))
}
if (settings$cno$clustering$algorithm == "infomap") {
  com <- cluster_infomap(as.undirected(g1))
}

m_com <- membership(com)
network_description$modularity <- modularity(g1, m_com)

id_com <- sort(unique(m_com))
ordered <- as.numeric(names(sort(table(m_com), decreasing = TRUE)))
repl <- sapply(m_com, function(x) {
  which(ordered == x)
})
names(repl) <- names(m_com)
m_com <- repl

# Verify the clusters are ordered from the largest
plot(table(m_com))

# plot(table(dataset$"_C")) #Compare with original Newman Solution
table(m_com)

# Order vector of communities as they appear in the dataset
vertex <- as.numeric(names(V(g1)))
cluster_map <- data.frame(
  X_N = vertex,
  X_C = as.numeric(m_com[as.character(vertex)])
)

dataset <- dataset %>%
  filter(!is.na(X_N)) %>%
  left_join(cluster_map, by = "X_N")

missing_clusters <- sum(is.na(dataset$X_C))
if (missing_clusters > 0) {
  warning(sprintf(
    "Dropping %d dataset row(s) that have no matching graph node for clustering.",
    missing_clusters
  ))
  dataset <- dataset %>% filter(!is.na(X_C))
}

# dataset_minimal
dataset_minimal <- dataset %>% select(X_N, UT, uuid, X_C)
