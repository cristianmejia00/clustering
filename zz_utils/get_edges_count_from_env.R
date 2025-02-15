# 2025-02-09

# Get the edges from a solution file.
# We need the dataset_minimal and g1 objects

# Read input files
analysis_folder_path <- file.path(settings$metadata$bibliometrics_folder,
                                  settings$metadata$project_folder,
                                  settings$metadata$analysis_id)

network <- readr::read_csv(file.path(analysis_folder_path, "network_comp.csv"))
g1 <- graph_from_data_frame(network, directed = TRUE)

# Select the list of clusters
cluster_list <- unique(dataset_minimal$subcluster_label1)

# Compute the edges
summary_list <- list()
for (i in cluster_list) {
  sections <- strsplit(i, "-")
  tmp <- dataset_minimal %>% 
    filter(subcluster_label1 == i) %>%
    pull("X_N") %>% 
    as.character()
  
  this_vertices <- which(V(g1)$name %in% tmp)
  
  tg1 <- subgraph(g1, this_vertices)
  
  this_edges <- ecount(tg1)
  
  this_summary <- data.frame(
    'cluster_code' = i,
    'main_cluster' = sections[[1]][1] %>% as.numeric(),
    'subcluster' = sections[[1]][2] %>% as.numeric(),
    'egdes' = this_edges
  )
  
  summary_list <- append(summary_list, list(this_summary))
}

summary_df <- rbindlist(summary_list) %>%
  arrange(main_cluster, subcluster)

write.csv(summary_df, 'level1_edges.csv', row.names = FALSE)





