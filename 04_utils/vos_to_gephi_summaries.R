########################################################################
# Tokyo Institute of Technology
# 20230522
# Cristian Mejia -- cristianmejia00@gmail.com
# Code for reporting based of VosViewer clusters
########################################################################

# Use this code after VOSViewer

# Focused on the node file.
# This also add color columns for cluster and countries.
# Which allows using same palette for multiple analyses hence helping to navigate quicker.

# Clusters can be removed and automatically reordered here. IT will update the numbering
# of the clusters. However, the current implementation only allows removing clusters 1 by 1.

# Finally, columns are better ordered for reporting purposes.

# This code finishes the reports, by aggregating the data from the network and the stats we computed earlier.

# The main cases for this code are
# Reports of Co-authors networks 
# Reports of Co-institutions networks

###########################################################
# Requirements:
# The dataset (or environment with the dataset) used to create the network in VosViewer
# The Map and Net files exported from VosViewer after the network was created

########################################################### INPUTS
getwd()

# Select type: e.g. authors or institutions
type_of_clusters <- "countries"

## Externals
# Load the main dataset
load("future_scenario.rdata")

# Read the files from VOSViewer.
nodes <- fread("fs__co_map.csv") %>% as.data.frame()
edges_original <- fread("fs__co_net.csv") %>% as.data.frame()

# Utils
# Generate palettes
load("05_assets/color_vectors.rdata")

###########################################################
names(edges) <- c("Source", "Target", "Weight")
names(nodes)[1:2] <- c("Id", "Label")

# Adjust the edgest file
edges$Label <- NULL
edges$Label.x <- NULL
edges$timeset <- NULL
edges <- merge(edges, nodes[, c("Id", "Label")], by.x = "Source", by.y = "Id", all.x = TRUE)
setnames(edges, "Label", "SourceLabel")
edges <- merge(edges, nodes[, c("Id", "Label")], by.x = "Target", by.y = "Id", all.x = TRUE)
setnames(edges, "Label", "TargetLabel")
edges$pair <- paste(edges$SourceLabel, edges$TargetLabel, sep = "---")

# Remove unnecesary columns
nodes$timeset <- NULL
colnames(nodes)

# Adjust cluster number so cluster 1 is the biggest
# com == the cluster column we want to rearrange
rearrange_clusters <- function(com) {
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
nodes$cluster <- rearrange_clusters(nodes$cluster)
table(nodes$cluster)

# Remove cluster label from small clusters
tt <- table(nodes$cluster)
tt <- tt[tt > 1]
test <- nodes[nodes$cluster > length(tt), ]
nodes$cluster[nodes$cluster > length(tt)] <- NA
table(nodes$cluster)


# Adjust column names
setnames(
  nodes,
  c("weight<Links>", "weight<Total link strength>", "weight<Documents>", "score<Avg. pub. year>", "score<Avg. citations>", "weight<Citations>"),
  c(paste("co-", type_of_clusters, sep=""), "total link strength", "documents", "ave. document year", "ave. citations", "sum citations")
)

# Remove columns we do not use
nodes$`weight<Norm. citations>` <- NULL
nodes$`score<Avg. norm. citations>` <- NULL
nodes <- nodes[with(nodes, order(cluster, -documents)),]


# Generate color palette
cluster_color <- generate_colors(unique(nodes$cluster), main_clusters_color)

# Add colors
nodes$cluster_color <- cluster_color[as.numeric(nodes$cluster)]
nodes$cluster_color[is.na(nodes$cluster_color)] <- "#ffffff"

# Add their most relevant paper
dataset <- dataset[order(dataset$Z9, decreasing = TRUE),]

# Create the search strings
if (type_of_clusters == "authors") {
  search_string <- dataset$AF %>% tolower %>% paste(";", sep = '')
}
if (type_of_clusters %in% c("institutions", "countries")) {
  search_string <- dataset$C1 %>% tolower 
}

# Find the most cited title per author or institution
ttt <- lapply(nodes$Label, function(x) {
  match_idx <- grep(x, search_string)[1]
  author_data <- dataset[match_idx, c("TI","SO", "PY", "Z9", "WC", "ID", "DI")]
  return(author_data)
}) %>% rbindlist()
ttt$DI <- paste('https://doi.org/', ttt$DI, sep = "")


# Append to nodes
nodes <- cbind(nodes, ttt)


####################################################
# Cluster reports
cluster_summary <- lapply(c(1:max(nodes$cluster, na.rm = TRUE)), function(y) {
  cl_data <- nodes[nodes$cluster == y,]
  if (type_of_clusters == "authors") {
    my_regex <- paste(paste(cl_data$Label, ";", sep = ""), collapse = "|")    
  }
  if (type_of_clusters  %in% c("institutions", "countries")) {
    my_regex <- paste(cl_data$Label, collapse = "|") 
  }

  authors <- nrow(cl_data)
  
  cl_data <- cl_data[!duplicated(cl_data$TI),]
  
  if (authors >= 5) {
    top_authors <- paste(cl_data$Label[1:5], collapse = "; ")
  } else {
    top_authors <- paste(cl_data$Label, collapse = "; ")
  }
  
  wc_summary <- cl_data$WC %>% tolower %>% strsplit("; ") %>% unlist %>% table %>% sort(decreasing = TRUE) %>% names
  if (length(wc_summary) >= 1) {
    wc_summary <- paste(wc_summary[1:min(5, length(wc_summary))], collapse = "; ")
  } else {
    wc_summary <- ""
  }
  
  id_summary <- cl_data$ID %>% tolower %>% strsplit("; ") %>% unlist %>% table %>% sort(decreasing = TRUE) %>% names
  if (length(id_summary) >= 1) {
    id_summary <- paste(id_summary[1:min(5, length(id_summary))], collapse = "; ")
  } else {
    id_summary <- ""
  }
  
  cl_data_full <- grep(my_regex, search_string)
  cl_documents <- length(cl_data_full)
  cl_py <- mean(dataset$PY[cl_data_full], na.rm = TRUE) %>% round(1)
  cl_z9 <- mean(dataset$Z9[cl_data_full], na.rm = TRUE) %>% round(1)
  

  
  tmp <- data.frame("Cluster" = y,
                    "Node" = authors,
                    "Documents" = cl_documents,
                    "Ave._Year" = cl_py,
                    "Ave._Citations" = cl_z9,
                    "Top_Authors__by_docs" = top_authors,
                    "Categories" = wc_summary,
                    "Keywords" = id_summary)
  return(tmp)
}) %>% rbindlist()

# Change header names for reports
node_heading <- type_of_clusters %>% str_to_title()
setnames(cluster_summary, "Node", node_heading)

# Change names
setnames(nodes, 
         c("TI","SO", "PY", "Z9", "WC", "ID", "DI"), 
         c("Top Cited Article of this node (TCA)","Journal of TCA", "Year of TCA", "Citations of TCA", "Category of TCA", "Keywords  of TCA", "DOI  of TCA"))

# Write files
write.csv(edges, file = paste(type_of_clusters, "edges.csv", sep = "_"), row.names = FALSE)
write.csv(nodes, file = paste(type_of_clusters, "nodes.csv", sep = "_"), row.names = FALSE)
write.csv(cluster_summary, file = paste(type_of_clusters, "cluster_summary.csv", sep = "_"), row.names = FALSE)
getwd()
