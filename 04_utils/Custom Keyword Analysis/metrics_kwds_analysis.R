# 20210131
# This code creates a summary of the presence of a certain list of keywords in a dataset, and their clusters.

# Input:
# A list of keywords to find. For this paper we used the keywords output from "compute_adjacent_keywords.R"
# where we looked for the top 20 "performance keywords"
# We also need the "myDataCorrect" object from a citation analysis.

# Output
# A .csv file with the count of articles, average year and average citations of the paper containing such keywords.
# A .csv article summarizing the  number of articles where those keywords appear per clusters
metrics_kwds <- c(bibliometrics_kwds, scientometrics_kwds, informetrics_kwds)

metrics_kwds <- names(metrics_kwds)
metrics_kwds_counts <- unname(metrics_kwds)

metrics_kwds_PY <- sapply(metrics_kwds, function(x) {
  df <- myDataCorrect[grepl(x, data_TIAB3),c("PY")]
  return(round(summary(df, na.rm = TRUE),1))
})
metrics_kwds_PY


metrics_kwds_Z9 <- sapply(metrics_kwds, function(x) {
  df <- myDataCorrect[grepl(x, data_TIAB3),c("Z9")]
  return(round(summary(df, na.rm = TRUE),1))
})
metrics_kwds_Z9



metrics_kwds_summary <- data.frame("Keyword" = metrics_kwds,
                                       "articles" = as.numeric(metrics_kwds_counts))

metrics_kwds_summary[,paste("PY", row.names(metrics_kwds_PY), sep = "_")] <- t(metrics_kwds_PY)
metrics_kwds_summary[,paste("Z9", row.names(metrics_kwds_PY), sep = "_")] <- t(metrics_kwds_Z9)

# These will be needed for plotting
metrics_kwds_summary$metric <- sapply(strsplit(as.character(metrics_kwds_summary$Keyword), " "), function(x){x[[1]]})
metrics_kwds_summary$label <- sapply(strsplit(as.character(metrics_kwds_summary$Keyword), " "), function(x){x[[2]]})


write.csv(metrics_kwds_summary, file = "metrics_kwds_summary.csv", row.names = FALSE)


########## Number of metric keywords per cluster
metrics_kwds_clusters <- lapply(metrics_kwds, function(x) {
  df <- myDataCorrect[grepl(x, data_TIAB3),c("X_C")]
  return(as.data.frame.list(table(df)))
}) %>% bind_rows()


colnames(metrics_kwds_clusters) <- as.numeric(gsub("X","",colnames(metrics_kwds_clusters)))
metrics_kwds_clusters <- metrics_kwds_clusters[,order(as.numeric(colnames(metrics_kwds_clusters)))]
metrics_kwds_clusters[is.na(metrics_kwds_clusters)] <- 0
rownames(metrics_kwds_clusters) <- metrics_kwds
metrics_kwds_clusters[[1]]


write.csv(metrics_kwds_clusters, file = "metrics_kwds_clusters_counts.csv")

