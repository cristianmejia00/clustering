p <- plot_scatter(tmp %>% filter(dataset == "AI"),
                  point_labels = "local_cluster", # "scatter_labels" for the full cluster name. # "local_cluster" for the cluster code only
                  x_column = "PY_Mean",
                  y_column = "Z9_Mean",
                  color_hex_column = "color",
                  color_labels = "dataset",
                  size_column = "documents",
                  min_x = min(tmp$PY_Mean, na.rm = TRUE) %>% floor(),
                  max_x = max(tmp$PY_Mean, na.rm = TRUE) %>% ceiling(),
                  max_y = max(tmp$Z9_Mean, na.rm = TRUE) %>% ceiling(),
                  show_tags = TRUE)
p


# Assuming your data frame is called 'df'
convert_to_text <- function(df) {
  # Open a connection to write to a file
  fileConn <- file("EU_act_clusters.md", "w")
  
  # Loop through each row of the data frame
  for(i in 1:nrow(df)) {
    # Write each column name and its value
    writeLines(paste0("cluster_code: ", df$cluster_code[i]), fileConn)
    writeLines(paste0("cluster_name: ", df$cluster_name[i]), fileConn)
    writeLines(paste0("PY_Mean: ", df$PY_Mean[i]), fileConn)
    writeLines(paste0("Z9_Mean: ", df$Z9_Mean[i]), fileConn)
    writeLines(paste0("description: ", df$description[i]), fileConn)
    
    # Add separator between records (except for the last record)
    if(i < nrow(df)) {
      writeLines("\n\n---\n\n", fileConn)
    }
  }
  
  # Close the connection
  close(fileConn)
}


convert_to_text(rcs_merged_EUact %>%
                  select(
                    cluster_code, cluster_name,
                    documents, PY_Mean, Z9_Mean, description
                  ))
