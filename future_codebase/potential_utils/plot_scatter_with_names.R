write.csv(rcs_tmp %>% 
            select(X_C_name, color_hex) %>%
            filter(!grepl("99", X_C_name)), 
          file = "subcluster_colors.csv", row.names = FALSE)

getwd()

library(ggrepel)
plot_clusters(rcs_merged)

(rcs_data,
  point_labels,
  x_column,
  y_column,
  color_hex_column,
  color_labels,
  size_column,
  x_column_label = x_column,
  y_column_label = y_column) 

rcs_tmp$cluster_name <- NULL
rcs_tmp2 <- merge(rcs_tmp, rcs_merged %>% select(cluster_code, cluster_name),
                  by = "cluster_code",
                  all.x = TRUE,
                  all.y = FALSE)
rcs_tmp2$labels = paste(rcs_tmp2$cluster_code, rcs_tmp2$cluster_name, sep = " ")
rcs_tmp2$labels[rcs_tmp2$cluster_code == 25] <- "25 Public Policy Implementation and Service Accessibility"
plot_scatter(rcs_data = rcs_tmp2, 
             point_labels = "labels",
             x_column = "PY_Mean",
             y_column = "Z9_Mean",
             color_labels = "main_cluster",
             color_hex_column = "color_hex",
             size_column = "documents")
