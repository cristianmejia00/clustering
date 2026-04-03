# Plots the subclusters and clusters as a topic model chart.
# We computed the sub/cluster coordinates from the utils/aa_cluster_coordinates.R
# And is needed that the x and y coordinates be passed to the respective RCS. Here "cluster_summary_extended"

# Load necessary library
library(ggplot2)

df <- read_csv("~/Library/CloudStorage/GoogleDrive-cristianmejia00@gmail.com/My Drive/Bibliometrics_Drive/Q327 TI Policy/a01_cn__f01_dc__c01_lv/level1/cluster_summary_extended_dc.csv")

df$main_cluster <- as.character(df$main_cluster)

# Create the scatter plot
ggplot(df,# %>% filter(main_cluster == "4"), 
       aes(x = x, y = y, color = main_cluster, size = documents, label = cluster_name)) +
  geom_point(alpha = 0.7) +
  #geom_text(aes(label = cluster_name), vjust = -0.5, hjust = 0.5, size = 3) +
  scale_size_continuous(range = c(2, 10)) +  # Adjust min and max size as needed
  theme_minimal() +
  labs(title = "Cluster Visualization",
       x = "X Coordinate", 
       y = "Y Coordinate",
       color = "Main Cluster",
       size = "Number of Documents") +
  theme(legend.position = "right")


########################################################################
# Load necessary libraries
library(plotly)

# Create the interactive scatter plot
plot <- plot_ly(
  data = df,
  x = ~x,
  y = ~y,
  color = ~main_cluster,
  size = ~documents,
  sizes = c(10, 300),  # Adjust min and max size as needed
  text = ~cluster_name,
  hoverinfo = "text",
  type = "scatter",
  mode = "markers",
  textposition = "top center"
) %>%
  layout(
    title = "Cluster Visualization",
    xaxis = list(title = "X Coordinate"),
    yaxis = list(title = "Y Coordinate"),
    legend = list(title = list(text = "Main Cluster"))
  )

# Display the plot
plot
