library(readr)
df <- read_csv("~/Library/CloudStorage/GoogleDrive-cristianmejia00@gmail.com/My Drive/sts_cluster_all_science/cluster_summary_sel2022_gpt4.1nano.csv")


library(ggplot2)
library(ggrepel)
library(dplyr)

# # --- 2. Create a Sample Data Frame ---
# # This mimics the Python script's data generation for reproducibility.
# set.seed(42) # for reproducibility
# num_points <- 50
# 
# df <- data.frame(
#   short_name = paste0("Entity-", 1:num_points),
#   ave_py = runif(num_points, 2022, 2026),
#   ave_citations = runif(num_points, 5, 50),
#   publications = sample(10:200, num_points, replace = TRUE),
#   meso_cluster = sample(c('Cluster A', 'Cluster B', 'Cluster C', 'Cluster D'), num_points, replace = TRUE)
# )

df['macro_cluster'] <- as.character(df['macro_cluster'])

# --- 3. Create the Scatter Plot with ggplot2 ---
# We build the plot layer by layer.
ggplot(df, aes(x = ave_py, y = ave_citations)) +
  
  # Add the points (scatter plot layer)
  # aes() maps variables to visual properties.
  # 'size' is mapped to the 'publications' column.
  # 'color' is mapped to the 'meso_cluster' column.
  geom_point(aes(size = publications, color = macro_cluster), alpha = 0.7) +
  
  # Add the smart labels using ggrepel
  # This is the equivalent of Python's adjust_text.
  # It automatically positions text labels to avoid overlapping.
  geom_text_repel(
    aes(label = short_name),
    size = 3,                 # Font size for labels
    max.overlaps = Inf,       # Allow more labels to be placed
    box.padding = 0.5,        # Padding around each label
    point.padding = 0.5,      # Padding around each point
    segment.color = 'grey50'  # Color of the lines from label to point
  ) +
  
  # --- 4. Customize the Plot ---
  
  # Set scales for color and size
  # scale_color_viridis_d() provides a nice color palette for discrete variables.
  scale_color_viridis_d(name = "Meso Clusters") +
  # scale_size_continuous() creates a legend for the point sizes.
  scale_size_continuous(name = "Publications", range = c(2, 12)) +
  
  # Set titles and axis labels
  labs(
    title = "Average Citations vs. Average Publication Year (2022-2026)",
    x = "Average Publication Year (ave_py)",
    y = "Average Citations (ave_citations)",
    caption = "Data points sized by number of publications."
  ) +
  
  # Apply a clean theme
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.caption = element_text(face = "italic")
  )

# The plot will be displayed in the R environment's plot viewer.