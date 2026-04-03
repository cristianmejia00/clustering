# Load required libraries
library(dplyr)
library(ggplot2)
library(ggrepel)

rcs_merged2 <- read_csv("~/Library/CloudStorage/GoogleDrive-cristianmejia00@gmail.com/My Drive/Bibliometrics_Drive/Q342_science_communication/a01_tm__f01_e01__hdbs/level0/cluster_summary_short_dc.csv")

# This function was used!
# Create the scatter plot
ggplot(rcs_merged2 %>% filter(cluster_code <= 99), 
       aes(x = PY_Mean, 
           y = Z9_Mean, 
           size = documents, 
           label = cluster_name)) +
  geom_point(color = "#4682B4", alpha = 0.7) +  # Steel Blue color
  scale_size_continuous(range = c(5, 30)) +  # Adjust the size range as needed
  #scale_x_continuous(breaks = c(2017:2022)) +  # Set x-axis markers for each year from 2018 to 2021
  scale_y_continuous(breaks = seq(10,120, by=20)) +  # Set y-axis range from 15 to 130
  geom_text_repel(
    aes(size = NULL),
    box.padding = 0.5,
    point.padding = 0.5,
    force = 4,
    segment.color = "grey50",
    size = 4
  ) +
  labs(
    x = "Average Publication Year",
    y = "Average Citations",
    size = "Number of Documents"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    axis.title = element_text(face = "bold", size = 12),
    legend.title = element_text(face = "bold", size = 10),
    legend.position = "none"
  )

# 
# 
# # Use this snippet instead of `geom_text` above
# # to display labels with ggrepel

