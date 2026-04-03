library(ggplot2)
library(dplyr)
library(stringr) # For string manipulation
library(ggrepel) # For non-overlapping labels

# ---------------------------------------------------------
# 1. Create Dummy Data (To make this script runnable)
# ---------------------------------------------------------
# In your actual workflow, you would load your data here:
# df <- read.csv("your_data.csv")

df <- rcs_merged
df$cluster_name <- gsub("\\*\\*", "", df$cluster_name)
# ---------------------------------------------------------
# 2. Data Preparation for Bar Chart (Top 20)
# ---------------------------------------------------------

plot_data <- df %>%
    filter(cluster_code != "99999") %>%
    # 1. Create the label by combining code and name
    mutate(full_label = paste(cluster_code, cluster_name, sep = ": ")) %>%
    # 2. Select top 20 by number of documents
    slice_max(documents, n = 20) %>%
    # 3. Reorder the factor levels of 'full_label' based on 'documents'.
    mutate(full_label = reorder(full_label, documents))

# ---------------------------------------------------------
# 3. Generate the Bar Plot
# ---------------------------------------------------------

# Saving the bar plot to a variable
bar_plot <- ggplot(plot_data, aes(x = full_label, y = documents)) +
    geom_col(fill = "steelblue") +
    coord_flip() +
    labs(
        title = "Top 20 Clusters by Document Count",
        x = "Cluster",
        y = "Number of Documents"
    ) +
    theme_minimal() +
    theme(
        axis.text.y = element_text(hjust = 1, color = "black", size = 10),
        plot.margin = margin(t = 10, r = 10, b = 10, l = 10, unit = "pt")
    )

bar_plot
# ---------------------------------------------------------
# 4. Generate the Scatterplot
# ---------------------------------------------------------

scatter_plot <- ggplot(df, aes(x = PY_Mean, y = Z9_Mean)) +
    # Points sized by number of documents
    geom_point(aes(size = documents), alpha = 0.6, color = "darkred") +

    # Add labels using ggrepel to avoid overlapping
    geom_text_repel(aes(label = cluster_name), size = 4, max.overlaps = 15) +

    # Scale the size of the bubbles
    scale_size_continuous(range = c(2, 10)) +

    # Labels and Titles
    labs(
        title = "Cluster Landscape: Impact vs. Recency",
        x = "Average Publication Year (PY_Mean)",
        y = "Average Citations (Z9_Mean)",
        size = "Cluster Size"
    ) +
    theme_minimal()

scatter_plot
# Optional: Print plots if running interactively
# print(bar_plot)
# print(scatter_plot)
