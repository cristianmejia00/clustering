library(ggplot2)
library(dplyr)
library(stringr) # For string manipulation

# ---------------------------------------------------------
# 1. Create Dummy Data (To make this script runnable)
# ---------------------------------------------------------
# In your actual workflow, you would load your data here:
# df <- read.csv("your_data.csv")

set.seed(123)
df <- rcs_merged

# ---------------------------------------------------------
# 2. Data Preparation
# ---------------------------------------------------------

plot_data <- df %>%
    filter(cluster_code != "99999") %>%
    # 1. Create the label by combining code and name
    mutate(full_label = paste(cluster_code, cluster_name, sep = ": ")) %>%
    # 2. Select top 20 by number of documents
    slice_max(documents, n = 20) %>%
    # 3. Reorder the factor levels of 'full_label' based on 'documents'.
    # This is CRITICAL for ggplot sorting.
    # By default, reorder sorts ascending (small to large).
    # In a coord_flip plot, the Y-axis (categories) draws from bottom to top.
    # Therefore, the 'largest' value (highest factor level) appears at the top visually.
    mutate(full_label = reorder(full_label, documents))

# ---------------------------------------------------------
# 3. Generate the Plot
# ---------------------------------------------------------

ggplot(plot_data, aes(x = full_label, y = documents)) +
    geom_col(fill = "steelblue") + # Create bars

    # Flip coordinates to make it horizontal
    coord_flip() +

    # Labels and Titles
    labs(
        title = "Top 20 Clusters by Document Count",
        x = "Cluster",
        y = "Number of Documents"
    ) +

    # Styling
    theme_minimal() +
    theme(
        # Ensure the y-axis text (the long labels) is left-aligned and black
        axis.text.y = element_text(hjust = 1, color = "black", size = 10),

        # Add extra margin on the left (t, r, b, l) to ensure labels fit comfortably
        # unit(cm) or lines can be used.
        plot.margin = margin(t = 10, r = 10, b = 10, l = 10, unit = "pt")
    )
