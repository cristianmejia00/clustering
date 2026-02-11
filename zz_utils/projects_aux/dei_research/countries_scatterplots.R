# Load necessary libraries
library(ggplot2)
library(ggrepel)
library(patchwork) 

dfs <- "Q333_dei_space"#Note: in the same space folder we save the management data (it is overridden with the same name after running colab. i.e., there's no separate file)
dfs <- "Q335_dei_ai"
dfs <- "Q334_dei_quantum"
dfs <- "R18_sustainability_ti_di_20250707"
dfs <- "R19_wellbeing_ti_di_20250707"

# Read file
# This constructs the path using the correct separator for your specific OS
file_path <- file.path("~", "Library", "CloudStorage", "GoogleDrive-cristianmejia00@gmail.com", "My Drive", 
                       "Bibliometrics_Drive", "Q333_dei_paper", 
                       dfs, "a01_cn__f01_dc__c01_lv", "louvain", 
                       "0.9", "level0", "country_summary_dff_NEW.csv")

country_summary_dff <- read.csv(file_path)

# 1. Setup variables and labels
x_vars <- c(
  "mean_countries_shannon_entropy_norm",
  "mean_ethnicities_shannon_entropy_norm",
  "mean_score"
)

x_labels <- c(
  "Mean Geographic Shannon Entropy (Normalized)",
  "Mean Ethnicities Shannon Entropy (Normalized)",
  "Mean Combined Diversity Score (Normalized)"
)

titles <- c(
  "Citations vs. Geographic Diversity",
  "Citations vs. Ethnicities Diversity",
  "Citations vs. Combined Diversity Score"
)

# 2. Create a list to store the three plots
plot_list <- list()

for (i in seq_along(x_vars)) {
  
  # Calculate R value for the specific pair (to match Python label)
  r_value <- cor(country_summary_dff[[x_vars[i]]], 
                 country_summary_dff$mean_Z9_log_normalized, 
                 use = "complete.obs")
  
  # Create the plot
  p <- ggplot(country_summary_dff, aes(x = .data[[x_vars[i]]], y = mean_Z9_log_normalized)) +
    
    # Scatter points (alpha=0.7, s=50 roughly translates to size=3 in ggplot)
    geom_point(alpha = 0.7, size = 3) +
    
    # Regression line (red, dashed)
    # se=FALSE disables the confidence interval shading to match Python code
    geom_smooth(method = "lm", color = "red", linetype = "dashed", se = FALSE) +
    
    # Point labels using ggrepel
    geom_text_repel(
      aes(label = unique_countries),
      size = 4,              # Adjust text size
      alpha = 0.8,
      min.segment.length = 0, # Always draw lines if displaced
      segment.color = "gray",
      segment.size = 0.5,
      max.overlaps = Inf      # Force all labels to show
    ) +
    
    # Titles and Labels
    labs(
      title = titles[i],
      # Add the R-value as a subtitle or caption to mimic the legend label
      subtitle = paste0("Regression Line (R = ", round(r_value, 3), ")"),
      x = x_labels[i],
      y = if (i == 1) "Mean Citations (Log Normalized)" else NULL # Only show Y label on first plot
    ) +
    
    # Theme configuration (Grid lines, etc.)
    theme_minimal() +
    theme(
      panel.grid.major = element_line(linetype = "dashed", color = alpha("black", 0.2)),
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold"),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 1)
    )
  
  plot_list[[i]] <- p
}

# 3. Combine plots into one figure
# share_y equivalent logic is handled here by layout
final_plot <- wrap_plots(plot_list, nrow = 1)

# Display the plot
print(final_plot)
