# --- Final "Nature-Style" Plot ---

# 1. Prepare Data: Order Cluster by MEDIAN Z9_log
plot_data <- df %>%
  mutate(Cluster = fct_reorder(Cluster, Z9_log, .fun = median))

# 2. Create Plot
p_academic <- ggplot(plot_data, aes(x = Z9_log, y = Cluster)) +
  
  # A. Add raw data points (Jitter)
  # Placed *before* boxplot so they sit in the background.
  # height = 0.3 keeps points constrained near their cluster's center line.
  geom_jitter(
    height = 0.3,  
    width = 0,      # No horizontal jittering, only vertical separation
    alpha = 0.3,    # High transparency to show density
    color = "grey40", 
    size = 1
  ) +
  
  # B. Add Refined Boxplots
  geom_boxplot(
    width = 0.6,         # Slimmer boxes (as requested)
    fill = "white",      # Clean white fill provides contrast against grey points
    color = "#2c3e50",   # Dark slate grey outline instead of harsh black
    lwd = 0.4,           # Thinner line width for a crisp look
    outlier.shape = NA,  # Hide outliers here because they are already visible in the jitter layer
    fatten = 1           # Don't fatten the median line, we will draw a custom one next
  ) +
  
  # C. Add Emphasized Median Line
  # Drawing a separate white line over the boxplot's median for a clean highlight
  stat_summary(
    fun = median,
    geom = "crossbar",
    width = 0.6,
    lwd = 0.4,
    color = "#2c3e50",
    fill = "white"
  ) +
  
  # D. Theme & Typography
  theme_classic(base_size = 14) + # Start with a classic, clean theme with larger base font
  labs(
    title = "Normalized Citation Impact per Cluster",
    subtitle = "Clusters ordered by median log-normalized citations (Z9_log)",
    x = "Log-Normalized Citations (Z9_log, centered at 0)",
    y = NULL # Cluster labels are sufficient
  ) +
  theme(
    # Typography
    plot.title = element_text(face = "bold", color = "black", size = 16),
    plot.subtitle = element_text(color = "grey30", size = 12, margin = margin(b = 15)),
    axis.text.y = element_text(color = "black", face = "bold"),
    axis.text.x = element_text(color = "black"),
    axis.title.x = element_text(margin = margin(t = 10), color = "grey20"),
    
    # Clean Axes and Grids
    axis.line = element_line(color = "grey20", linewidth = 0.3),
    axis.ticks = element_line(color = "grey20", linewidth = 0.3),
    axis.ticks.y = element_blank(),   # Remove ticks on the cluster axis for cleaner look
    panel.grid.major.x = element_line(color = "grey92", linewidth = 0.2), # Faint vertical guides only
    panel.grid.major.y = element_blank(), 
    panel.grid.minor = element_blank(),
    plot.margin = margin(20, 20, 20, 10) # Give the plot breathing room
  )

# Display plot
p_academic
