library(tidyverse)
library(patchwork)

# 1. Create Mockup Dataset
set.seed(42)
n_records <- 500
n_clusters <- 15

# Simulate data
df <- tibble(
  ID = 1:n_records,
  Cluster = sample(paste0("C", 1:n_clusters), n_records, replace = TRUE),
  PY = round(2024 - rexp(n_records, rate = 0.2)), 
  Z9 = round(rlnorm(n_records, meanlog = 2, sdlog = 1))
) %>%
  mutate(PY = pmax(PY, 1990)) %>%
  mutate(PY = pmin(PY, 2024))

# --- NEW SECTION: CALCULATE NORMALIZED METRICS ---
df <- df %>%
  group_by(PY) %>%
  mutate(
    # 1. Rank (Percentile Rank)
    Z9_rank = if(n() > 1) percent_rank(Z9) else 0.5,
    
    # 2. Z-score (Standard Score)
    # Check if n > 1 and sd > 0 to avoid division by zero/NA
    Z9_zs = if(n() > 1 && sd(Z9) > 0) as.numeric(scale(Z9)) else 0,
    
    # 3. Log (Log-Transform + Z-score)
    # Temporary log column created for calculation
    temp_log = log(Z9 + 1),
    Z9_log = if(n() > 1 && sd(temp_log) > 0) as.numeric(scale(temp_log)) else 0
  ) %>%
  ungroup() %>%
  select(-temp_log) # Clean up temp column

# Global values for normalization
current_year <- max(df$PY)
global_mean_age <- mean(current_year - df$PY)
global_mean_py <- mean(df$PY)

# frequencies of the years
PY_freq <- table(df$PY) %>% sort(decreasing = TRUE)
df$WeightPY <- PY_freq[as.character(df$PY)]

# 2. Calculate Summaries & Strategies
cluster_summary <- df %>%
  group_by(Cluster) %>%
  summarize(
    Count = n(),
    
    # Original Metric
    Ave_Z9 = mean(Z9),
    
    # --- NEW METRICS AGGREGATION ---
    Ave_Z9_rank = mean(Z9_rank),
    Ave_Z9_zs = mean(Z9_zs),
    Ave_Z9_log = mean(Z9_log),
    # -------------------------------
    
    # Baseline
    Ave_PY = mean(PY),
    
    # Strategy 1: Median Year
    Median_PY = median(PY),
    
    # Strategy 2: Weighted Mean Year (Center of Gravity)
    Weighted_Mean_Year = sum(PY * Z9) / sum(Z9),
    Weighted_Mean_Yearv = sum(PY * WeightPY) / sum(WeightPY),
    
    # Strategy 3: Price Index (% papers in last 5 years)
    Price_Index = mean(PY >= (current_year - 5)),
    
    # Strategy 4: Normalized Mean Age
    Cluster_Mean_Age = mean(current_year - PY),
    Norm_Mean_Age = Cluster_Mean_Age / global_mean_age,
    
    # Strategy 5 Normalized PY
    Norm_PY = Ave_PY / global_mean_py
  )

cluster_summary$global_age <- global_mean_age
cluster_summary$global_py <- global_mean_py 

# 3. Updated Plotting Function
# Added y_var parameter
create_plot <- function(data, x_var, y_var, title_text) {
  
  # Use .data[[string]] to access columns dynamically
  ggplot(data, aes(x = .data[[x_var]], y = .data[[y_var]], size = Count, label = Cluster)) +
    geom_point(alpha = 0.7, color = "steelblue") +
    geom_text(vjust = 1.5, size = 3, check_overlap = TRUE) +
    scale_size_continuous(range = c(3, 10)) +
    theme_minimal() +
    labs(
      title = title_text,
      x = x_var,
      y = y_var # Automatically labels y-axis with the column name
    ) +
    theme(legend.position = "none")
}

# Example Usage:
# You can now change the 3rd argument to "Ave_Z9_rank", "Ave_Z9_zs", or "Ave_Z9_log"
target_y <- "Ave_Z9"
target_y <- "Ave_Z9_rank"
target_y <- "Ave_Z9_log"
target_y <- "Ave_Z9_zs"

p1 <- create_plot(cluster_summary, "Ave_PY", target_y, "Baseline: Average PY")
p2 <- create_plot(cluster_summary, "Median_PY", target_y, "Strategy 1: Median PY")
p3 <- create_plot(cluster_summary, "Weighted_Mean_Year", target_y, "Strategy 2: Weighted Mean Year")
p4 <- create_plot(cluster_summary, "Price_Index", target_y, "Strategy 3: Price Index")
p5 <- create_plot(cluster_summary, "Weighted_Mean_Yearv", target_y, "Strategy 2v: Weighted Mean Year v2")
p6 <- create_plot(cluster_summary, "Norm_PY", target_y, "Strategy 5: Norm. PY")

# Display
(p1 | p2) / (p3 | p4) / (p5 | p6)

# Selected metrics
# Weighted Year v
# Either rank or log norm

cluster_summary$score <- (rank(-cluster_summary$Ave_Z9_log) * rank(-cluster_summary$Weighted_Mean_Yearv))
cluster_summary$score_rank <- rank(-cluster_summary$score)


#############
# Create the horizontal boxplot
p_box <- df %>%
  mutate(Cluster = fct_reorder(Cluster, Z9_log, .fun = median)) %>%
  ggplot(aes(x = Z9_log, y = Cluster)) +
  geom_boxplot(fill = "steelblue", alpha = 0.7, outlier.alpha = 0.3) +
  theme_minimal() +
  labs(
    title = "Distribution of Normalized Citations (Z9_log) per Cluster",
    subtitle = "Ordered by Mean Z9_log (Highest on Top)",
    x = "Log-Normalized Citations (Z9_log)",
    y = NULL # Hide y-axis label as Cluster names are self-explanatory
  ) +
  theme(
    panel.grid.major.y = element_blank() # Remove horizontal grid lines for cleaner look
  )

# Display the plot
p_box

