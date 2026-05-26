library(readr)

# Hydrogen PRODUCTION (dataset 2)

# Load required environment and dataset
load("~/Library/CloudStorage/GoogleDrive-cristianmejia00@gmail.com/My Drive/Bibliometrics_Drive/Q345_hydrogen_d2/a01_cn__f01_dc__c01_lv_v1/louvain/0.9/level1/environ.rdata")

cluster_summary_short_dc <- read_csv("~/Library/CloudStorage/GoogleDrive-cristianmejia00@gmail.com/My Drive/Bibliometrics_Drive/Q345_hydrogen_d2/a01_cn__f01_dc__c01_lv_v1/louvain/0.9/level1/cluster_summary.csv")
cluster_summary_short_dc$cluster_name <- NULL
cluster_summary_short_dc$subcluster_name <- cluster_summary_short_dc$global_name

main_clusters <- read_csv("~/Library/CloudStorage/GoogleDrive-cristianmejia00@gmail.com/My Drive/Bibliometrics_Drive/Q345_hydrogen_d2/a01_cn__f01_dc__c01_lv_v1/louvain/0.9/level0/cluster_summary.csv")
main_clusters$cluster_name <- NULL
main_clusters$cluster_name <- main_clusters$global_name
main_clusters$main_cluster <- main_clusters$cluster_code

# Fix encoding errors for a label
#cluster_summary$subcluster_name[cluster_summary$cluster_code == "8-1---"] <- "Power Hydrogen Network Coordination and Filling Stations"

# Append the Yearly-normalized citations
dataset <- dataset %>%
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
current_year <- max(dataset$PY)
global_mean_age <- mean(current_year - dataset$PY)
global_mean_py <- mean(dataset$PY)

# frequencies of the years
PY_freq <- table(dataset$PY) %>% sort(decreasing = TRUE)
dataset$WeightPY <- PY_freq[as.character(dataset$PY)]


# 2. Calculate Summaries & Strategies
dataset$cluster_code <- dataset$subcluster_label1
cluster_summary <- dataset %>%
  group_by(cluster_code) %>%
  summarize(
    Count = n(),
    
    # Original Metric
    Z9_Ave = mean(Z9),
    
    # --- NEW METRICS AGGREGATION ---
    Z9_Ave_rank = mean(Z9_rank),
    Z9_Ave_zs = mean(Z9_zs),
    Z9_Ave_log = mean(Z9_log),
    # -------------------------------
    
    # Baseline
    PY_Ave = mean(PY),
    
    # Strategy 1: Median Year
    PY_Median = median(PY),
    
    # Strategy 2: Weighted Mean Year (Center of Gravity)
    PY_Weighted_Mean_Year = sum(PY * Z9) / sum(Z9),
    PY_Weighted_Mean_Yearv = sum(PY * WeightPY) / sum(WeightPY),
    
    # Strategy 3: Price Index (% papers in last 5 years)
    PY_Price_Index = mean(PY >= (current_year - 5)),
    
    # Strategy 4: Normalized Mean Age
    PY_Cluster_Mean_Age = mean(current_year - PY),
    PY_Norm_Mean_Age = PY_Cluster_Mean_Age / global_mean_age,
    
    # Strategy 5 Normalized PY
    PY_Norm = PY_Ave / global_mean_py
  )

cluster_summary$global_age <- global_mean_age
cluster_summary$global_py <- global_mean_py 


# Merge with rcs_merged
cluster_summary <- cluster_summary %>%
  left_join(
    rcs_merged[c("cluster_code", "main_cluster")],
    by = "cluster_code"
  )
cluster_summary$xxx <- as.character(cluster_summary$main_cluster)
cluster_summary$fff <- cluster_summary$main_cluster
cluster_summary$main_cluster <- as.numeric(as.character(cluster_summary$main_cluster))

cluster_summary <- cluster_summary %>%
  left_join(
    main_clusters[c("main_cluster", "cluster_name")],
    by = "main_cluster"
  )

cluster_summary <- cluster_summary %>%
  left_join(
    cluster_summary_short_dc[c("cluster_code", "subcluster_name", "documents", "description")],
    by = "cluster_code"
  )

cluster_summary$label <- paste(cluster_summary$cluster_code, cluster_summary$subcluster_name, sep="")

# Save report
write.csv(cluster_summary[c("cluster_code",	"cluster_name",	"subcluster_name",	"documents",	"PY_Ave",	"Z9_Ave", "Z9_Ave_rank",	"description")],
          file = "cluster_summary.csv",
          row.names = FALSE)


#  Color palette
fukan_colors <- c("#f00f15","#2270e7","#e5e510","#ff8103","#4f3dd1",
                  "#26cc3a","#ec058e","#9cb8c2","#BF5B17","#b40e68")
fukan_colors_extended <- c(fukan_colors, tolower(c(
  "#5AFB5A","#BEAED4","#FDC086","#C430FF","#E4DBE0","#99FDFF", "#666666", "#fffdd0"
)))

# Map each unique main_cluster to a color in order
unique_clusters <- sort(unique(cluster_summary$main_cluster))
color_palette <- setNames(
  rep_len(fukan_colors_extended, length(unique_clusters)),
  unique_clusters
)

# Ensure main_cluster is a factor so color order is respected
cluster_summary <- cluster_summary %>%
  mutate(main_cluster = factor(main_cluster, levels = unique_clusters))

#  Scatterplot 
ggplot(cluster_summary,
       aes(x = PY_Ave,
           y = Z9_Ave_rank,
           size = Count,
           color = main_cluster,
           label = cluster_code)) +
  geom_point(alpha = 0.75) +
  geom_text_repel(
    size          = 3,
    max.overlaps  = 20,
    segment.color = "grey50",
    segment.size  = 0.3,
    box.padding   = 0.4,
    show.legend   = FALSE
  ) +
  scale_color_manual(values = color_palette) +
  scale_size_continuous(range = c(2, 12)) +
  scale_x_continuous(
    breaks = seq(
      floor(min(cluster_summary$PY_Ave, na.rm = TRUE)),
      ceiling(max(cluster_summary$PY_Ave, na.rm = TRUE)),
      by = 2
    )
  ) +
  labs(
    x     = "Average Publication Year",
    y     = "Mean Yearly-Normalized Citations (Z9)",
    size  = "Documents",
    color = "Main Cluster",
    title = "Cluster Landscape: Impact vs. Recency"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position  = "right",
    panel.grid.minor = element_blank()
  )


# Order cluster_name as a factor by ascending cluster_code
cluster_summary <- cluster_summary %>%
  arrange(main_cluster, -documents) %>%
  mutate(label = factor(label, levels = rev(label)))

ggplot(cluster_summary,
       aes(x = documents,
           y = label,
           fill = main_cluster)) +
  geom_bar(stat = "identity", alpha = 0.85) +
  scale_fill_manual(values = color_palette) +
  labs(
    x     = "Number of Documents",
    y     = NULL,
    fill  = "Main Cluster",
    title = "Documents per Cluster"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position    = "right",
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_blank()
  )

