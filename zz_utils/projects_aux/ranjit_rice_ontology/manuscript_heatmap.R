# Matrix Visualization of Intervention-Outcome Pairs
# Two approaches: Scatter plot (dot size) and Heatmap (color intensity)
# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(viridis)

# Create mapping for Intervention categories (short labels)
intervention_mapping <- data.frame(
  intervention_category = c(
    "Biochar and Pyrolytic Carbon Management",
    "Integrated Water Management Systems",
    "Organic Matter and Biomass Valorization",
    "Precision Agriculture and Digital Technologies",
    "Biological Solutions and Bio-Inputs",
    "Aquaculture-Agriculture Systems",
    "Sustainable Cropping and Tillage Systems",
    "Nutrient Optimization and Fertilizer Management",
    "Stress Resilience and Climate Adaptation",
    "Soil and Water Remediation",
    "Pests, Weeds & Disease Management",
    "Grazing and Livestock Related Interventions",
    "Other"
  ),
  intervention_short = c(
    "Biochar & Pyrolytic Carbon",
    "Integrated Water Management",
    "Organic Matter Valorization",
    "Precision Agriculture",
    "Biological Solutions",
    "Aquaculture-Agriculture",
    "Sustainable Cropping Systems",
    "Nutrient Optimization",
    "Stress Resilience",
    "Soil & Water Remediation",
    "Pest & Disease Management",
    "Grazing & Livestock",
    "Other/Unspecified"
  ),
  stringsAsFactors = FALSE
)

# Create mapping for Outcome categories (short labels)
outcome_mapping <- data.frame(
  output_category = c(
    "Crop Yield Enhancement and Quality Improvement",
    "Resource Use Efficiency Optimization",
    "Soil Health and Fertility Enhancement",
    "Greenhouse Gas Mitigation and Carbon Management",
    "Water Conservation and Management",
    "Heavy Metal and Contaminant Remediation",
    "Nitrogen Cycling and Loss Reduction",
    "Integrated Pest and Disease Control",
    "Biodiversity Conservation and Ecosystem Services",
    "Agricultural Waste Valorization and Circular Economy",
    "Economic Profitability and Cost Optimization",
    "Livelihood Security and Rural Development",
    "Salinity and Sodicity Management",
    "Erosion Control and Landscape Management",
    "Water Quality and Pollution Control",
    "Renewable Energy Integration",
    "Climate Adaptation and Resilience",
    "Other"
  ),
  outcome_short = c(
    "Crop Yield Enhancement",
    "Resource Use Efficiency",
    "Soil Health & Fertility",
    "GHG Mitigation",
    "Water Conservation",
    "Contaminant Remediation",
    "Nitrogen Cycling",
    "Pest & Disease Control",
    "Biodiversity & Ecosystems",
    "Waste Valorization",
    "Economic Profitability",
    "Livelihood Security",
    "Salinity Management",
    "Erosion Control",
    "Water Quality",
    "Renewable Energy",
    "Climate Adaptation",
    "Unspecified"
  ),
  stringsAsFactors = FALSE
)

# Prepare data with short labels
dff_labeled <- dff %>%
  left_join(intervention_mapping, by = "intervention_category") %>%
  left_join(outcome_mapping, by = "output_category")

# Calculate total counts for sorting
intervention_totals <- dff_labeled %>%
  group_by(intervention_short) %>%
  summarise(total_count = sum(count)) %>%
  arrange(desc(total_count))

outcome_totals <- dff_labeled %>%
  group_by(outcome_short) %>%
  summarise(total_count = sum(count)) %>%
  arrange(desc(total_count))

# Create ordered factors (highest count on left for interventions, top for outcomes)
dff_matrix <- dff_labeled %>%
  mutate(
    intervention_short = factor(intervention_short, 
                                levels = intervention_totals$intervention_short),
    outcome_short = factor(outcome_short, 
                           levels = rev(outcome_totals$outcome_short))  # Reverse for top-to-bottom
  )

# ============================================================================
# OPTION 1: SCATTER PLOT (Dot size represents count)
# ============================================================================
p_scatter <- ggplot(dff_matrix, aes(x = intervention_short, y = outcome_short)) +
  geom_point(aes(size = count, color = count), alpha = 0.7) +
  scale_size_continuous(name = "Count", range = c(1, 20)) +
  scale_color_viridis_c(name = "Count", option = "plasma") +
  labs(
    x = "Intervention Category",
    y = "Outcome Category",
    title = "Intervention-Outcome Matrix: Scatter Plot",
    subtitle = "Dot size and color intensity represent number of studies"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    axis.text.y = element_text(size = 9),
    axis.title = element_text(face = "bold", size = 12),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, color = "grey40"),
    panel.grid.major = element_line(color = "grey90"),
    legend.position = "right"
  )

print(p_scatter)

ggsave("matrix_scatter.png", 
       plot = p_scatter, 
       width = 14, 
       height = 10, 
       dpi = 300,
       bg = "white")

ggsave("matrix_scatter.pdf", 
       plot = p_scatter, 
       width = 14, 
       height = 10,
       bg = "white")

# ============================================================================
# OPTION 2: HEATMAP (Color intensity represents count)
# ============================================================================
p_heatmap <- ggplot(dff_matrix, aes(x = intervention_short, y = outcome_short, fill = count)) +
  geom_tile(color = "white", size = 0.5) +
  scale_fill_viridis_c(name = "Count", option = "inferno", direction = -1) +
  labs(
    x = "Intervention Category",
    y = "Outcome Category",
    title = "Intervention-Outcome Matrix: Heatmap",
    subtitle = "Color intensity represents number of studies (darker = more studies)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    axis.text.y = element_text(size = 9),
    axis.title = element_text(face = "bold", size = 12),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, color = "grey40"),
    panel.grid = element_blank(),
    legend.position = "right"
  )

print(p_heatmap)

ggsave("matrix_heatmap.png", 
       plot = p_heatmap, 
       width = 14, 
       height = 10, 
       dpi = 300,
       bg = "white")

ggsave("matrix_heatmap.pdf", 
       plot = p_heatmap, 
       width = 14, 
       height = 10,
       bg = "white")

# ============================================================================
# OPTION 3: HEATMAP WITH TEXT LABELS (showing actual counts)
# ============================================================================
p_heatmap_text <- ggplot(dff_matrix, aes(x = intervention_short, y = outcome_short, fill = count)) +
  geom_tile(color = "white", size = 0.5) +
  geom_text(aes(label = count), color = "white", size = 3, fontface = "bold") +
  scale_fill_viridis_c(name = "Count", option = "inferno", direction = -1) +
  labs(
    x = "Intervention Category",
    y = "Outcome Category",
    title = "Intervention-Outcome Matrix: Heatmap with Labels",
    subtitle = "Numbers show exact count of studies"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    axis.text.y = element_text(size = 9),
    axis.title = element_text(face = "bold", size = 12),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, color = "grey40"),
    panel.grid = element_blank(),
    legend.position = "right"
  )

print(p_heatmap_text)

ggsave("matrix_heatmap_text.png", 
       plot = p_heatmap_text, 
       width = 14, 
       height = 10, 
       dpi = 300,
       bg = "white")

ggsave("matrix_heatmap_text.pdf", 
       plot = p_heatmap_text, 
       width = 14, 
       height = 10,
       bg = "white")

# ============================================================================
# OPTION 4: TILE PLOT WITH SELECTIVE LABELS (only show high counts)
# ============================================================================
# Only label cells with count > threshold
count_threshold <- quantile(dff_matrix$count, 0.75)

p_tile_selective <- ggplot(dff_matrix, aes(x = intervention_short, y = outcome_short, fill = count)) +
  geom_tile(color = "grey80", size = 0.3) +
  geom_text(data = filter(dff_matrix, count > count_threshold),
            aes(label = count), color = "white", size = 3.5, fontface = "bold") +
  scale_fill_gradient(name = "Count", 
                      low = "lightyellow", 
                      high = "darkred",
                      na.value = "grey95") +
  labs(
    x = "Intervention Category",
    y = "Outcome Category",
    title = "Intervention-Outcome Matrix: Selective Labeling",
    subtitle = paste0("Labels shown for counts > ", round(count_threshold, 1), " (top 25%)")
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    axis.text.y = element_text(size = 9),
    axis.title = element_text(face = "bold", size = 12),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, color = "grey40"),
    panel.grid = element_blank(),
    legend.position = "right"
  )

print(p_tile_selective)

ggsave("matrix_tile_selective.png", 
       plot = p_tile_selective, 
       width = 14, 
       height = 10, 
       dpi = 300,
       bg = "white")

# ============================================================================
# OPTION 5: BUBBLE CHART (Alternative to scatter plot)
# ============================================================================
p_bubble <- ggplot(dff_matrix, aes(x = intervention_short, y = outcome_short)) +
  geom_point(aes(size = count, fill = count), 
             shape = 21, color = "black", alpha = 0.8, stroke = 0.5) +
  scale_size_continuous(name = "Count", range = c(2, 25)) +
  scale_fill_gradient2(name = "Count",
                       low = "lightblue",
                       mid = "yellow",
                       high = "red",
                       midpoint = median(dff_matrix$count)) +
  labs(
    x = "Intervention Category",
    y = "Outcome Category",
    title = "Intervention-Outcome Matrix: Bubble Chart",
    subtitle = "Bubble size and color represent number of studies"
  ) +
  theme_light(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    axis.text.y = element_text(size = 9),
    axis.title = element_text(face = "bold", size = 12),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, color = "grey40"),
    panel.grid.major = element_line(color = "grey90"),
    legend.position = "right"
  )

print(p_bubble)

ggsave("matrix_bubble.png", 
       plot = p_bubble, 
       width = 14, 
       height = 10, 
       dpi = 300,
       bg = "white")

# ============================================================================
# Summary Statistics
# ============================================================================
cat("\n=============== MATRIX SUMMARY ===============\n")
cat("Total intervention-outcome pairs:", nrow(dff_matrix), "\n")
cat("Total studies:", sum(dff_matrix$count), "\n\n")

cat("Interventions ranked by total count:\n")
print(intervention_totals)

cat("\nOutcomes ranked by total count:\n")
print(outcome_totals)

cat("\nTop 10 intervention-outcome combinations:\n")
top_combinations <- dff_matrix %>%
  arrange(desc(count)) %>%
  select(intervention_short, outcome_short, count) %>%
  head(10)
print(top_combinations)

# Print sparsity information
cat("\nMatrix Sparsity:\n")
total_possible <- length(unique(dff_matrix$intervention_short)) * length(unique(dff_matrix$outcome_short))
actual_pairs <- nrow(dff_matrix)
cat("Possible combinations:", total_possible, "\n")
cat("Actual combinations:", actual_pairs, "\n")
cat("Sparsity:", round((1 - actual_pairs/total_possible) * 100, 1), "%\n")