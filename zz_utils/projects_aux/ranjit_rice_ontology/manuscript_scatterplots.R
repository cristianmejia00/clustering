# Alternative version with different aesthetics
# Load required libraries
library(ggplot2)
library(ggrepel)
library(dplyr)
library(RColorBrewer)

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

# Filter and prepare data
dff_filtered <- dff %>%
  filter(count >= 5) %>%
  left_join(intervention_mapping, by = "intervention_category") %>%
  left_join(outcome_mapping, by = "output_category") %>%
  mutate(
    label = paste0(intervention_short, " → ", outcome_short)
  )

# Alternative 1: Faceted by intervention category
p1 <- ggplot(dff_filtered, aes(x = mean_year, y = mean_citations)) +
  geom_point(aes(size = count, fill = outcome_short),
             shape = 21, alpha = 0.7, color = "black", stroke = 0.5) +
  geom_text_repel(aes(label = outcome_short),
                  size = 2.5,
                  max.overlaps = 15) +
  scale_size_continuous(name = "Count", range = c(2, 10)) +
  scale_fill_brewer(name = "Outcome Category", palette = "Set3") +
  facet_wrap(~intervention_short, scales = "free") +
  labs(x = "Average Publication Year",
       y = "Average Citations",
       title = "Interventions and Outcomes by Category") +
  theme_bw() +
  theme(legend.position = "bottom",
        strip.text = element_text(face = "bold", size = 8))

# Alternative 2: Simpler version with manual color palette
# Define color palettes
n_interventions <- length(unique(dff_filtered$intervention_short))
n_outcomes <- length(unique(dff_filtered$outcome_short))

p2 <- ggplot(dff_filtered, aes(x = mean_year, y = mean_citations)) +
  geom_point(aes(size = count, fill = intervention_short, color = outcome_short),
             shape = 21, alpha = 0.6, stroke = 2) +
  geom_text_repel(aes(label = label),
                  size = 2.8,
                  max.overlaps = 15,
                  box.padding = 0.4,
                  segment.size = 0.2,
                  segment.color = "grey60",
                  force = 2) +
  scale_size_continuous(name = "Study Count", range = c(4, 20), 
                        breaks = c(5, 10, 20, 50, 100)) +
  scale_fill_manual(name = "Intervention",
                    values = colorRampPalette(brewer.pal(12, "Set3"))(n_interventions)) +
  scale_color_manual(name = "Outcome",
                     values = colorRampPalette(brewer.pal(9, "Set1"))(n_outcomes)) +
  labs(x = "Average Publication Year",
       y = "Average Number of Citations",
       title = "Bibliometric Landscape of Agricultural Interventions and Outcomes",
       subtitle = "Studies with n ≥ 5 observations") +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "right",
    legend.box = "vertical",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 13),
    axis.title = element_text(face = "bold")
  )

# Alternative 3: Highlight high-impact studies
# Define high-impact threshold
citation_threshold <- quantile(dff_filtered$mean_citations, 0.75)

dff_filtered <- dff_filtered %>%
  mutate(high_impact = mean_citations >= citation_threshold)

p3 <- ggplot(dff_filtered, aes(x = mean_year, y = mean_citations)) +
  geom_point(aes(size = count, fill = intervention_short, 
                 color = outcome_short, alpha = high_impact),
             shape = 21, stroke = 1.5) +
  scale_alpha_manual(values = c(0.4, 0.9), guide = "none") +
  geom_text_repel(data = filter(dff_filtered, high_impact),
                  aes(label = label),
                  size = 3,
                  fontface = "bold",
                  max.overlaps = 20,
                  box.padding = 0.5) +
  scale_size_continuous(name = "Count", range = c(3, 15)) +
  scale_fill_viridis_d(name = "Intervention", option = "plasma") +
  scale_color_viridis_d(name = "Outcome", option = "viridis") +
  labs(x = "Average Publication Year",
       y = "Average Citations",
       title = "High-Impact Studies Highlighted",
       subtitle = paste0("Bold labels for top 25% cited studies (≥", round(citation_threshold, 1), " citations)")) +
  theme_classic() +
  theme(legend.position = "right",
        plot.title = element_text(face = "bold"))

# Alternative 4: Same as p1 but with fixed y-axis limits
p4 <- ggplot(dff_filtered, aes(x = mean_year, y = mean_citations)) +
  geom_point(aes(size = count, fill = outcome_short),
             shape = 21, alpha = 0.7, color = "black", stroke = 0.5) +
  geom_text_repel(aes(label = outcome_short),
                  size = 2.5,
                  max.overlaps = 15) +
  scale_size_continuous(name = "Count", range = c(2, 10)) +
  scale_fill_brewer(name = "Outcome Category", palette = "Set3") +
  facet_wrap(~intervention_short) +  # Only x-axis is free, y-axis is fixed
  labs(x = "Average Publication Year",
       y = "Average Citations",
       title = "Interventions and Outcomes by Category (Fixed Y-axis)") +
  theme_bw() +
  theme(legend.position = "bottom",
        strip.text = element_text(face = "bold", size = 8))

# Print all plots
print(p1)
print(p2)
print(p3)
print(p4)

# Save all versions
ggsave("scatter_faceted.png", plot = p1, width = 16, height = 12, dpi = 300, bg = "white")
ggsave("scatter_simple.png", plot = p2, width = 14, height = 10, dpi = 300, bg = "white")
ggsave("scatter_highlighted.png", plot = p3, width = 14, height = 10, dpi = 300, bg = "white")
ggsave("scatter_faceted_fixed_y.png", plot = p4, width = 16, height = 12, dpi = 300, bg = "white")