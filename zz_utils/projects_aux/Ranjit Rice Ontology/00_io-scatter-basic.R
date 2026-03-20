dff <- summary_intervention_outcome

colnames(dff)

# Load required libraries
library(ggplot2)
library(ggrepel)
library(dplyr)

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
    "Grazing and livestock-related interventions",
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
    "Other"
  ),
  stringsAsFactors = FALSE
)

# Filter and prepare data
dff_filtered <- dff %>%
  filter(count >= 5) %>%
  left_join(intervention_mapping, by = "intervention_category") %>%
  left_join(outcome_mapping, by = "output_category") %>%
  mutate(
    # Create combined label
    label = paste0(intervention_short, " → ", outcome_short)
  )

# Create the scatter plot
p <- ggplot(dff_filtered, aes(x = mean_year, y = mean_citations)) +
  # Points with color fill by intervention and border by outcome
  geom_point(aes(size = count, fill = intervention_short, color = outcome_short),
             shape = 21, alpha = 0.7, stroke = 1.5) +
  
  # Add labels with ggrepel
  geom_text_repel(aes(label = label),
                  size = 3,
                  max.overlaps = 20,
                  box.padding = 0.5,
                  point.padding = 0.3,
                  segment.color = "grey50",
                  segment.size = 0.3,
                  min.segment.length = 0) +
  
  # Scale for point size
  scale_size_continuous(name = "Count", range = c(3, 15)) +
  
  # Color scales
  scale_fill_viridis_d(name = "Intervention Category", option = "turbo") +
  scale_color_viridis_d(name = "Outcome Category", option = "mako") +
  
  # Labels
  labs(
    x = "Average Publication Year",
    y = "Average Citations",
    title = "Bibliometric Analysis: Interventions and Outcomes",
    subtitle = "Points sized by study count (n ≥ 5)"
  ) +
  
  # Theme
  theme_bw(base_size = 12) +
  theme(
    legend.position = "right",
    legend.box = "vertical",
    legend.text = element_text(size = 9),
    legend.title = element_text(size = 10, face = "bold"),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, color = "grey40"),
    panel.grid.minor = element_blank(),
    axis.title = element_text(face = "bold")
  ) +
  
  # Guides to prevent legend overcrowding
  guides(
    fill = guide_legend(override.aes = list(size = 5), ncol = 1),
    color = guide_legend(override.aes = list(size = 5), ncol = 1),
    size = guide_legend(ncol = 1)
  )

# Display the plot
print(p)

# Save the plot
ggsave("scatter_plot_interventions_outcomes.png", 
       plot = p, 
       width = 16, 
       height = 10, 
       dpi = 300,
       bg = "white")

ggsave("scatter_plot_interventions_outcomes.pdf", 
       plot = p, 
       width = 16, 
       height = 10,
       bg = "white")

# Print summary statistics
cat("\nSummary Statistics:\n")
cat("Total data points plotted:", nrow(dff_filtered), "\n")
cat("Year range:", min(dff_filtered$mean_year), "-", max(dff_filtered$mean_year), "\n")
cat("Citation range:", min(dff_filtered$mean_citations), "-", max(dff_filtered$mean_citations), "\n")
cat("Count range:", min(dff_filtered$count), "-", max(dff_filtered$count), "\n")