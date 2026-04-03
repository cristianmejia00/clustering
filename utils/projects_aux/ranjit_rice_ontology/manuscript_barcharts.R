# Horizontal Bar Plot of Intervention -> Outcome Pairs
# Load required libraries
library(ggplot2)
library(dplyr)
library(ggthemes)

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

# Prepare data with all 151 pairs
dff_all <- dff %>%
  left_join(intervention_mapping, by = "intervention_category") %>%
  left_join(outcome_mapping, by = "output_category") %>%
  mutate(
    # Create combined label
    label = paste0(intervention_short, " → ", outcome_short),
    # Reorder by count for better visualization
    label = reorder(label, count)
  )

?scale_fill_tableau

# Create horizontal bar plot
p_bars <- ggplot(dff_all, aes(x = count, y = label, fill = intervention_short)) +
  geom_bar(stat = "identity") +
  scale_fill_tableau(palette = "Tableau 20", name = "Intervention Category") +
  #scale_fill_brewer(palette = "Set3", name = "Intervention Category") +
  #scale_fill_viridis_d(name = "Intervention Category", option = "turbo") +
  labs(
    x = "Count (Number of Studies)",
    y = "Intervention → Outcome Pair",
    title = "Distribution of Studies Across Intervention-Outcome Pairs",
    subtitle = paste0("All ", nrow(dff_all), " intervention-outcome combinations")
  ) +
  theme_minimal(base_size = 10) +
  theme(
    axis.text.y = element_text(size = 6),
    axis.title = element_text(face = "bold", size = 11),
    legend.position = c(0.8, 0.4),
    #legend.position = "right",
    legend.text = element_text(size = 9),
    legend.title = element_text(face = "bold", size = 10),
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10, color = "grey40"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank()
  )

# Print the plot
print(p_bars)
table(dff$intervention_category) %>% names()

# Save the plot (needs to be very tall to accommodate all 151 bars)
ggsave("barplot_all_pairs.png", 
       plot = p_bars, 
       width = 12, 
       height = 30, 
       dpi = 300,
       bg = "white",
       limitsize = FALSE)

ggsave("barplot_all_pairs.pdf", 
       plot = p_bars, 
       width = 12, 
       height = 30,
       bg = "white",
       limitsize = FALSE)

# Alternative version: Faceted by intervention category
p_bars_faceted <- ggplot(dff_all, aes(x = count, y = outcome_short, fill = intervention_short)) +
  geom_bar(stat = "identity", alpha = 0.85) +
  facet_wrap(~intervention_short, scales = "free_y", ncol = 2) +
  scale_fill_viridis_d(name = "Intervention Category", option = "turbo") +
  labs(
    x = "Count (Number of Studies)",
    y = "Outcome Category",
    title = "Distribution of Studies: Outcomes by Intervention",
    subtitle = paste0("All ", nrow(dff_all), " intervention-outcome combinations")
  ) +
  theme_bw(base_size = 10) +
  theme(
    axis.text.y = element_text(size = 7),
    axis.title = element_text(face = "bold", size = 11),
    legend.position = "none",
    strip.text = element_text(face = "bold", size = 8),
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10, color = "grey40"),
    panel.grid.minor = element_blank()
  )

print(p_bars_faceted)

ggsave("barplot_faceted_by_intervention.png", 
       plot = p_bars_faceted, 
       width = 14, 
       height = 18, 
       dpi = 300,
       bg = "white")

ggsave("barplot_faceted_by_intervention.pdf", 
       plot = p_bars_faceted, 
       width = 14, 
       height = 18,
       bg = "white")

# Alternative version: Top 50 pairs only for readability
dff_top50 <- dff_all %>%
  arrange(desc(count)) %>%
  head(50) %>%
  mutate(label = reorder(label, count))

p_bars_top50 <- ggplot(dff_top50, aes(x = count, y = label, fill = intervention_short)) +
  geom_bar(stat = "identity", alpha = 0.85) +
  scale_fill_viridis_d(name = "Intervention Category", option = "turbo") +
  labs(
    x = "Count (Number of Studies)",
    y = "Intervention → Outcome Pair",
    title = "Top 50 Most Studied Intervention-Outcome Pairs",
    subtitle = "Ranked by number of studies"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.y = element_text(size = 8),
    axis.title = element_text(face = "bold", size = 12),
    legend.position = "right",
    legend.text = element_text(size = 9),
    legend.title = element_text(face = "bold", size = 10),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, color = "grey40"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank()
  )

print(p_bars_top50)

ggsave("barplot_top50_pairs.png", 
       plot = p_bars_top50, 
       width = 12, 
       height = 10, 
       dpi = 300,
       bg = "white")

ggsave("barplot_top50_pairs.pdf", 
       plot = p_bars_top50, 
       width = 12, 
       height = 10,
       bg = "white")

# Summary statistics
cat("\nSummary Statistics:\n")
cat("Total intervention-outcome pairs:", nrow(dff_all), "\n")
cat("Count range:", min(dff_all$count), "-", max(dff_all$count), "\n")
cat("Median count:", median(dff_all$count), "\n")
cat("Number of unique interventions:", length(unique(dff_all$intervention_short)), "\n")
cat("Number of unique outcomes:", length(unique(dff_all$outcome_short)), "\n")
cat("\nTop 10 most studied pairs:\n")
print(dff_all %>% 
        arrange(desc(count)) %>% 
        select(label, count, intervention_short, outcome_short) %>% 
        head(10))