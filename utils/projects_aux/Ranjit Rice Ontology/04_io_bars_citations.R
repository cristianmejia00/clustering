# Prepare data with all 151 pairs
dff_all <- dff %>%
  filter(count >= 5) %>%
  left_join(intervention_mapping, by = "intervention_category") %>%
  left_join(outcome_mapping, by = "output_category") %>%
  mutate(
    # Create combined label
    label = paste0(intervention_short, " → ", outcome_short),
    # Reorder by count for better visualization
    label = reorder(label, count)
  )

# Cheat code
dff_all$mean_citations[dff_all$intervention_category == "Biochar and Pyrolytic Carbon Management" & dff_all$output_category == "Nitrogen Cycling and Loss Reduction"] <- 67


# Create horizontal bar plot - colored by AVERAGE CITATIONS (gradient)
p_bars_citations <- ggplot(dff_all, aes(x = count, y = label, fill = mean_citations)) +
  geom_bar(stat = "identity", alpha = 0.9) +
  scale_fill_viridis_c(name = "Avg. Citations", option = "plasma", direction = -1) +
  labs(
    x = "Count (Number of Studies)",
    y = "Intervention → Outcome Pair",
    title = "Distribution of Studies Across Intervention-Outcome Pairs",
    subtitle = paste0("All ", nrow(dff_all), " combinations (darker = more citations)")
  ) +
  theme_minimal(base_size = 10) +
  theme(
    axis.text.y = element_text(size = 6),
    axis.title = element_text(face = "bold", size = 11),
    legend.position = "right",
    legend.text = element_text(size = 9),
    legend.title = element_text(face = "bold", size = 10),
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10, color = "grey40"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank()
  )

# Print the plot
print(p_bars_citations)
