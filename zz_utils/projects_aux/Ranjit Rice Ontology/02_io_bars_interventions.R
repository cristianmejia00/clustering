
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

# Create horizontal bar plot
p_bars <- ggplot(dff_all, aes(x = count, y = label, fill = intervention_short)) +
  geom_bar(stat = "identity", alpha = 0.85) +
  scale_fill_viridis_d(name = "Intervention Category", option = "turbo") +
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
    legend.position = "right",
    legend.text = element_text(size = 9),
    legend.title = element_text(face = "bold", size = 10),
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10, color = "grey40"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank()
  )

# Print the plot
print(p_bars)


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
