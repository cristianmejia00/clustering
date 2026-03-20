# Create horizontal bar plot - colored by OUTCOME
p_bars_outcome <- ggplot(dff_all, aes(x = count, y = label, fill = outcome_short)) +
  geom_bar(stat = "identity", alpha = 0.85) +
  scale_fill_viridis_d(name = "Outcome Category", option = "plasma") +
  labs(
    x = "Count (Number of Studies)",
    y = "Intervention → Outcome Pair",
    title = "Distribution of Studies Across Intervention-Outcome Pairs",
    subtitle = paste0("All ", nrow(dff_all), " intervention-outcome combinations (colored by outcome)")
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
print(p_bars_outcome)


# Faceted by OUTCOME category
p_bars_faceted_outcome <- ggplot(dff_all, aes(x = count, y = intervention_short, fill = outcome_short)) +
  geom_bar(stat = "identity", alpha = 0.85) +
  facet_wrap(~outcome_short, scales = "free_y", ncol = 2) +
  scale_fill_viridis_d(name = "Outcome Category", option = "plasma") +
  labs(
    x = "Count (Number of Studies)",
    y = "Intervention Category",
    title = "Distribution of Studies: Interventions by Outcome",
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

print(p_bars_faceted_outcome)
