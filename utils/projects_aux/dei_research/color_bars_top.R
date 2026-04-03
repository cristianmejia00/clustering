library(ggplot2)
library(dplyr)

# 1. Exact Data Entry
# Values extracted directly from the labels in the high-res image
data <- bind_rows(
  # Space Group
  data.frame(Group = "Space", Category = "MII + ME",   Value = 2.05),
  data.frame(Group = "Space", Category = "MID + ME",    Value = 1.35),
  data.frame(Group = "Space", Category = "MII + SE",   Value = 1.29),
  data.frame(Group = "Space", Category = "SI + ME", Value = 1.12),
  data.frame(Group = "Space", Category = "SI + SE",  Value = 1.00),
  data.frame(Group = "Space", Category = "MID + SE",  Value = 0.88),
  
  # AI Group
  data.frame(Group = "AI", Category = "MII + ME",   Value = 2.47),
  data.frame(Group = "AI", Category = "MII + SE",   Value = 2.04),
  data.frame(Group = "AI", Category = "MID + ME",    Value = 1.51),
  data.frame(Group = "AI", Category = "SI + ME", Value = 1.44),
  data.frame(Group = "AI", Category = "MID + SE",  Value = 1.27),
  data.frame(Group = "AI", Category = "SI + SE",  Value = 1.00),
  
  # Sustainability Group
  data.frame(Group = "Sustainability", Category = "MII + ME",   Value = 2.10),
  data.frame(Group = "Sustainability", Category = "MII + SE",   Value = 1.49),
  data.frame(Group = "Sustainability", Category = "MID + SE",  Value = 1.27),
  data.frame(Group = "Sustainability", Category = "MID + ME",    Value = 1.20),
  data.frame(Group = "Sustainability", Category = "SI + ME", Value = 1.01),
  data.frame(Group = "Sustainability", Category = "SI + SE",  Value = 1.00),
  
  # Management Group
  data.frame(Group = "Management", Category = "MII + ME",   Value = 1.73),
  data.frame(Group = "Management", Category = "MII + SE",   Value = 1.50),
  data.frame(Group = "Management", Category = "MID + ME",    Value = 1.29),
  data.frame(Group = "Management", Category = "MID + SE",  Value = 1.10),
  data.frame(Group = "Management", Category = "SI + SE",  Value = 1.00),
  data.frame(Group = "Management", Category = "SI + ME", Value = 0.97),
  
  # Quantum Group
  data.frame(Group = "Quantum", Category = "MII + ME",   Value = 1.99),
  data.frame(Group = "Quantum", Category = "MID + ME",    Value = 1.60),
  data.frame(Group = "Quantum", Category = "MII + SE",   Value = 1.52),
  data.frame(Group = "Quantum", Category = "MID + SE",  Value = 1.31),
  data.frame(Group = "Quantum", Category = "SI + ME", Value = 1.26),
  data.frame(Group = "Quantum", Category = "SI + SE",  Value = 1.00),
  
  # Wellbeing Group
  data.frame(Group = "Wellbeing", Category = "MII + ME",   Value = 1.49),
  data.frame(Group = "Wellbeing", Category = "MII + SE",   Value = 1.44),
  data.frame(Group = "Wellbeing", Category = "MID + ME",    Value = 1.24),
  data.frame(Group = "Wellbeing", Category = "SI + ME", Value = 1.12),
  data.frame(Group = "Wellbeing", Category = "SI + SE",  Value = 1.00),
  data.frame(Group = "Wellbeing", Category = "MID + SE",  Value = 0.88)
)

# 2. Logic to Sort Bars Independently Per Panel
# We set the Group order first so the panels appear in the correct grid positions
data$Group <- factor(data$Group, levels = c("Space", "AI", "Sustainability", 
                                            "Management", "Quantum", "Wellbeing"))

# We create a combined column (Group + Category). 
# We then order this column based on the Value (descending).
# This "tricks" ggplot into sorting every panel individually.
data <- data %>%
  arrange(Group, desc(Value)) %>%
  mutate(Unique_Bar_ID = factor(paste(Group, Category, sep="_"), 
                                levels = paste(Group, Category, sep="_")))

# 3. Define Exact Colors
custom_colors <- c(
  "MII + ME"   = "#2ca02c",  # Green
  "MII + SE"   = "#d62728",  # Red
  "MID + ME"    = "#1f77b4",  # Blue
  "MID + SE"  = "#ff7f0e",  # Orange
  "SI + ME" = "#9467bd",  # Purple
  "SI + SE"  = "#8c564b"   # Brown
)

# 4. Create the Plot
p <- ggplot(data, aes(x = Unique_Bar_ID, y = Value, fill = Category)) +
  geom_bar(stat = "identity", width = 0.7) +
  
  # Labels on top of bars
  geom_text(aes(label = paste0(format(Value, nsmall=2), "x")), 
            vjust = -0.4, 
            size = 2.8, 
            fontface = "bold") +
  
  # Create the 2x3 Grid
  facet_wrap(~ Group, scales = "free_x", ncol = 3) +
  
  # Apply Colors
  scale_fill_manual(values = custom_colors) +
  
  # Fix X-axis labels (Remove the hidden 'Group_' prefix we added for sorting)
  scale_x_discrete(labels = function(x) sub(".*_", "", x)) +
  
  # Labels and Theme
  labs(y = "(Top 10% / Total) / (SI + SE Top 10% / SI + SE Total)", x = "") +
  theme_bw() +
  theme(
    # Panel titles (Strip)
    strip.background = element_rect(fill = "white", color = NA),
    strip.text = element_text(face = "bold", size = 10, hjust = 0),
    
    # Grid lines (Horizontal mainly, lighter vertical)
    panel.grid.major.x = element_line(color = "gray95"),
    panel.grid.major.y = element_line(color = "gray85"),
    panel.grid.minor = element_blank(),
    
    # Axis Text
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9, color = "black"),
    axis.title = element_text(size = 9, face = "bold"),
    
    # Legend
    legend.position = "none" # Hidden as requested by visual match
  ) +
  
  # Expand Y axis slightly to fit the labels on top
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)))

# Display
print(p)