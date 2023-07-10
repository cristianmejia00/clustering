"""
20210610
Create a scatter plot with metrics keywords data.
Inputs: the .csv (or object) with the metrics_kwds_summary
Outputs: a plotly scatter plot where:
- x axis is the years (mean or median) of the articles including the keyword
- y axis is the number of citations (mean or median) of the articles including the keyword
- size relative to the number of the articles including the keyword
- color depending on the metric type (i.e. bibliometric, scientometric or informetric)
"""

# Call library
library(plotly)

# Get data
metrics_kwds_summary <- read.csv("metrics_kwds_summary.csv", stringsAsFactors = FALSE, header = TRUE)

# Format labels
t <- list(
  family = "arial",
  size = 10,
  color = toRGB("black")
  )

# Generate plot
p <- plot_ly(metrics_kwds_summary, 
             x = metrics_kwds_summary$PY_Mean, 
             y = metrics_kwds_summary$Z9_Mean, 
             mode = "markers", 
             type = "scatter",
             size = as.numeric(metrics_kwds_summary$articles),
             text = metrics_kwds_summary$label, 
             color = metrics_kwds_summary$metric
)
p <- p %>% add_text(textfont = t, textposition = "top left")
p