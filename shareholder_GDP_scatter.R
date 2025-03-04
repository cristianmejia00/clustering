library(dplyr)
library("ggplot2")
library("ggrepel")
library(readr)

country_gdp <- read_csv("~/Library/CloudStorage/OneDrive-Personal/Documentos/00-Research projects/67 - Shareholder Network (Moodys)/02-Results/20250218/countries/country_summary_selected_gsp.csv")

# 
p <- ggplot(country_gdp, aes(round(gdp/1000,0), scope1_2)) +
              geom_point() + 
              xlab("GDP Billions USD") + 
              #ylab("Difference Millions tCO2e (expanded - direct iScope3)") +
              #ylab("Expanded i-scope3 Millions tCO2e") +
              #ylab("Direct i-scope3 Millions tCO2e") +
              ylab("Scope 1 & 2 Millions tCO2e") +
              geom_smooth() + 
              theme_bw() + # Use a white background with grid lines
              theme(panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank(), 
                    legend.position = "none") + # Remove grid lines
              theme(axis.line = element_line(colour = "black")) + 
              geom_text_repel(aes(label=country), max.overlaps = 20)
p
    # scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
    #               labels = trans_format("log10", math_format(10^.x))) +
    # scale_y_log10(
    #   breaks = trans_breaks("log10", function(x) 10^x, n = 6),
    #   labels = trans_format("log10", math_format(10^.x))) +

corr = cor(country_gdp$gdp, country_gdp$difference)

tmp <- country_gdp %>% filter(country != "China")
corr = cor(tmp$gdp, tmp$difference)
