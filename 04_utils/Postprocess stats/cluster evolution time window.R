# 20210303

# Code to recreate Figure "4" of 20210303 CIM Main Document submission
# Caption: "Evolution of creativity research. The x-axis represents the proportion of articles per cluster. The y-axis is different windows of time."

# Input
# Load the environment for the analysis level0 (The main clustering)
# Note: For the article we used the level 0, but other vases we can use subclusters too.
# If the environment is not available. At least we need to load the mission.facet.all from Fukan System and rename it as myDataCorrect

# Output
# A .csv file to be opened in Excel where we make the figure. 
# Excel instructions:
# Change the header to read "Cluster 1", "Cluster 2", etc.
# Select the table >  insert chart >  Recommended charts >  100% stacked column
# Add a column for row names:  <= 1990; 1991 - 1995; 1996 - 2000; 2001 - 2005; 2006- 2010; 2011 - 2015; 2016 - 2020
# Chart design > Switch Row/Column
# Delete the title of the chart
# Right click on the chart > format data series > Gap width


library(dplyr)
library(data.table)
library(plyr)

cr_2000 <- myDataCorrect[myDataCorrect$PY <= 2000,]
cr_2005 <- myDataCorrect[myDataCorrect$PY > 2000 & myDataCorrect$PY <= 2005,]
cr_2010 <- myDataCorrect[myDataCorrect$PY > 2005 & myDataCorrect$PY <= 2010,]
cr_2015 <- myDataCorrect[myDataCorrect$PY > 2010 & myDataCorrect$PY <= 2015,]
cr_2020 <- myDataCorrect[myDataCorrect$PY > 2015,]

cr_2000 <- table(cr_2000$level0) %>% prop.table() %>% as.matrix() %>% t() %>% as.data.frame()
cr_2005 <- table(cr_2005$level0) %>% prop.table() %>% as.matrix() %>% t() %>% as.data.frame()
cr_2010 <- table(cr_2010$level0) %>% prop.table() %>% as.matrix() %>% t() %>% as.data.frame()
cr_2015 <- table(cr_2015$level0) %>% prop.table() %>% as.matrix() %>% t() %>% as.data.frame()
cr_2020 <- table(cr_2020$level0) %>% prop.table() %>% as.matrix() %>% t() %>% as.data.frame()

trend_chart <- rbind.fill(cr_2000, cr_2005, cr_2010, cr_2015, cr_2020)

write.csv(trend_chart, file = "trend_chart_metrics.csv", row.names = FALSE)
