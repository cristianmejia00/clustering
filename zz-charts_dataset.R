# 2023/07/13
# Dataset-level stats
# All bar charts in this dataset.


# Load plotting library
library(ggplot2)
library(glue)

# INPUT
dataset = dataset
document_label <- toTitleCase(params$type_of_dataset)
output_folder_level <- output_folder_level
subfolder_dataset <- 'charts_dataset'
subfolder_clusters <- 'charts_clusters'
available_columns <- available_columns
column_labels = rp$column_labels


# SYSTEM
dir.create(file.path(output_folder_level, subfolder_dataset)) 
dir.create(file.path(output_folder_level, subfolder_clusters)) 

##################################################################
##################################################################
##################################################################
# BAR CHARTS   --> summarize categorical columns 
##################################################################
# UTILS
#' param column_data: a column in the form of `dataset$something`
#' param item_label: the label of the category axis
#' param document_label: 'Document' or 'Paper', 'News, 'Patent', etc.
#' param top_items: the number of bars to show. The report will contain everything anyways.
#' param sorted_bars: TRUE, sort from the highest
#' param horizontal: TRUE, for horizontal bars
#' return nothing. But it saves the plot and report
create_report_and_barchart <- function(column_data,
                                     item_label = 'Item',
                                     document_label = 'Documents',
                                     top_items = 20,
                                     sorted_bars = TRUE,
                                     horizontal = TRUE) {
  stats_column <- column_data %>% 
    as.character() %>% 
    strsplit("; ") %>% 
    unlist() %>% 
    tolower() %>% 
    toTitleCase() %>% 
    table() %>% 
    sort(decreasing = TRUE) %>% 
    data.frame() %>% 
    setNames(c('Item', 'Documents'))
  write.csv(stats_column, 
            file = file.path(output_folder_level, subfolder_dataset, glue("dataset_{tolower(item_label)}.csv")), 
            row.names = FALSE)
  
  
  ggplot(stats_column[c(1:top_items),], aes(x=Item, y=Documents)) + 
    geom_bar(stat = "identity", width = 0.7, fill = "deepskyblue3") + 
    #theme(axis.text=element_text(size=14)) +
    coord_flip() +
    scale_x_discrete(name = item_label, limits=rev) +
    scale_y_continuous(name = document_label) +
    theme_bw()
  ggsave(file.path(output_folder_level, subfolder_dataset, glue("fig_{tolower(item_label)}.jpg")))
}

################################################################################
# TEXT COLUMNS TRENDS
################################################################################
for (i in rp$categorical_long_reports) {
  if (i %in% available_columns) {
    create_report_and_figure(dataset[[i]], item_label = column_labels[i], document_label = document_label)
  }
}

################################################################################
# YEARLY TRENDS
################################################################################
yearly_trends <- dataset$PY %>% 
  as.numeric %>% 
  table %>% 
  data.frame %>% 
  setNames(c("Year", "Articles"))
yearly_trends
yearly_trends <- yearly_trends[order(yearly_trends$Year, decreasing = TRUE),]
write.csv(yearly_trends, file=file.path(output_folder_level, subfolder_dataset, "data_yearly_trends.csv"), row.names = FALSE)

ggplot(yearly_trends[1:15,], aes(x=Year, y=Articles)) + 
  geom_bar(stat = "identity", width = 0.7, fill = "deepskyblue3") +
  theme_bw()
ggsave(file.path(output_folder_level, subfolder_dataset, "fig_yearly_trends.jpg"))

################################################################################
# CLUSTER SIZE
################################################################################
stats_size <- dataset$X_C %>% 
  as.numeric() %>% 
  table %>% 
  data.frame() %>% 
  setNames(c("Item", "Documents"))
write.csv(stats_size, file=file.path(output_folder_level, subfolder_clusters, "data_cluster_size.csv"), row.names = FALSE)

ggplot(stats_size, aes(x=Cluster, y=Articles)) + 
  geom_bar(stat = "identity", width = 0.7, fill = "deepskyblue3") + 
  theme_bw() +
  coord_flip() +
  scale_x_discrete(limits=rev)
ggsave(file.path(output_folder_level, subfolder_clusters, "fig_cluster_size_h.jpg"))

ggplot(stats_size, aes(x=Cluster, y=Articles)) + 
  geom_bar(stat = "identity", width = 0.7, fill = "deepskyblue3") +
  theme_bw()
ggsave(file.path(output_folder_level, subfolder_clusters, "fig_cluster_size_v.jpg"))


##################################################################
##################################################################
##################################################################
# BOXPLOTS   --> summarize numerical columns 
##################################################################
# Boxplots sorted from lowest to highest
# Based on medians; ties are broken with the mean

# UTILS
#' param value_column: the dataset column with numerical values to summarize (e.g. PY, Z9, sentiment, score)
#' param category_column: the dataset column with single categorical items (e.g. X_C, SO, PY)
#' param value_label: the label to use in the x axis
#' param category_label: the label to use in the y axis
#' return a ggplot
plot_boxplots <- function(dataset, 
                          value_column, 
                          category_column,
                          value_label = value_column,
                          category_label = category_column) {
  
  # Get mean and median for sorting
  compound_mean <- tapply(dataset[[value_column]], dataset[[category_column]], mean, na.rm = TRUE)
  compound_median <- tapply(dataset[[value_column]], dataset[[category_column]], median, na.rm = TRUE)
  
  # Prepare df
  long <- dataset[,c(category_column, value_column)]
  setnames(long, c(category_column, value_column), c('category', 'values'))
  long$category <- as.character(long$category)
  long$category <-factor(long$category, levels = as.character(order(compound_median, compound_mean)))
  
  bp <- ggplot(long, aes(x = category, y = values)) + 
    geom_boxplot(width = 0.7, 
                 fill = "deepskyblue3") + 
    xlab(category_label) + 
    ylab(value_label) 
  #bp + coord_flip()
  K <- length(unique(dataset[[category_column]]))
  if (K > 20) {
    bp <- bp + theme(axis.text.x = element_text(size = 6, angle = 90, vjust = 0.5, hjust=1))
  }
  bp + theme_bw() 
}

for (i in rp$numerical_reports) {
  plot_boxplots(dataset_tmp, value_column = i, category_column = 'X_C', value_label = column_labels[i], category_label = 'Clusters')
  ggsave(file.path(output_folder_level, subfolder_clusters, glue("fig_clusters_{i}_boxplot.jpg")))
}