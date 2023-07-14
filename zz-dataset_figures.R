# 2023/07/13
# Dataset-level stats
# All bar charts in this dataset.


# Load plotting library
library(ggplot2)
library(glue)

# INPUT
dataset = dataset
rp <- data.frame("top_items" = 20)
document_label <- toTitleCase(params$type_of_dataset)
output_folder_reports <- output_folder_reports
subfolder <- 'dataset_stats'
available_columns <- available_columns

# SYSTEM
dir.create(file.path(output_folder_reports, subfolder)) 

# UTILS
create_report_and_figure <- function(column_data,
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
            file = file.path(output_folder_reports, subfolder, glue("dataset_{tolower(item_label)}.csv")), 
            row.names = FALSE)
  
  
  ggplot(stats_column[c(1:top_items),], aes(x=Item, y=Documents)) + 
    geom_bar(stat = "identity", width = 0.7, fill = "deepskyblue3") + 
    #theme(axis.text=element_text(size=14)) +
    coord_flip() +
    scale_x_discrete(name = item_label, limits=rev) +
    scale_y_continuous(name = document_label) +
    theme_bw()
  ggsave(file.path(output_folder_reports, subfolder, glue("fig_{tolower(item_label)}.jpg")))
}

################################################################################
# TEXT COLUMNS TRENDS
################################################################################
column_labels = c('Countries' = 'Countries',
                 'SO' = 'Journals',
                 'Institutions' = 'Institutions',
                 'AU' = 'Authors',
                 'WC' = 'Categories',
                 'DE' = 'Author Keywords',
                 'sentiment_factor' = 'Sentiment')

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
write.csv(yearly_trends, file=file.path(output_folder_reports, subfolder, "data_yearly_trends.csv"), row.names = FALSE)

ggplot(yearly_trends[1:15,], aes(x=Year, y=Articles)) + 
  geom_bar(stat = "identity", width = 0.7, fill = "deepskyblue3") +
  theme_bw()
ggsave(file.path(output_folder_reports, subfolder, "fig_yearly_trends.jpg"))

################################################################################
# CLUSTER SIZE
################################################################################
stats_size <- dataset$X_C %>% 
  as.numeric() %>% 
  table %>% 
  data.frame() %>% 
  setNames(c("Item", "Documents"))
write.csv(stats_size, file=file.path(output_folder_reports, subfolder, "data_cluster_size.csv"), row.names = FALSE)

ggplot(stats_size, aes(x=Cluster, y=Articles)) + 
  geom_bar(stat = "identity", width = 0.7, fill = "deepskyblue3") + 
  theme_bw() +
  coord_flip() +
  scale_x_discrete(limits=rev)
ggsave(file.path(output_folder_reports, subfolder, "fig_cluster_size_h.jpg"))

ggplot(stats_size, aes(x=Cluster, y=Articles)) + 
  geom_bar(stat = "identity", width = 0.7, fill = "deepskyblue3") +
  theme_bw()
ggsave(file.path(output_folder_reports, subfolder, "fig_cluster_size_v.jpg"))