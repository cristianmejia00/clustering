# 20220915

library("plotly")
library("reshape2")
library("ggplot2")
library("ggrepel")
if (exists("topic_names")) {
  rm(topic_names)
}


#################################################################
# When using clusters or topic models we write the topic names at a later stage.
# We can add the names here, so the reports and charts show those names. 
# Load the topic_names file here
# 
# topic_names


# There are 3 options:
# If cluster/topic analysis and we have not named the clusters. Use the cluster number
# If cluster/topic analysis and we named the clusters.Load a file `topic_names.csv` with 2 columns of X_C and X_C_name
# If facet analysis use the facet names. In this case we expect the `dataset` to have a X_C_name column since the begining

if (params$unit_of_analysis %in% c("topic", "topics", "cluster", "clusters") &
    !exists("topic_names")) {
      print("Unnamed clusters. Attaching topic/cluster based on cluster number")
      dataset$X_C_name <- as.character(dataset$X_C)
      rcs$X_C_name <- as.character(rcs$cluster)
}

if (params$unit_of_analysis %in% c("topic", "topics", "cluster", "clusters") &
    exists("topic_names")) {
      print("Attaching topic/cluster names from file")
      dataset$X_C_name <- topic_names$X_C_name[match(dataset$X_C, topic_names$X_C)]
      rcs$X_C_name <- topic_names$X_C_name[match(dataset$X_C, topic_names$X_C)]
    }

if (!(params$unit_of_analysis %in% c("topic", "topics", "cluster", "clusters"))) {
  print("Attaching facet names")
  topic_names <- dataset[!duplicated(dataset$X_C),c("X_C", "X_C_name")]
  rcs$X_C_name <- topic_names$X_C_name[match(rcs$cluster, topic_names$X_C)]
}

#################################################################
# In case of sub-clusters we may want to have the main cluster in the RCS as well
main_cluster <- gsub("---|-0", "", rcs$cluster_code)
main_cluster <- strsplit(main_cluster, "-")
main_cluster <- sapply(main_cluster, function(x) {x[[1]]})
rcs$main_cluster <- factor(main_cluster, levels = as.numeric(main_cluster) %>% unique() %>% sort() %>% as.character())

#################################################################
# Inputs
rcs_tmp <- rcs
dataset_tmp <- dataset

# Facet subsetting. 
# When facet analysis, the list of facets is expected to be long. e.g. firms
# Hence, I plot facets mentioned in at least 10 news
if (!(params$unit_of_analysis %in% c("topic", "topics", "cluster", "clusters"))) {
  facets_tmp <- rcs$X_C_name[rcs$cluster_size >= 10]
  dataset_tmp <- dataset[dataset$X_C_name %in% facets_tmp,]
  rcs_tmp <- rcs[rcs$X_C_name %in% facets_tmp,]
}

#################################################################
# DYNAMIC PLOT Ave. Year x Size 
t <- list(
  #family = "arial",
  size = 12,
  color = toRGB("black"))

p <- plot_ly(rcs_tmp, 
             x = rcs_tmp$cluster_year, 
             y = rcs_tmp$sum_cites, 
             mode = "markers", 
             type = "scatter",
             
             #marker = list(color = '#000000'),
             text = gsub("---|-0", "", rcs_tmp$cluster_code),#~cluster,
             
             #symbol =  as.character(plot_dataset$large_cluster),
             #symbols = c('x-thin-open','triangle-up', 'circle',"circle-open",'square-dot')
             
             color = main_cluster,
             #colors = c('#0000ff', '#e5e500', '#00ff00', '#ff0000', '#e9e9e9' )
             
             size = as.numeric(rcs_tmp$cluster_size)
) %>%
  add_text(textfont = t, textposition = "top left") %>% 
  layout(xaxis = list(title = "Average Publication Year"), 
         yaxis = list(title="Cummulative citations", tickformat = ",d"), # range = c(0,8000),
         showlegend = FALSE) 
p

#################################################################
# STATIC PLOTS


################################################################# 
# Ave. Year x Cites STATIC PLOTS
p <- ggplot(rcs_tmp, aes(x=cluster_year, y=sum_cites)) + geom_point() + xlab("Average Publication Year") + ylab("Sum Citations")
#p + geom_text(aes(label=gsub("---|-0", "", cluster_code)))
p <- p + geom_point(aes(color=main_cluster, size=sum_cites))
p <- p + geom_text_repel(aes(label=gsub("---|-0", "", cluster_code)))
p <- p + theme(legend.position="none")
p
ggsave(file.path(output_folder_level, "fig_clusters_year_x_cites.jpg"))


#################################################################
# Ave. Year x Size STATIC PLOTS
p <- ggplot(rcs_tmp, aes(x=cluster_year, y=cluster_size)) + geom_point() + xlab("Average Publication Year") + ylab("Articles")
#p + geom_text(aes(label=gsub("---|-0", "", cluster_code)))
p <- p + geom_point(aes(color=main_cluster, size=cluster_size))
p <- p + geom_text_repel(aes(label=gsub("---|-0", "", cluster_code)))
p <- p + theme(legend.position="none")
p
ggsave(file.path(output_folder_level, "fig_clusters_year_x_size.jpg"))


##################################################################
# Year boxplots Plots:
##################################################################
# Boxplots sorted from the most recent to oldest
# Based on medians; ties are broken with the mean
PY_compound_mean <- tapply(dataset_tmp$PY, dataset_tmp$X_C, mean, na.rm=TRUE)
PY_compound_median <- tapply(dataset_tmp$PY, dataset_tmp$X_C, median, na.rm = TRUE)

# Plots
PY_long <- dataset_tmp[,c("X_C", "PY")]
PY_long$X_C <- as.character(PY_long$X_C)
PY_long$X_C <-factor(PY_long$X_C, levels = as.character(order(PY_compound_median, PY_compound_mean)))

bp <- ggplot(PY_long, aes(x = X_C, y = PY)) + geom_boxplot(width = 0.7, fill = "deepskyblue3") + xlab("Cluster") + ylab("Ave. Year") 
#bp + coord_flip()
K <- length(unique(dataset_tmp$X_C))
if (K > 20) {
  bp <- bp + theme(axis.text.x = element_text(size = 6, angle = 90, vjust = 0.5, hjust=1))
}
bp
ggsave(file.path(output_folder_level, "fig_clusters_PY_boxplot.jpg"))

