# 20220915
# Deprecated code.
# As of 20230714 use zz-charts_scatterplot instead.


# We plot 4 scatterplots
# - years x citations (or score)
# - years x size
# - years x sentiment
# - citations x size
# - citations x sentiment
# - size x sentiment

# This is an scatterplot and thus is preferred to have the actual names of the clusters if possible
# When using clusters or topic models we write the topic names at a later stage.
# We can add the names here, so the reports and charts show those names. 

# There are 3 options:
# If cluster/topic analysis and we have not named the clusters. Use the cluster number
# If cluster/topic analysis and we named the clusters directly in `rcs_merged.csv`
# If facet analysis use the facet names. In this case we expect the `dataset` to have a X_C_name column since the beginning


# Load libraries
library("plotly")
library("reshape2")
library("ggplot2")
library("ggrepel")
if (exists("topic_names")) {
  rm(topic_names)
}

# Global inputs

output_folder_level <- output_folder_level
subfolder <- 'chart_cluster'
column_labels <- column_labels

load(file.path(input_folder, analysis_metadata$query_id, "dataset.rdata"))
rcs <- read.csv(file.path(output_folder_level, 'rcs_merged.csv'))
#column_labels <- read.csv(file.path(output_folder_level, 'column_labels.csv'))

# System
dir.create(file.path(output_folder_level, subfolder))

# Preparation
# Convert the colimn_labels to a named vector




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


##################################################################
# SCATTERPLOT: Ave. Year x Cites
##################################################################
p <- ggplot(rcs_tmp, aes(x=cluster_year, y=ave_cites)) + 
  geom_point() + 
  xlab("Average Publication Year") + 
  ylab("Average Citations")
#p + geom_text(aes(label=gsub("---|-0", "", cluster_code)))
p <- p + geom_point(aes(color=main_cluster, size=ave_cites))
p <- p + geom_text_repel(aes(label=gsub("---|-0", "", cluster_code)))
p <- p + theme_bw() + theme(legend.position="none")
p
ggsave(file.path(output_folder_level, "fig_clusters_year_x_cites.jpg"))


##################################################################
# SCATTERPLOT: Ave. Year x Size
##################################################################
p <- ggplot(rcs_tmp, aes(x=cluster_year, y=cluster_size)) + geom_point() + xlab("Average Publication Year") + ylab("Articles")
#p + geom_text(aes(label=gsub("---|-0", "", cluster_code)))
p <- p + geom_point(aes(color=main_cluster, size=cluster_size))
p <- p + geom_text_repel(aes(label=gsub("---|-0", "", cluster_code)))
p <- p + theme_bw() + theme(legend.position="none")
p
ggsave(file.path(output_folder_level, "fig_clusters_year_x_size.jpg"))


##################################################################
# SCATTERPLOT: Ave. Year x Sentiment
##################################################################
p <- ggplot(rcs_tmp, aes(x=cluster_year, y=cluster_size)) + geom_point() + xlab("Average Publication Year") + ylab("Articles")
#p + geom_text(aes(label=gsub("---|-0", "", cluster_code)))
p <- p + geom_point(aes(color=main_cluster, size=cluster_size))
p <- p + geom_text_repel(aes(label=gsub("---|-0", "", cluster_code)))
p <- p + theme_bw() + theme(legend.position="none")
p
ggsave(file.path(output_folder_level, "fig_clusters_year_x_size.jpg"))

##################################################################
# SCATTERPLOT: Ave. Year x Sentiment !!!
##################################################################
p <- ggplot(rcs_tmp, aes(x=cluster_year, y=cluster_size)) + geom_point() + xlab("Average Publication Year") + ylab("Articles")
#p + geom_text(aes(label=gsub("---|-0", "", cluster_code)))
p <- p + geom_point(aes(color=main_cluster, size=cluster_size))
p <- p + geom_text_repel(aes(label=gsub("---|-0", "", cluster_code)))
p <- p + theme_bw() + theme(legend.position="none")
p
ggsave(file.path(output_folder_level, "fig_clusters_year_x_size.jpg"))

##################################################################
# SCATTERPLOT: Ave. Size x Sentiment !!!
##################################################################
p <- ggplot(rcs_tmp, aes(x=cluster_year, y=cluster_size)) + geom_point() + xlab("Average Publication Year") + ylab("Articles")
#p + geom_text(aes(label=gsub("---|-0", "", cluster_code)))
p <- p + geom_point(aes(color=main_cluster, size=cluster_size))
p <- p + geom_text_repel(aes(label=gsub("---|-0", "", cluster_code)))
p <- p + theme_bw() + theme(legend.position="none")
p
ggsave(file.path(output_folder_level, "fig_clusters_year_x_size.jpg"))
