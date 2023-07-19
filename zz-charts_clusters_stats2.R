# 20230719
# Cluster-level charts
# Bar charts for the cluster sizes
# All bar charts per cluster for rp$categorical_long_reports:
# "AU", "WC", "SO", "Countries", "Institutions", "DE", "sentiment_factor",
# "PY.

# INPUTS
output_folder_level <- output_folder_level
subfolder_clusters <- "charts_clusters"



################################################################################
# Create output folder
dir.create(file.path(output_folder_level, subfolder_clusters))
dir.create(file.path(output_folder_level, subfolder_clusters, 'by_clusters'))
dir.create(file.path(output_folder_level, subfolder_clusters, 'by_columns'))


# Libraries
library(glue)
library(data.table)
library(ggplot2)
library(gridExtra)
library(dplyr)
library(stringr)


# Load datasets in settings$rp$categorical_long_reports
charts_datasets <- list()
for (i in settings$rp$categorical_long_reports) {
  report_name <- glue('report_{i}.csv')
  report_path <- file.path(output_folder_level, report_name)
  print(report_path)
  if (file.exists(report_path)) {
    charts_datasets[[i]] <- read.csv(report_path)
  }
}

# Load keywords
report_path <- file.path(output_folder_level, "report_keyword.csv")
if (file.exists(report_path)) {
  tmp <- read.csv(report_path)
  setnames(tmp, c("term", "freq", "cluster", "type", "normalized"), c("Term", "Freq", "Cluster", "Type", "Normalized"))
  charts_datasets[['Keywords']] <- tmp
}

# Utils
#' @description
#' Compute the number of digits of integer.
#' This is code taken from StackOverflow and is faster than the nchar(as.character(x))
#' @param x INTEGER. a positive integer
get_digits <- function(x){
  floor(log10(x)) + 1
}


#' @description
#' Generate a ggplot bar plot for cluster's categorical columns
#' @param plot_data DATAFRAME. a long report for a given column
#' @param cluster_number INTEGER. the cluster number
#' @returns a bar plot ggplot
plot_cluster_data <- function(plot_data, cluster_number) {
  plot_data <- subset(plot_data, Cluster != 0)
  cluster_data <- subset(plot_data, Cluster == cluster_number)
  cluster_data[[1]] <- tolower(cluster_data[[1]]) %>% 
                       substr(start = 0, stop = 20)
  cluster_data[[1]][duplicated(cluster_data[[1]])] <- paste(cluster_data[[1]][duplicated(cluster_data[[1]])], ' ', sep = '')
  cluster_levels <- cluster_data[[1]][order(cluster_data[[2]])]
  cluster_labels <- sapply(cluster_levels, 
                           function(x) {
                             if (nchar(x) >= 20) {
                               paste(x, '...', sep = '')
                              } else {
                                x
                              }})
  cluster_data[[1]] <- factor(cluster_data[[1]], 
                              levels = cluster_levels,
                              labels = cluster_labels)
  p <- ggplot(cluster_data[c(1:20),], 
              aes(x=.data[[names(cluster_data)[1]]], y=Freq)) + 
              geom_bar(stat = "identity", width = 0.7, fill = "deepskyblue3") + 
              scale_y_continuous(limits=c(0, max(plot_data[[2]]))) + 
              #scale_x_discrete(labels = function(x) formatC(x, width = 5)) +
              coord_flip() +
              theme_bw()

  return(p)
}



# Plot and save
available_charts <- names(charts_datasets)
for (i in available_charts) {
  tmp <- charts_datasets[[i]]
  clusters_n <- unique(tmp$Cluster) %>% sort()
  clusters_n <- clusters_n[clusters_n != 0]
  char_size <- get_digits(max(clusters_n))
  for (j in clusters_n) {
    plot_cluster_data(tmp, j)
    
    # by columns
    ggsave(filename = file.path(output_folder_level,
                                subfolder_clusters,
                                'by_columns',
                                glue('fig_{i}_{str_pad(j, char_size, "left", "0")}.png')), 
           width = 1000, 
           height = 1000, 
           units = 'px')
    
    # by cluster
    ggsave(filename = file.path(output_folder_level,
                                subfolder_clusters,
                                'by_clusters',
                                glue('fig_{str_pad(j, char_size, "left", "0")}_{i}.png')), 
           width = 1000, 
           height = 1000, 
           units = 'px')
  }
}




my_dfs <- list(r_countries, r_institutions, r_journals, r_authors)
chart_rows <- 5
chart_columns <- length(my_dfs)
my_charts <- list()
for (cl in c(1:chart_rows)) {
  for (df in my_dfs) {
    print(colnames(df))
    plot_cluster_data(df,cl)
    my_charts <- append(my_charts, list(plot_cluster_data(df,cl)))
  }
}


do.call("grid.arrange", c(my_charts, ncol=4))
ggsave(filename = 'cl_plots.pdf', width = 1000, height = 1500, units = 'px')

test <- do.call("arrangeGrob", c(my_charts, ncol=4))
ggsave(filename = 'cl_plots.pdf', width = 300, height = 600, units = 'px', test)

# Print plots to a pdf file
pdf("ggplot.pdf", paper = 'a4')
print(do.call("grid.arrange", c(my_charts, ncol=4)))     # Plot 1 --> in the first page of PDF
#print(myplot2)     # Plot 2 ---> in the second page of the PDF
dev.off() 