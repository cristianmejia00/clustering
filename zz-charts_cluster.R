# Charts for clustering analysis

# Libraries
library(glue)
library(data.table)
library(ggplot2)
library(gridExtra)
library(dplyr)


# Data Directory
#dd <- choose.dir()
dd <- "C:\\Users\\crist\\OneDrive\\Documentos\\03-bibliometrics\\Q259-palm-oil-enhanced\\Q259-palm-oil-2-topics\\001"
dd <- "C:\\Users\\crist\\OneDrive\\Documentos\\03-bibliometrics\\Q260-rice\\Q260-rice-topics\\001"

# Load datasets
r_keywords <- read.csv(file.path(dd, 'keywords_report.csv'))
setnames(r_keywords, c("term", "freq", "cluster", "type", "normalized"), c("Term", "Freq", "Cluster", "Type", "Normalized"))

r_countries <- read.csv(file.path(dd, 'report_Countries.csv'))
r_authors <- read.csv(file.path(dd, 'report_AU.csv'))
r_journals <- read.csv(file.path(dd, 'report_SO.csv'))

# Check why this are not appearing
r_institutions <- read.csv(file.path(dd, 'report_Institutions.csv'))
r_categories <- read.csv(file.path(dd, 'report_WC.csv'))


# Cluster-level
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
  p <- ggplot(cluster_data, 
              aes(x=.data[[names(cluster_data)[1]]], y=Freq)) + 
              geom_bar(stat = "identity", width = 0.7, fill = "deepskyblue3") + 
              scale_y_continuous(limits=c(0, max(plot_data[[2]]))) +
              coord_flip() +
              theme_bw()
  return(p)
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