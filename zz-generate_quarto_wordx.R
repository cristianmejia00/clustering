# 20230717

# Create a Quarto publishable document

# Libraries
library(glue)
library(quarto)
# require(devtools)
#install.packages('rsvg-convert')
# install_version("knitr", version = "1.42", repos = "http://cran.us.r-project.org")

# Inputs
# analysis_metadata <- analysis_metadata
# params <- params

bibliometrics_folder <- settings$analysis_metadata$bibliometrics_folder
project_folder <- settings$analysis_metadata$project_folder
analysis_folder <- settings$analysis_metadata$analysis_folder
level_folder <- "level0"



# # Initialization
# main_path
main_path <- file.path(
  bibliometrics_folder,
  project_folder,
  analysis_folder,
  level_folder
)

# Utils
glue_code <- function(text) {
  glue(text, .open = "<<", .close = ">>", .literal = FALSE, .comment = "##")
}

###################################
###################################
qt <- list()

###################################
###################################
# Article metadata
###################################
document_title <- settings$analysis_metadata$project_name %>% toTitleCase()

qt$yml <- glue('---
title: "{document_title}"
author: Cristian Mejia
date: {Sys.Date()}
bibliography: index_files/bibliography.bib
format:
  docx:
    toc: true
    toc-title: Contents
    toc-depth: 4
editor: visual
always_allow_html: yes
---')

###################################
###################################
# Article initialization
###################################
qt$load_data <- glue_code('
```{r, message=FALSE}
#| echo: false

library(dplyr)
library(data.table)
library(DT)

source("settings.R")

bibliometrics_folder <- settings$analysis_metadata$bibliometrics_folder
project_folder <- settings$analysis_metadata$project_folder
analysis_folder <- settings$analysis_metadata$analysis_folder

main_path <- file.path(bibliometrics_folder,
                       project_folder,
                       analysis_folder,
                       "<<level_folder>>")

rcs_merged <- read.csv(file.path(main_path, "rcs_merged.csv"))
```
')
qt$load_data

###################################
###################################
# Data
###################################
if ((!settings$analysis_metadata$fukan_url %in% c("", "NA")) | is.na(settings$analysis_metadata$fukan_url)) {
  fukan_url <- glue("[Link]({settings$analysis_metadata$fukan_url})")
} else {
  fukan_url <- ""
}
qt$data <- glue("
| Query           | {settings$analysis_metadata$query}         |
|-----------------|-----------------------------------|
| Database        | {toupper(settings$params$dataset_source)}  |
| Documents       | {settings$analysis_metadata$downloaded_documents}|
| Date retrieved  | {settings$analysis_metadata$date}          |
| Fukan Analysis  | {fukan_url}                       |
| ID              | {settings$analysis_metadata$project_name}  |
: Metadata
")

###################################
###################################
# Methods
###################################


###################################
###################################
# Results
###################################

# INTRO


# First figure
# - Figure
# - caption



# Cluster table
qt$results_table <- '
```{r}
#| echo: false
tmp <- rcs_merged[c("cluster_name", "documents", "documents_percent", "PY_Median", "PY_Mean", "Z9_Median", "Z9_Mean", "rcs_label")]
if (all(TRUE)) {
  tmp$cluster_name <- sapply(rcs_merged$frequent_keywords, function(x) {
    k <- strsplit(x, "; ") %>% unlist()
    k <- k[c(1:min(5, length(k)))]
    k <- paste(k, collapse = "; ")
  })
  tmp$cluster_name[nrow(tmp)] <- "Others"
}
tmp$PY_Mean <- round(tmp$PY_Mean, 1)
tmp$Z9_Mean <- round(tmp$Z9_Mean, 1)
setnames(tmp,
         c("cluster_name", "documents", "documents_percent", "PY_Median", "PY_Mean", "Z9_Median", "Z9_Mean", "rcs_label"),
         c("Cluster", "Documents", "Documents %", "Year Median", "Year Mean", "Cites Median", "Cites Mean", "Label"))
tmp$Cluster <- NULL
datatable(tmp)
```
'
# Write cluster descriptions and paper summaries
list_of_clusters <- dataset$X_C %>%
  unique() %>%
  sort()

char_size <- nchar(as.character(max(list_of_clusters)))

# Charts of cluster 99 are saved as x, where x is the main_clusters + 1
# If there are 27 clusters + 99, then 99 is the cluster 28.
list_of_clusters_edited <- list_of_clusters
list_of_clusters_edited[length(list_of_clusters_edited)] <- length(list_of_clusters_edited)
cluster_chart_panel <- list()

for (cluster in list_of_clusters_edited) {
  cluster_chart_panel[[cluster]] <- glue_code('
  

::: {#fig-panel-<<cluster>> layout-ncol=3}

![](index_files/charts/by_clusters/fig_<<str_pad(cluster, char_size, "left", "0")>>_DE.svg)

![](index_files/charts/by_clusters/fig_<<str_pad(cluster, char_size, "left", "0")>>_WC.svg)

![](index_files/charts/by_clusters/fig_<<str_pad(cluster, char_size, "left", "0")>>_SO.svg)

![](index_files/charts/by_clusters/fig_<<str_pad(cluster, char_size, "left", "0")>>_Countries.svg)

![](index_files/charts/by_clusters/fig_<<str_pad(cluster, char_size, "left", "0")>>_Institutions.svg)

![](index_files/charts/by_clusters/fig_<<str_pad(cluster, char_size, "left", "0")>>_AU.svg)  

Cluster <<cluster>> stats.
:::

')
  
}

qt$clusters <- ""
for (cluster in list_of_clusters) {
  cluster_main_description <- glue("
  #### Cluster {cluster}: {rcs_merged$cluster_name[rcs_merged$cluster_code == cluster]}
  {rcs_merged$description[rcs_merged$cluster_code == cluster]}
  ")
  cluster_data <- subset(dataset_bibliography, X_C == cluster)
  cluster_papers_description <- list()
  for (i in c(1:nrow(cluster_data))) {
    cluster_papers_description[[i]] <- glue("{cluster_data$summary[i]} [@{cluster_data$citation_key[i]}]  `degree: {cluster_data$X_E[i]}` `citations: {cluster_data$Z9[i]}`  ", .literal = TRUE)
  }
  cluster_papers_description <- paste(cluster_papers_description, collapse = "\n")
  qt$clusters <- glue("{qt$clusters}
                      {cluster_main_description}
                      {cluster_chart_panel[[min(length(list_of_clusters), cluster)]]}
                      **Articles:**
                      {cluster_papers_description}
                      ---

                      ")
}


qt$figures <- glue_code("::: {#fig-elephants layout-ncol=2}

![Surus](index_files/images/fig_clusters_year_x_cites.jpg){#fig-surus}

![Hanno](index_files/images/fig_clusters_year_x_size.jpg){#fig-hanno}

![Surus](index_files/images/fig_clusters_PY_boxplot.jpg){#fig-surus}

![Hanno](index_files/images/fig_clusters_Z9_boxplot.jpg){#fig-hanno}

Famous Elephants

:::

")

###################################
###################################
# DOCUMENT
###################################
quarto_document <- glue('
{qt$yml}
{qt$load_data}
## Data and Methods

### Data
{qt$data}

### Methods

## Results
### Overview
{qt$figures}


### Clusters
![](index_files/images/network.png)
{figure_caption$choices[[1]]$message$content}
{qt$results_table}

{qt$clusters}

')


if (nchar(quarto_document) == 0) {
  stop('The generated quarto ducument is blank.')
}


# Write the file
fileConn <- file(file.path(output_folder_level, "dockx.qmd"))
writeLines(quarto_document, fileConn)
close(fileConn)

# Render the file to HTML
# settings.R and bibliography.bib must be on the same directory as index.qmd
quarto_render(file.path(output_folder_level, "index.qmd"))


# Render the file as PDF
# write.csv(rcs_merged, file = 'rcs_merged.csv', row.names = FALSE)

# Upload files to repo for online viewing
