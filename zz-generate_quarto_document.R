# 20230717

# Create a Quarto publishable document

# Libraries
library(glue)
library(quarto)
# require(devtools)
# install_version("knitr", version = "1.42", repos = "http://cran.us.r-project.org")

# Inputs
analysis_metadata <- analysis_metadata
params <- params

bibliometrics_folder <- settings$analysis_metadata$input_folder
dataset_folder <- settings$analysis_metadata$query_id
analysis_folder <- settings$analysis_metadata$project_id
level_folder <- "level0"



# # Initialization
main_path <- file.path(
  bibliometrics_folder,
  dataset_folder,
  analysis_folder,
  level_folder
)
# main_path
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
document_title <- glue("{settings$params$type_of_analysis} of {settings$analysis_metadata$theme} {settings$params$type_of_dataset}") %>% toTitleCase()

qt$yml <- glue('---
title: "{document_title}"
author: Cristian Mejia
date: {Sys.Date()}
bibliography: bibliography.bib
format:
  html:
    toc: true
    toc-title: Contents
    toc-depth: 4
editor: visual
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

bibliometrics_folder <- settings$analysis_metadata$input_folder
dataset_folder <- settings$analysis_metadata$query_id
analysis_folder <- settings$analysis_metadata$project_id

main_path <- file.path(bibliometrics_folder,
                       dataset_folder,
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
qt$clusters <- ""
for (cluster in list_of_clusters) {
  cluster_main_description <- glue("
  #### Cluster {cluster}: {rcs_merged$cluster_name[rcs_merged$cluster_code == cluster]}
  {rcs_merged$description[rcs_merged$cluster_code == cluster]}
  ")
  cluster_data <- subset(dataset_bibliography, X_C == cluster)
  cluster_papers_description <- list()
  for (i in c(1:nrow(cluster_data))) {
    print(cluster_data$X_E[i])
    cluster_papers_description[[i]] <- glue("{cluster_data$summary[i]} [@{cluster_data$citation_key[i]}]  `degree: {cluster_data$X_E[i]}` `citations: {cluster_data$Z9[i]}`  ", .literal = TRUE)
  }
  cluster_papers_description <- paste(cluster_papers_description, collapse = "\n")
  qt$clusters <- glue("{qt$clusters}
                      {cluster_main_description}
                      **Articles:**
                      {cluster_papers_description}
                      ---

                      ")
}
qt$clusters


qt$figures <- glue_code("::: {#fig-elephants layout-ncol=2}

![Surus](index_files/images/fig_clusters_year_x_cites.jpg){#fig-surus}

![Hanno](index_files/images/fig_clusters_year_x_size.jpg){#fig-hanno}

![Surus](index_files/images/fig_clusters_PY_boxplot.jpg){#fig-surus}

![Hanno](index_files/images/fig_clusters_Z9_boxplot.jpg){#fig-hanno}

Famous Elephants

:::

")
qt$figures
###################################
###################################
# DOCUMENT
###################################

quarto_document <- glue("
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

")


# Write the file
fileConn <- file("index.qmd")
writeLines(quarto_document, fileConn)
close(fileConn)

# Render the file to HTML
quarto_render("index.qmd")


# Render the file as PDF
# write.csv(rcs_merged, file = 'rcs_merged.csv', row.names = FALSE)

# Upload lifes to repo for online viewing
