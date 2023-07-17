# 20230717

# Create a Quarto publishable document

# Libraries
library(glue)
library(quarto)

# Inputs
analysis_metadata <- analysis_metadata
params <- params

bibliometrics_folder <- analysis_metadata$input_folder
dataset_folder <- analysis_metadata$query_id
analysis_folder <- analysis_metadata$project_id
level_folder <- 'level0'



# Initialization
main_path <- file.path(bibliometrics_folder,
                       dataset_folder,
                       analysis_folder,
                       level_folder)
main_path
# Utils
glue_code <- function(text) {
  glue(text, .open = '<<', .close = '>>', .literal = FALSE, .comment = '##')
}

###################################
###################################
qt <- list()

###################################
###################################
# Article metadata
###################################
document_title <- glue('{params$type_of_analysis} of {analysis_metadata$theme} {params$type_of_dataset}') %>% toTitleCase()

qt$yml <- glue('---
title: "{document_title}"
author: Cristian Mejia
date: {Sys.Date()}
format: 
  html:
    toc: true
    toc-title: Contents
    toc-depth: 2
editor: visual
---')

###################################
###################################
# Article initialization
###################################
qt$load_data <- glue_code('
```{r}
#| echo: false
library(dplyr)
library(data.table)
library(DT)

source("settings.R")

bibliometrics_folder <- analysis_metadata$input_folder
dataset_folder <- analysis_metadata$query_id
analysis_folder <- analysis_metadata$project_id

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
if ((!analysis_metadata$fukan_url %in% c('', 'NA')) | is.na(analysis_metadata$fukan_url)) {
  fukan_url <- glue('[Link]({analysis_metadata$fukan_url})')
} else {
  fukan_url <- ''
}
qt$data <- glue('
| Query           | {analysis_metadata$query}         |
|-----------------|-----------------------------------|
| Database        | {toupper(params$dataset_source)}  |
| Documents       | {analysis_metadata$downloaded_documents}|
| Date retrieved  | {analysis_metadata$date}          |
| Fukan Analysis  | {fukan_url}                       |
| ID              | {analysis_metadata$project_name}  |
: Metadata
')

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
datatable(tmp)
```
'

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




### Clustering
{figure_caption$choices[[1]]$message$content}
{qt$results_table}

')

# Write the file
fileConn<-file("output.qmd")
writeLines(quarto_document, fileConn)
close(fileConn)

# Render the file to HTML
quarto_render("output.qmd")


# Render the file as PDF
#write.csv(rcs_merged, file = 'rcs_merged.csv', row.names = FALSE)

# Upload lifes to repo for online viewing