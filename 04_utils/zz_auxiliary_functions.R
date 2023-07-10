# 20161220
# Function to get cluster, and
# Widgets that complement The Knowledge Dashboard

library(data.table)
library(plyr)
library(dplyr)
library(plotly)
library(tools)
library(DT)
library(igraph)

#####################################################################
#####################################################################

# This outputs ONLY 1 country per paper
# Input a DATA FRAME
# Works over column RP
# Parse and get the country name of the corresponding author
getCountry <- function(d) {
  addresses <- strsplit(d$RP, split = ", |,") #Separate string by the comma
  addresses <- lapply(addresses, function(x) {x[length(x)]}) #Get the last value which is the country
  addresses <- sapply(addresses, function(x) gsub("\\.", "", x)) #Remove the dot
  addresses <- tolower(addresses)
  addresses[grep(" usa$|^usa$|^[[:alpha:]][[:alpha:]] |^[[:alpha:]][[:alpha:]]$", addresses)] <- "usa" #Standardize all US addresses
  addresses[grep("england|scotland|wales|north ireland", addresses)] <- "united kingdom"
  # Add the country
  Country <- as.character(addresses)
  Country[which(Country=="character(0)")] <- "-" #Correct the papers with no country info
  
  return(Country)
}

# This outputs MULTIPLE countries per paper
# Inputs a COLUMN (i.e. myDataCorrect$C1)
# Works over column C1
# This function extract the countries participating in the study.
# Countries are not repeated. i.e. A paper with multiple authors from different institutions in the US and 1 author
# affiliated to an institution in the UK will return "usa; england"
getCountries <- function(a_C1_column) {
  countries <- gsub("\\[.*?\\]", "author_list", a_C1_column)
  countries <- strsplit(countries, split = "; ") 
  countries <- sapply(countries, function(ii) {
    tmp <- sapply(ii, function(jj) {
      x <- strsplit(jj, split = ", |,") %>% unlist
      return(x[length(x)])
    }) %>%  
      trimws %>% 
      unique %>% 
      tolower %>% 
      unname
    tmp <- gsub("\\.", "", tmp)
    tmp[grep(" usa$|^usa$|^[[:alpha:]][[:alpha:]] |^[[:alpha:]][[:alpha:]]$", tmp)] <- "usa"
    tmp[grep("england|scotland|wales|north ireland", tmp)] <- "united kingdom"
    tmp <- unique(tmp)
    return(paste(tmp, collapse = "; "))})
  return(countries)
}


# Function that transforms the WOS country names
# to ISO 3166 3-characters code.
# We need this codes to plot in map libraries
# The conversion tables is expected to be a data frame with headers including "ISO3166alpha3" and "WOS" with the corresponding ISO code and country names as appear in the WOS.
# I already created this file and have it here as an asset. 
country_codes <- read.csv(file.path(getwd(), "05_assets", "country_conversion.csv"), header = TRUE, stringsAsFactors = FALSE)
iso_search <- country_codes$ISO3166alpha3
names(iso_search) <- country_codes$WOS
rm(country_codes)
getIsoCountries <- function(a_countries_column, conversion_vector = iso_search) {
  tmp <- strsplit(a_countries_column, split = "; ")
  tmp <- lapply(tmp, function(x){
    paste(conversion_vector[x], collapse = "; ")
  })
  return(as.character(tmp))
}

# This outputs MULTIPLE countries per paper
# Inputs a COLUMN (i.e. myDataCorrect$C1)
# Works over column C1
# This function extract the institutions participating in the study.
# Institutes are not repeated. i.e. A paper with multiple authors from same institution in the US and 1 author
# affiliated to an institution in the UK will return, for example, "univ harvard; univ tokyo"
getInstitutions <- function(a_c1_column) {
  inst <- gsub("\\[.*?\\] ", "", a_c1_column ) %>%
    tolower %>%
    strsplit(split ="; ")
  inst <- lapply(inst, function(ii) {
    lapply(ii, function(jj) {
      x <- strsplit(jj, split = ", ") %>% unlist
      return(x[1])
    }) %>% 
      unlist %>%
      unique %>%
      paste(., collapse = "; ")
  })
  return(inst)
}


########################################################
########################################################
# Widgets

# Get Top Authors
TopAuthors <- function(d, top = 5){
  authors <- 
    tolower(d$AU) %>%
    strsplit(split ="; ") %>% 
    unlist %>% 
    table %>% 
    sort(decreasing = TRUE) %>% 
    .[1:top]
  return(authors)
}

# Top WOS categories (Assigned to journal)
TopCategories <- function(d, top = 5){
  categories <- 
    strsplit(d$WC, split ="; ") %>% 
    unlist %>% 
    table %>% 
    sort(decreasing = TRUE) %>% 
    .[1:top]
  return(categories)
}



# Top Keywords by frequency (Author Keywords + Smart keywords)
TopKeywords <- function(d, top = 10){
  keys <- 
    paste(d$DE, d$ID, sep = "; ") %>%
    tolower %>%
    strsplit(split ="; ") %>% 
    lapply(function(x) x[x!=""]) %>%
    unlist %>% 
    table %>% 
    sort(decreasing = TRUE) %>% 
    .[1:top]
  return(keys)
}


# Get top countries
TopCountries <-  function(d, top = 5){
  Countries <- 
    tolower(d$Country) %>%
    table %>% 
    sort(decreasing = TRUE) %>%
    .[1:top]
  return(Countries)
}

#Top Journal/Conference (Assigned to paper)
TopJournals <- function(d, top = 5){
  Journals <- 
    strsplit(d$J9, split ="; ") %>% 
    unlist %>% 
    table %>% 
    sort(decreasing = TRUE) %>% 
    .[1:top]
  return(Journals)
}



#Top Institutions (Based on all co-authors)
TopInstitutions <- function(d, top = 10){
  Insts <- 
    strsplit(d$institutions, split ="; ") %>% 
    unlist %>% 
    table %>% 
    sort(decreasing = TRUE) %>% 
    .[1:top]
  return(Insts)
}


## Generic
# Auxiliar function to get the top values from a Factiva 
# Works for "NS"("AU") categories "CO"("ID") companies "IN"("DE") industries "RE"("C1") regions. Factiva("WOS")
TopSomething <- function(d, coll = "ID" , top = 5){
  categories <- 
    strsplit(d[[coll]], split ="; ") %>% 
    unlist %>% 
    table %>% 
    sort(decreasing = TRUE) %>% 
    .[1:top]
  return(categories)
}

