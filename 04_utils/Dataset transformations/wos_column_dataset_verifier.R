library(plyr)
library(dplyr)

# Read a dataset
dataset <- read.csv(file.choose(), header = TRUE, check.names = FALSE, stringsAsFactors = FALSE)

################################################################
################################################################
# WOS format conversion for any type of data.
# This code helps creating a dataset that contains the necesary objects for creating reports based on a 
# dataset having a clustering solution.
# This code assumes that the clustering or any king of grouping was performed already in the data.

# Usage examples:
# 1.-
# Analysinig a completely new dataset from a new provider. e.g. a dataset of company information from Orbis.
# We can transform the company name, to titles. And the business decription as abstracts, etc.
# Also we could treat each company as its own clustering, or group companies according to agrouping system in the dataset. 
# In the case of Orbis using the GUO field or the category field.

# 2.-
# Using datasets we know, but we want reports based on an arbitrary clustering or grouping.
# Assume we have a patent dataset from Derwent. If we want the reports based on IPC we first assign each patent to a unique IPC
# And then treat the IPCs as clusters (By assigning a distict numeric value to each IPC).
# OR analyzing by year. Using years as clusters. Or authors, etc.

# This can also be used as an entry point for Topic Model, even before computing the topics.
# In such case, just rename "myDataCorrect" as "dataset" at the end of the code.

################################################################
################################################################
# Format guidelines:
# TI, AB, DT, UT, SO, J9: Character. A single string of text.
# WC, DE, ID, AU: Character. A single string of text where multiple values can be listed, separated by a SEMICOLON. No NAs or blanks allowed.
# X_N, X_E, Z9, PY, cluster_code: Numeric values. Remove any NA or convert it to zero.
# X_C: Numeric integer. The cluster or grouping ID. Cluster must be named from 1 to n. No number should be skip. No NAs or zeroes allowed.
# C1, RP: Character. A single string of text.Just used for compatibility. Neglect this.

write.csv(levels(as.factor(myDataCorrect[,10])), file = "legend.csv")

################################################################
################################################################
# Force to data frame object
myDataCorrect <- as.data.frame(dataset)

# If possible, remove unnecesary rows at this point.

# fill necesary or optional columns that need trasnlation from the format to WOS format
myDataCorrect$TI <- dataset$`Company name`
myDataCorrect$AB <- paste(dataset[,41],dataset[,45],dataset[,47],dataset[,48],dataset[,50],dataset[,52],dataset[,54],sep=". ")
myDataCorrect$AB <- dataset[dataset[,10]!="",48]
myDataCorrect$X_C <- as.integer(as.factor(myDataCorrect[,10])) #as.numeric(myDataCorrect_SAMPLE$X_C)
myDataCorrect$X_E <- as.numeric(gsub(",", "",dataset[,7]))
myDataCorrect$PY <- as.numeric(dataset[,6])
myDataCorrect$Country <- dataset[,3]
myDataCorrect$WC <- dataset[,52]
myDataCorrect$WC[myDataCorrect$WC == ""] <- "no_category"
# Both cluster_code and Z9 can be considered a repetition of X_C and X_E.
myDataCorrect$Z9 <- myDataCorrect$X_E
myDataCorrect$cluster_code <- myDataCorrect$X_C

################################################################
################################################################
# No need to change anything from here. 
# Available columns at this point
available_columns <- colnames(myDataCorrect)

# Append necesary columns when missing
# Critical
if (!("TI" %in% available_columns))  {print("ERROR: NO TITLE")}
if (!("AB" %in% available_columns))  {print("ERROR: NO ABSTRACT")}
if (!("X_C" %in% available_columns)) {print("ERROR: NO CLUSTER")}
if (!("X_E" %in% available_columns)) {print("ERROR: NO INDICATOR X_E")}
if (!("cluster_code" %in% available_columns)) {print("ERROR: NO CLUSTER CODE")}

# Solvable
if (!("PY" %in% available_columns))  {myDataCorrect$PY  <- 2000}
if (!("DT" %in% available_columns))  {myDataCorrect$DT  <- "Article"}
if (!("Z9" %in% available_columns))  {myDataCorrect$Z9  <- 1}
if (!("X_N" %in% available_columns)) {myDataCorrect$X_N <- c(1:nrow(myDataCorrect))}
if (!("UT" %in% available_columns))  {myDataCorrect$UT  <- myDataCorrect$X_N}
if (!("DE" %in% available_columns))  {myDataCorrect$DE  <- myDataCorrect$TI}
if (!("ID" %in% available_columns))  {myDataCorrect$ID  <- myDataCorrect$TI}

# Optional
if (!("WC" %in% available_columns)) {print("warning: no WC")}
if (!("AU" %in% available_columns)) {print("warning: no AU")}
if (!("DI" %in% available_columns)) {print("warning: no DI")}
if (!("SO" %in% available_columns)) {print("warning: no SO")}
if (!("J9" %in% available_columns)) {print("warning: no J9")}
if (!("C1" %in% available_columns)) {print("warning: no C1")}
if (!("RP" %in% available_columns)) {print("warning: no RP")}

# Get country column
if (!("Country" %in% available_columns)) {
  source("03_reports/zz_auxiliary_functions.R")
  if ("RP" %in% available_columns) {myDataCorrect$Country <- getCountry(myDataCorrect)}
  else {myDataCorrect$Country <- "NA"}
}

# Format classes
myDataCorrect$X_N <- as.numeric(myDataCorrect$X_N)
myDataCorrect$X_C <- as.numeric(myDataCorrect$X_C)
myDataCorrect$X_E <- as.numeric(myDataCorrect$X_E)
myDataCorrect$Z9  <- as.numeric(myDataCorrect$Z9)
myDataCorrect$PY  <- as.numeric(myDataCorrect$PY)

#############################
# To be used as entry dataset to compute Topic Model from scratch?
# dataset <- myDataCorrect
# dataset$X_C <- NULL
# dataset$X_E <- NULL