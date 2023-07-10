# 20210513

# Big Fila refers to create a consolidated dataset based on other independent datasets.
# For instance, we downloaded from WOS 3 distinct datasets of climate change, surgery, and AI.
# And we want to get summary statistics of each, and statistis on the aggregations
# To use our current codes, we just need to treat each dataset as if they belong to a cluster.

# In this code, we aggregate the datasets and assign "fake" cluster numbers to them.

#############################################################################################################
# For this purpose we can still use the cluster analysis code, but we need to format the datasets properly,
# This formatting includes, 
# * assign each dataset to a cluster
# * bind the dataset
# * rename the object and add missing columns

# After finishing this code we can run the "01_execute_and_reports.R"


# Load files
load("C:\\Users\\crist\\OneDrive\\Documentos\\Transfer from Google Drive\\45 - Frontiers Bibliometrics\\00-Data\\bibliometrics.rdata")
load("C:\\Users\\crist\\OneDrive\\Documentos\\Transfer from Google Drive\\45 - Frontiers Bibliometrics\\00-Data\\scientometrics.rdata")
load("C:\\Users\\crist\\OneDrive\\Documentos\\Transfer from Google Drive\\45 - Frontiers Bibliometrics\\00-Data\\informetrics.rdata")
load("C:\\Users\\crist\\OneDrive\\Documentos\\Transfer from Google Drive\\45 - Frontiers Bibliometrics\\00-Data\\altmetrics.rdata")
load("C:\\Users\\crist\\OneDrive\\Documentos\\Transfer from Google Drive\\45 - Frontiers Bibliometrics\\00-Data\\webometrics.rdata")
load("C:\\Users\\crist\\OneDrive\\Documentos\\Transfer from Google Drive\\45 - Frontiers Bibliometrics\\00-Data\\cybermetrics.rdata")

# Assign each dataset to a cluster number and  bind them as a single "big file"
bibliometrics$X_C <- 1
scientometrics$X_C <- 2
informetrics$X_C <- 3
altmetrics$X_C <- 4
webometrics$X_C <- 5
cybermetrics$X_C <- 6

dataset <- rbind.fill(bibliometrics, scientometrics, informetrics, altmetrics, webometrics, cybermetrics)
class(dataset)

# Add missing columns
dataset$X_E <- dataset$Z9
dataset$X_N <- c(1:nrow(dataset))
dataset$UT <- dataset$X_N
dataset$cluster_code <- dataset$X_C

#Rename the object
myDataCorrect <- dataset

################################################################
################################################################
# No need to change anything from here. 
# Available columns at this point
available_columns <- colnames(myDataCorrect)

# Append necessary columns when missing
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
  #if ("RP" %in% available_columns) {myDataCorrect$Country <- getCountries(myDataCorrect)}
  if ("C1" %in% available_columns) {myDataCorrect$Country <- getCountries(myDataCorrect$C1)}
  else {myDataCorrect$Country <- "NA"}
}

# Format classes
myDataCorrect$X_N <- as.numeric(myDataCorrect$X_N)
myDataCorrect$X_C <- as.numeric(myDataCorrect$X_C)
myDataCorrect$X_E <- as.numeric(myDataCorrect$X_E)
myDataCorrect$Z9  <- as.numeric(myDataCorrect$Z9)
myDataCorrect$PY  <- as.numeric(myDataCorrect$PY)
