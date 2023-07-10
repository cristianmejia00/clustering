# 20181122
# With the purpose of using patent datasets from Derwent in the codes created for WOS
# Instead of creating custom code for patents, I change the patent dataset to resemble WOS format
# So will it only be needed to use this formated dataset.

# 20181208
# Updates related to use patent data to the topic explorer.
# In particular, differenctiate when using this code before or after any processing.

#########################################################################
# Functions to transform a Derwent dataset to the WOS format
# So we can use it in any code created for WOS
library(plyr)
library(dplyr)
#########################################################################
# Parameters
# Write "before_analysis" If this dataset has just been downloaded from Derwent and is yet to be processed by citation network or topic model
# write "after_analysis"  If this dataset already finished citation network or topic model.
dataset_status <- "before_analysis"

# The length of the IPC to use, either 3 or 4.
IPC_digits <- 4

# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# After this, just source the remaining code.

# #######################################################################
# Helper function to obtain cut IPC code
# x is the vector of IPC in text format
# digits is the size of the cut
# unique_ipc = TRUE: Get all unique instances of IPC found in the patent, sorted from the most frequent
#              FALSE: Just let repeated values there.  
IPC_list <-  function(IPC_column, digits = 4, unique_ipc = TRUE) { 
  members <- strsplit(IPC_column, split =" \\| ")
  members <- lapply(members, function(x) x[x!=""])
  members <- lapply(members, unique)
  members <- lapply(members, function(x) substring(x, 1, digits))
  if (unique_ipc) {
    members <- lapply(members, function(x) {
      temp <- table(x) %>% 
        sort(., decreasing = TRUE) %>% 
        names
      return(temp)
    })
  }
  return (lapply(members, function(x) {paste(x, collapse = " | ")}))}


#if (dataset_status == "after_analysis") {
#  dataset <- myDataCorrect
#}

# Change the contents to desired formats
# IPC of 4 digits
dataset$`IPC - Current - DWPI` <- IPC_list(dataset$`IPC - Current - DWPI`, IPC_digits)

# Append other columns, which header might be necessary at some point.
# Keyword fundtions use 2 columns DE and ID. Patents only have one, so we append blank "ID"
dataset$ID <- ""

# Blank DT is necessary
dataset$DT <- ""
dataset$`Citation Counts Citing - DPCI`
# To emulate Fukan columns we append these too
if (! "_D" %in% names(dataset)) {dataset$"_D" <- as.numeric(dataset$`Count of Citing Patents`)}
if (! "_E" %in% names(dataset)) {dataset$"_E" <- as.numeric(dataset$`Count of Citing Patents`)}
#if (dataset_status == "before_analysis") { 
#  dataset$"_C" <- 9999 
#  dataset$"_N" <- c(1:nrow(dataset))}


# Let only usable columns only when the dataset has not been proceesed
if (dataset_status == "before_analysis") {
  dataset <- dataset[, c("_N",
                         "_C",
                         "_D",
                         "_E",
                         "ID",
                         "DT",
                         "Publication Number",
                         "Title - DWPI",
                         "Abstract - DWPI",
                         "IPC - Current - DWPI",
                         "Publication Country Code",
                         "Assignee/Applicant First",
                         "Publication Year",
                         "Count of Citing Patents",
                         "Inventor - DWPI",
                         "Title Terms - DWPI")]
}


# Change the separators from " | " to semicolons "; "
dataset$`IPC - Current - DWPI` <- gsub(" \\| ", "; ", dataset$`IPC - Current - DWPI`) %>% as.character()
dataset$`Inventor - DWPI` <- gsub(" \\| ", "; ", dataset$`Inventor - DWPI`)
dataset$`Title Terms - DWPI` <- gsub(" ", "; ", dataset$`Title Terms - DWPI`)

# Change names to equivalents
setnames(dataset, c("Publication Number",
                    "Title - DWPI",
                    "Abstract - DWPI",
                    "IPC - Current - DWPI",
                    "Publication Country Code",
                    "Assignee/Applicant First",
                    "Publication Year",
                    "Count of Citing Patents",
                    "Inventor - DWPI",
                    "Title Terms - DWPI"),
                   c("UT",
                     "TI",
                     "AB",
                     "WC",
                     "Country",
                     "J9",
                     "PY",
                     "Z9",
                     "AU",
                     "DE"
                   ))


