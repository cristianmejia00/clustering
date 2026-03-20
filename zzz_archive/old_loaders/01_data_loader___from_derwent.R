#==================================================
#Created in R; 2016-03-14; Kajikawa-lab; C.Mejia
#Working fine as of 2018-11-26.
#==================================================

# Call necessary libraries
library(plyr)
library(data.table)
library(dplyr)
library(stringr)
library(tm)


###########################################################################################
# OPTIONS
###########################################################################################
# Open a window to select the directory with the files to merge
dir_path = "../.."
paths_to_files = list.files(path = file.path(dir_path, "test"), full.names= TRUE, pattern = "*.csv", recursive = TRUE)
paths_to_files = paths_to_files[grepl('.csv$', paths_to_files)]

###########################################################################################
## Path to `/inputs`

dir.create(file.path(dir_path, "test", 'converted_to_wos'), showWarnings = TRUE)


# Read each file and store them in a vector
# fread sometimes fails when reading the header, what to do?
list_of_all_files <- lapply(paths_to_files, function(a_path){
  data1 <- readr::read_csv(a_path, skip = 1)
  return(data1)})

# Verify than the files have the expected number of rows: 500. Except for a few that were the tails.
plot(unlist(sapply(list_of_all_files, nrow))) #The number of rows in each file, mostly 500.

# Create the merged dataset
dataset <- rbind.fill(list_of_all_files)
dataset <- as.data.frame(dataset)
dataset$X_N <- c(1:nrow(dataset))

# #######################################################################
# #######################################################################
# #######################################################################
# Conversion to WOS format

# The length of the IPC to use, either 3 or 4.
IPC_digits <- 4

# ##########################################
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

# Change the contents to desired formats
# IPC of 4 digits
dataset$IPC_full <- dataset$`IPC - Current - DWPI` # Back it up
dataset$`IPC - Current - DWPI` <- IPC_list(dataset$`IPC - Current - DWPI`, IPC_digits)

# Change the pipe separator to semicolon as in WOS
piped_columns <- c(
  "IPC_full",
  "IPC - Current - DWPI",
  "Assignee/Applicant", 
  "Assignee - Current US",         
  "Optimized Assignee", 
  "Ultimate Parent",
  "Cited Refs - Patent",
  "DWPI Family Members"
)

# Convert to ; for what is available
available_piped_columns <- piped_columns[piped_columns %in% colnames(dataset)]
for (ii in available_piped_columns){
  dataset[,ii] <- gsub(" \\| ", "; ", dataset[,ii]) %>% as.character()
}

# Change names to equivalents
new_names <- c(
  UT =  "Publication Number",
  TI =  "Title - DWPI",
  AB = "Abstract - DWPI",
  WC = "IPC - Current - DWPI",
  Country = "Publication Country Code",
  SO = "Ultimate Parent", 
  PY = "Publication Year",
  Z9 = "Count of Citing Patents - DPCI",
  AU = "Assignee/Applicant",
  CR = "Cited Refs - Patent"
)

dataset <- dataset %>% rename(all_of(new_names))

# Add document identifier (for citation network)
dataset$DI <- dataset$UT


# Solve kind code
process_kind_code <- function(df, column = 'CR') {
  # Create a copy of the dataframe to avoid modifying the original
  result_df <- df
  
  # Process each element in the CR column
  result_df[[column]] <- sapply(df[[column]], function(text) {
    # Split the string by "; "
    split_values <- strsplit(text, "; ")[[1]]
    
    # Process each part by removing ending patterns
    processed_values <- sapply(split_values, function(value) {
      # Pattern 1: Ends with one uppercase character followed by an integer
      # Pattern 2: Ends with one uppercase character
      processed <- gsub("([A-Z]\\d+)$", "", value)  # Pattern 1
      processed <- gsub("([A-Z])$", "", processed)  # Pattern 2
      
      return(processed)
    })
    
    # Rejoin the processed parts
    paste(processed_values, collapse = "; ")
  })
  
  return(result_df)
}

dataset <- process_kind_code(dataset, 'CR')
dataset <- process_kind_code(dataset, 'DI')

# Prepare keywords
# Remove stopwords, numbers, and symbols from the 'text' column
dataset$DE <- dataset$TI %>%
  tolower() %>%  # Convert text to lowercase
  removeNumbers() %>%  # Remove numbers
  removePunctuation() %>%  # Remove punctuation
  removeWords(stopwords("english")) %>%  # Remove English stopwords
  stripWhitespace() %>% # Remove extra whitespace
  str_replace_all("\\s+", "; ") # Replace whitespace with a semicolon in the 'text_cleaned' column

# custom edits:
# Remove keywords tending to appear in patent text that are irrelevant
dataset$DE <- gsub(' eg;', '', dataset$DE) # from "e.g."


#########################
# Format

# Correct records without PY
table(dataset$PY)
table(is.na(dataset$PY))

# Corrections to Z9
dataset$Z9[is.na(dataset$Z9)] <- 0


# Remove duplicated files
dataset = dataset %>% filter(!duplicated(UT))

#################################################
# Save and FINISH!
#################################################
write.table(dataset, 
          sep = '\t',
          row.names = FALSE,
          file.path(dir_path, "test", 'converted_to_wos', "dataset.txt"))
