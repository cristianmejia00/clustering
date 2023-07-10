# 20220104

# Code to parse references.
# From a list of references provided in word and edited in excel
# We output the list of articles titles

# Input: snowballing_list.csv It is a 1 column document containing a list of references.
# output: list of titles in lowercase. Later in excel we build a query to be used in WOS

# Libraries
library(plyr)
library(dplyr)

# In the right panel, read directly the input file "snowballing_list.csv"

# lowercase
dataset <- as.character(snowballing_list$V1) %>% enc2utf8 %>% tolower()

# commas to periods
dataset <- gsub(",", ".", dataset)

# split on the "200'1.'"  because we know after that there is the title.
dataset <- strsplit(dataset, "[[:digit:]]\\.")

# get the second element of the split (i.e. the title, it also contains the remaining of other metadata)
dataset <- sapply(dataset, function(x) {
  if (length(x) >= 2) {
    tmp <- x[[2]]
  } else {
    tmp <- x[[1]]
  }
})

# remove trailing whitespace
dataset <- trimws(dataset)

# split it on the period
dataset <- strsplit(dataset, "\\.")

# get the first element which is expected to be the title. Other elements may be the journal, DOI, etc. We dont need them
dataset <- sapply(dataset, function(x) {
  if (length(x) >= 1) {
    tmp <- x[[1]]
  } else {
    tmp <- x
  }
})

# Compute the character length. Titles too short indicate a parsing error.
# These will be later assessed manually in Excel.
title_lengths <- sapply(dataset, nchar)

# Create the data frame with the parsed title, length, and raw reference
dataset <- data.frame("title_length" = title_lengths,
                      "title" = dataset,
                      "ref" = snowballing_list$V1)

# Save it.
# In excel, assess small titles and replace them with the correct one. 
# Also, built the query by attaching "OR"  at the end of the name
# And use that query to pull the articles from WOS.
write.csv(dataset, file="snowballing_parse.csv")
getwd()
