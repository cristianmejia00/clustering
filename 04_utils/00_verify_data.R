# 20190828
# Check requirements for the file dataset from where will we compute the reports

#################################################################################
# Force to data frame object
dataset <- as.data.frame(dataset)


#################################################################################
# News adjustments

# Remove citation network setting for news, in case we accidentally left it here.
# If we do not remove it it will cause problem creating the heatmap keywords
if (settings$params$type_of_dataset == "news" & exists("cno")) {
  rm(cno)
}


# Faceted news datasets are a special case where documents do not have any sorting metric
# And thus we add 1 to X_E
if (settings$params$type_of_dataset == "news") {
  if (!exists("myDataCorrect")) {
    myDataCorrect <- dataset
    setnames(myDataCorrect, c('SCORE','Score'), c('score','score'), skip_absent = TRUE)
  }
  if (!settings$params$unit_of_analysis %in% c("topics", "topic", "clusters", "cluster")) {
    myDataCorrect$cluster_code <- myDataCorrect$X_C
    myDataCorrect$related_topics <- "" # This can be added with the neighbors of the network
    if ('score' %in% colnames(myDataCorrect)) {
      if (!'Z9' %in% colnames(myDataCorrect)) {
        myDataCorrect$Z9 <- myDataCorrect$score
      } else {
        myDataCorrect$Z9 <- 1
      }
      if (!'X_E' %in% colnames(myDataCorrect)) {
        myDataCorrect$X_E <- myDataCorrect$score
      } else {
        myDataCorrect$X_E <- 1
      }
    } 
  }
}

# Preparation for news
if (settings$params$type_of_dataset == "news") {
  if (all(dataset$UT == myDataCorrect$UT)) {
    print("append cols to dataset")
    if (all((!c("X_C", "cluster_code", "X_E", "related_topics") %in% colnames(dataset))) &
      all(c("X_C", "cluster_code", "X_E", "related_topics") %in% colnames(myDataCorrect))) {
      dataset <- cbind(dataset, myDataCorrect[, c("X_C", "cluster_code", "X_E", "related_topics")])
      dataset$level0 <- dataset$X_C
    } else {
      print("colnames of dataset seems to be OK.")
      dataset$level0 <- dataset$X_C
    }
  } else {
    print("column mismatch between dataset and myDataCorrect")
  }
}

################################################################################
# Change column names
setnames(dataset, c("_N", "_E"), c("X_N", "X_E"), skip_absent = TRUE)

# Available columns at this point
available_columns <- colnames(dataset)

#################################################################################
# Append necessary columns when missing
# Critical
if (!("TI" %in% available_columns)) {
  print("ERROR: NO TITLE")
}
if (!("AB" %in% available_columns)) {
  print("ERROR: NO ABSTRACT")
}
if (!("X_C" %in% available_columns)) {
  print("ERROR: NO CLUSTER")
}

#################################################################################
# Solvable
if (!("X_E" %in% available_columns)) {
  if ("Z9" %in% available_columns) {
    dataset$X_E <- dataset$Z9
  }
}
if (!("PY" %in% available_columns)) {
  dataset$PY <- settings$rp$most_recent_year
}
if (!("DT" %in% available_columns)) {
  dataset$DT <- "Article"
}
if (!("Z9" %in% available_columns)) {
  dataset$Z9 <- 1
}
if (!("X_N" %in% available_columns)) {
  dataset$X_N <- c(1:nrow(dataset))
}
if (!("UT" %in% available_columns)) {
  dataset$UT <- dataset$X_N
}

get_keywords_split <- function(a_column) {
  text<- a_column %>%
    tolower() %>%  # Convert text to lowercase
    removeNumbers() %>%  # Remove numbers
    removePunctuation() %>%  # Remove punctuation
    removeWords(stopwords("english")) %>%  # Remove English stopwords
    stripWhitespace() %>% # Remove extra whitespace
    str_replace_all("\\s+", "; ") 
  return(text)
}

if (!("DE" %in% available_columns)) {
  dataset$DE <- get_keywords_split(dataset$TI)
}
if (!("ID" %in% available_columns)) {
  dataset$ID <- get_keywords_split(dataset$TI)
}

#################################################################################
# Optional
if (!("WC" %in% available_columns)) {
  print("warning: no WC")
}
if (!("AU" %in% available_columns)) {
  print("warning: no AU")
}
if (!("DI" %in% available_columns)) {
  print("warning: no DI")
}
if (!("SO" %in% available_columns)) {
  print("warning: no SO")
}
if (!("C1" %in% available_columns)) {
  print("warning: no C1")
}

#################################################################################
# Fix future years
# Near the end of the year we have papers having a PY on the next year. 
# Because they are already accepted and available online, but for printing and official date
# They will be published next year.

# From bibliometrics standpoint, these papers are already there. Hence we treat papers with a future
# year as if they were published this year. 
this_year <- format(Sys.Date(), "%Y") %>% as.numeric()
dataset$PY[is.na(dataset$PY)] <- this_year
future_year_papers <- sum(dataset$PY > this_year)
if (future_year_papers > 0) {
  print(glue('we found {future_year_papers} that will be published next year, and we treat them as published this year.'))
  dataset$PY[dataset$PY > this_year] <- this_year
}

##########################################################################
# Add columns

# Load utils
source("04_utils/zz_auxiliary_functions.R")

# Get countries column for news (In Factiva this is the RE regions column)
if (settings$params$type_of_dataset == "news") {
  if ("C1" %in% available_columns) {
    dataset$Countries <- dataset$C1
  }
}

# # Add Country column (Not needed) for papers and patents
# if (!("Country" %in% available_columns)) {
#   if ("RP" %in% available_columns) {
#     dataset$Country <- getCountry(dataset)
#     print("Country column has been added")
#   } else {
#     dataset$Country <- "NA"
#   }
# }

# Add Countries column
if (!("Countries") %in% available_columns) {
  if ("C1" %in% available_columns) {
    dataset$Countries <- getCountries(dataset$C1)
    dataset$IsoCountries <- as.character(getIsoCountries(dataset$Countries))
    dataset$IsoCountries <- gsub("NA; |; NA$", "", dataset$IsoCountries)
    dataset$IsoCountries <- gsub("; NA", "", dataset$IsoCountries)
    print("Countries column has been added")
  }
}

# Add institutions column
if (!("Institutions") %in% available_columns) {
  if (settings$params$type_of_dataset == "news") {
    if ("ID" %in% available_columns) {
      dataset$Institutions <- as.character(getInstitutions(dataset$ID))
      dataset$Institutions <- gsub("NA", "", dataset$Institutions)
    }
  }
  if (settings$params$type_of_dataset == "papers") {
    if ("C1" %in% available_columns) {
      dataset$Institutions <- as.character(getInstitutions(dataset$C1))
    }
  }
}

##########################################################################
# Format classes
dataset$X_N <- as.numeric(as.character(dataset$X_N))
dataset$X_C <- as.numeric(as.character(dataset$X_C))
dataset$PY <- as.numeric(as.character(dataset$PY))
dataset$X_E <- as.numeric(dataset$X_E)
dataset$Z9 <- as.numeric(dataset$Z9)


##########################################################################
# Clean abstract
dataset$AB <- remove_copyright_statements(dataset$AB)
dataset$AB <- remove_word_counts_line(dataset$AB)


dataset <- dataset[, !duplicated(colnames(dataset))]
if (settings$params$type_of_dataset == "news") {
  myDataCorrect <- dataset
}
