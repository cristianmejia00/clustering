# 20190828
# Check requirements for the file myDataCorrectReport from where will we compute the reports

# Any file, passing through this code will have the following columns in the following format

# NEEDED VALUES
# is news, articles, patents, crunchbase, other
# myDataCorrectReport
myDataCorrect <- dataset

source("03_reports/zz_auxiliary_functions.R")

# Helper function to get urls from DOI
DOI_to_URL <- function(a_list_of_DOI) {
  sapply(a_list_of_DOI, function (x) {
    if (nchar(x) > 0) {
      paste('https://doi.org/', x, sep = '')
    } else {x}
  })
}

myDataCorrectReport <- myDataCorrect#[myDataCorrect$UT %in% selected_articles$ID,]
myDataCorrectReport$Country <- getCountry(myDataCorrectReport)


# Find which colnames exist
columns_in_myDataCorrectReport <- intersect(c("X_C", "cluster_code",
                                        "topic", "related_topics",
                                        "AU", "PY", "DI", "TI", "AB", "Z9", "X_E", "DE", "SO", "VL", "IS", "WC", "Country", "UT", "title_match"),
                                      colnames(myDataCorrectReport))

# Create the file
article_report <- myDataCorrectReport[,columns_in_myDataCorrectReport]

## Format columns
# Format DOI
if ("DI" %in% colnames(article_report)) {article_report$DI <- DOI_to_URL(article_report$DI)}

# Replace NA by empty character
article_report[is.na(article_report)] <- ""

# Change names depending of the type of dataset
change_names <- c("AU",       "PY",   "DI",  "TI",       "AB", "Z9",                 "X_E",     "DE",              "SO",        "VL",  "IS",    "WC",         "UT", "Field", "ABS", "Category")
paper_names  <- c("Author",   "Year", "DOI", "Title",    "Abstract", "Citations received", "Degree",  "Author keywords", "Journal",   "Vol", "Issue", "Categories", "ID", "ABS_Field", "ABS_RANK", "WOS_Category")
setnames(article_report, change_names, paper_names,  skip_absent = TRUE)

# Write the article report
article_report$Year <- as.numeric(article_report$Year)
article_report$ID <- as.character(article_report$ID)
write.csv(article_report, file="subsistance_mkt_25.csv", row.names = FALSE)


