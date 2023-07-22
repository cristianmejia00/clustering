# 20190828
# Check requirements for the file myDataCorrect from where will we compute the reports

# Any file, passing through this code will have the following columns in the following format

# # INPUTS
# myDataCorrect
# settings$rp$top_documents
# settings$rp$column_labels
# rn$PROJECTarticlereport


print("###################### reports/01_document_report_with_abstract.R")

# Helper function to get urls from DOI
convert_doi_to_url <- function(a_list_of_DOI) {
  a_list_of_DOI[is.na(a_list_of_DOI)] <- ""
  sapply(a_list_of_DOI, function(x) {
    if (nchar(x) > 0) {
      paste("https://doi.org/", x, sep = "")
    } else {
      x
    }
  })
}


# Find which colnames exist.
# This define which columns and in which order they will appear in the article report
columns_in_myDataCorrect <- intersect(
  c(
    "X_C", "cluster_code",
    "topic", "related_topics", "TD",
    "AU", "PY", "DI", "TI", "AB", "Z9", "X_E", "DE", "SO", "WC", "Countries", "sentiment", "sentiment_factor", "Page_Rank", "Eigen", "Closeness", "Betweenness", "UT"
  ),
  colnames(myDataCorrect)
)

# Create the file
article_report <- myDataCorrect[, columns_in_myDataCorrect]

## Format columns
# Format DOI
if ("DI" %in% colnames(article_report)) {
  article_report$DI <- convert_doi_to_url(article_report$DI)
}

# Replace NA by empty character
article_report[is.na(article_report)] <- ""

# Filter to the top_documents of each cluster
if (settings$rp$top_documents != 0) {
  article_report <- article_report %>%
    group_by(X_C) %>%
    top_n(settings$rp$top_documents, X_E)
}

# Change colnames to natural names
setnames(article_report, 
         names(settings$rp$column_labels), 
         unname(settings$rp$column_labels), 
         skip_absent = TRUE)


# Write the article report
write.csv(article_report, 
          file = rn$PROJECTarticlereport, 
          row.names = FALSE)

# Cleaning up
rm('columns_in_myDataCorrect')