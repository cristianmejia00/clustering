# 20190828
# Check requirements for the file myDataCorrect from where will we compute the reports

# Any file, passing through this code will have the following columns in the following format

# NEEDED VALUES
# is news, articles, patents, crunchbase, other
# myDataCorrect
print("###################### reports/01_document_report_with_abstract.R")

# Helper function to get urls from DOI
DOI_to_URL <- function(a_list_of_DOI) {
  a_list_of_DOI[is.na(a_list_of_DOI)] <- ""
  sapply(a_list_of_DOI, function (x) {
    if (nchar(x) > 0) {
      paste('https://doi.org/', x, sep = '')
    } else {x}
  })
}


# Find which colnames exist
columns_in_myDataCorrect <- intersect(c("X_C", "cluster_code",
                                        "topic", "related_topics", "TD",
                                        "AU", "PY", "DI", "TI", "AB", "Z9", "X_E", "DE", "SO", "VL", "IS", "WC", "Countries", "sentiment", "Page_Rank", "Eigen", "Closeness", "Betweenness", "UT"),
                                      colnames(myDataCorrect))

# Create the file
article_report <- myDataCorrect[,columns_in_myDataCorrect]

## Format columns
# Format DOI
if ("DI" %in% colnames(article_report)) {article_report$DI <- DOI_to_URL(article_report$DI)}

# Replace NA by empty character
article_report[is.na(article_report)] <- ""

# Filter to the top_documents of each cluster
if (params$type_of_dataset != "news" & rp$top_documents != 0) {
  article_report <- article_report %>% group_by(X_C) %>% top_n(rp$top_documents, X_E)
}

# Change names depending of the type of dataset
change_names <- c("AU",       "PY",   "DI",  "TI",       "AB",                 "Z9",                 "X_E",     "DE",              "SO",        "VL",  "IS",    "WC",         "UT")
paper_names  <- c("Author",   "Year", "DOI", "Title",    "Abstract",           "Citations received", "Degree",  "Author keywords", "Journal",   "Vol", "Issue", "Categories", "ID")
patent_names <- c("Inventor", "Year", "URL", "Title",    "Abstract",           "Citations received", "Degree",  "Keywords",        "Assignee",  "Vol", "Issue", "IPC",        "Patent Number")
news_names   <- c("Author",   "Year", "URL", "Headline", "Lead Paragraph",     "Score1",             "Score2",  "Keywords",        "Newspaper", "Vol", "Issue", "Categeories","ID")

if (params$type_of_dataset == "papers")  {setnames(article_report, change_names, paper_names,  skip_absent = TRUE)}
if (params$type_of_dataset == "patents") {setnames(article_report, change_names, patent_names, skip_absent = TRUE)}
if (params$type_of_dataset == "news")    {setnames(article_report, change_names, news_names,   skip_absent = TRUE)}

# Write the article report
write.csv(article_report, file=rn$PROJECTarticlereport, row.names = FALSE)
