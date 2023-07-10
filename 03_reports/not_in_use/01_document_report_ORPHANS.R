# 20190828
# Check requirements for the file myDataCorrect from where will we compute the reports

# Any file, passing through this code will have the following columns in the following format

# NEEDED VALUES
# is news, articles, patents, crunchbase, other
# myDataCorrect

# Helper function to get urls from DOI
DOI_to_URL <- function(a_list_of_DOI) {
  sapply(a_list_of_DOI, function (x) {
    if (nchar(x) > 0) {
      paste('https://doi.org/', x, sep = '')
    } else {x}
  })
}


# Find which colnames exist
columns_in_myDataCorrect <- intersect(c("X_C", "cluster_code",
                                        "topic", "related_topics",
                                        "AU", "PY", "DI", "TI", "AB", "Z9", "X_E", "DE", "SO", "VL", "IS", "WC", "Country", "UT"),
                                      colnames(myDataCorrect))

# Create the file
article_report <- all_dataset[!all_dataset$MC & all_dataset$criteria, intersect(colnames(all_dataset), columns_in_myDataCorrect)]

## Format columns
# Format DOI
if ("DI" %in% colnames(article_report)) {article_report$DI <- DOI_to_URL(article_report$DI)}

# Replace NA by empty character
article_report[is.na(article_report)] <- ""

# Change names depending of the type of dataset
change_names <- c("AU",       "PY",   "DI",  "TI",       "AB", "Z9",                 "X_E",     "DE",              "SO",        "VL",  "IS",    "WC",         "UT")
paper_names  <- c("Author",   "Year", "DOI", "Title",    "Abstract", "Citations received", "Degree",  "Author keywords", "Journal",   "Vol", "Issue", "Categories", "ID")
patent_names <- c("Inventor", "Year", "URL", "Title",    "Citations received", "Degree",  "Keywords",        "Assignee",  "Vol", "Issue", "IPC",        "Patent Number")
news_names   <- c("Author",   "Year", "URL", "Headline", "Score1",             "Score2",  "Keywords",        "Newspaper", "Vol", "Issue", "Catgeories", "ID")


if (type_of_dataset == "papers")  {setnames(article_report, change_names, paper_names,  skip_absent = TRUE)}
if (type_of_dataset == "patents") {setnames(article_report, change_names, patent_names, skip_absent = TRUE)}
if (type_of_dataset == "news")    {setnames(article_report, change_names, news_names,   skip_absent = TRUE)}

# Filter to the top 15 of each cluster
#article_report <- article_report %>% group_by(X_C) %>% top_n(15, Degree)

# Write the article report
write.csv(article_report, file="article_report_orphans.csv", row.names = FALSE)
