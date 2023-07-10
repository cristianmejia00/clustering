# 20180109.
# Helper function for heatmaps

##################################
##################################
# Helper Functions
##################################
##################################
# Agreggate textual contents by clusters. Ideal to merge the documents by clusters.
# documents_vector = the column containing the text to use
# Inputs:
# cluster_assignation_vector = The column having the cluster ID per document
# list_of_cluster = The list of clusters to use
# Outputs:
# A list of type character of size list_of_clusters.
clusterBulkText <- function(documents_vector, cluster_assignation_vector, list_of_clusters) {
  lapply(list_of_clusters, function(i) {
    paste(documents_vector[cluster_assignation_vector == i], collapse = " ")
  })
}


# Transforms text from dataset to a tm corpus
# Inputs:
# content_vector = a column, or character vector containing the text to clean
# useSteamming = Boolean, whether to use porter stemmer
# my_stopwords = A list of stopwords of my choice. like in c("this", "that")
# Outputs:
# A tm corpus object.
tidyText <- function(content_vector, useStemming = TRUE, myStopWords) {
  text <- Corpus(VectorSource(content_vector))
  text <- tm_map(text, content_transformer(tolower))
  text <- tm_map(text, removeWords, stopwords("english"))
  text <- tm_map(text, removeWords, myStopWords)
  text <- tm_map(text, removePunctuation)
  text <- tm_map(text, removeNumbers)
  text <- tm_map(text, stripWhitespace)
  if (useStemming) {
    text <- tm_map(text, stemDocument, language = "english")
  }
  text <- tm_map(text, removeWords, myStopWords)
  return(text)
}

# Transfor back a tm corpus to normal text. Ideal to use after cleaning.
# Inputs:
# a_corpus = a tm corpus (e.g. created with function tidyText)
# Output:
# A character vector with the text re-assembled
corpusToText <- function(a_corpus) {
  text <- unlist(sapply(1:length(a_corpus), function(x) {
    return(a_corpus[[x]]$content)
  }))
  return(text)
}


# Create Document Term Matrix TFIDF
# Input: a tm corpus object
# Output: a tdm object (sparse matrix from the slam package)
tdmTfidf <- function(a_corpus) {
  return(TermDocumentMatrix(a_corpus, control = list(weighting = weightTfIdf)))
}


# Create Document Term Matrix TF
# Input: a tm corpus object
# Output: a tdm object (sparse matrix from the slam package)
tdmTf <- function(a_corpus) {
  return(TermDocumentMatrix(a_corpus, control = list(weighting = weightTf)))
}

# Gets a tdm and creates a list of clusters with its keywords sorted from the most relevant (either TF or Tfidf score)
# Inputs:
# a_tdm = a tdm object (e.g. from functions tdmTfifd, tdmTf, TermDocumentMatrix)
# list_of_clusters = the clusters to use
# a_top_limit = A threshold of the number of keyword to show
# Outputs:
# a list of data frames. The list is of length list_of_clusters. The data frames are of dimmension a_top_limit x 2. (the keyword and its score)
listOfKeywords <- function(a_tdm, list_of_clusters, a_top_limit) {
  lapply(list_of_clusters, function(i) {
    colindexes <- a_tdm$j == i
    keywordindexes <- a_tdm$i[colindexes]
    keywords <- a_tdm$dimnames$Terms[keywordindexes]
    tfidf_score <- a_tdm$v[colindexes]
    table_values <- data.frame(keywords, tfidf_score)
    table_values <- table_values[order(table_values[, 2], decreasing = TRUE)[1:a_top_limit], ]
    return(table_values)
  })
}

##################################
##################################
# Parameters and options
papersText <- paste(myDataCorrect$TI, myDataCorrect$AB, sep = " ")
# papersText <- paste(myDataCorrect$TI, myDataCorrect$DE, myDataCorrect$ID, sep = " ")
cluster_assignation_vector <- myDataCorrect$X_C
list_of_clusters <- myDataCorrect$X_C %>%
  unique() %>%
  sort()
useStemming <- TRUE
a_top_limit <- 100

##################################
##################################
# Execution
# Cluster contents
papersText <- enc2utf8(papersText)


cluster_contents <- papersText %>%
  clusterBulkText(
    cluster_assignation_vector = cluster_assignation_vector,
    list_of_clusters = list_of_clusters
  ) %>%
  tidyText(
    useStemming = useStemming,
    myStopWords = myStopWords
  )


# Cluster Keywords
cluster_keywords_tf <- cluster_contents %>%
  tdmTf() %>%
  listOfKeywords(
    .,
    list_of_clusters,
    a_top_limit
  )

cluster_keywords_tfidf <- cluster_contents %>%
  tdmTfidf() %>%
  listOfKeywords(
    .,
    list_of_clusters,
    a_top_limit
  )

##################################
##################################
# Save as R object
save(cluster_keywords_tf,
  cluster_keywords_tfidf,
  file = rn$PROJECTKeywords
)
