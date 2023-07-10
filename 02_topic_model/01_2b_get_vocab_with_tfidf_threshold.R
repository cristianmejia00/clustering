# 20210812 Last revised

###########################################################
###########################################################
# tfidf threshold: We are computing the `vocab` based on a tfidf threshold.
# We remove stopwords (from the cleaning stage)
# We remove terms appearing ion less than 5 documents
infrequent_terms_threshold <- 5

# We remove terms that overly appear in the corpus
# For instance, we expect that the terms we used in the query will appear in almost every document
# So we do not need them. Is obvious they are there and only slow down the computation of the topic model
# We remove terms that appear in 95% of documents of more
overly_frequent_terms_threshold <- 0.95


# Retain any term above a threshold
tfidf_thresold <- "average"
# Options:
#         "average" : The default option. Removes terms below tfidf average
#         "firstq": Removes values below the 1st quartile
#          0.001 to 1 : any number in that range. Keywords with a tfidf below that number are removed
###########################################################
###########################################################

# This is working fine.
# I tried to implement the tokenization by stopwords approach. However it did not work. 
# The tokenization by stopwords needs to be conducted in the complete dataset which too slow. 
# We can use it for exploration of cluster in the reports because is an approximation base of a sample dataset, and hence, it does not takes long.
# Additionally, the stopword approach created too many tokens (near 2million for the news dataset) which is problematic for the topic model.
# where we need to stay in the range of less than 50k tokens.
# In conclusion let the code as is.

# 20170523 Topic Models
# Topic Model 1/3
# Model and data preparation
# We prepare the text that is going to be used in the model. We also choose key parameters.


# The implementation in this code helps to filter out irrelevant keywords.
# I would consider there are 2 kind of irrelevant keywords
# The head and tails keywords.
# In general, if we simply take the frequency of a keyword, then.
# Head means those that are very frequent
# Tail means all keywords that barely appear. 
# In each case we have to choose the parameter (e.g. frequency) and the thresholds.

# In this code we use tfidf as parameter, and remove all keywords with low tfidf values.
# Hence, we are removing the tail of tfidf. 
# We further have to think if there is any value on removing also the head for tfidf.


# Get the TFIDF for all keywords based on the complete dataset
# Inputs: The result from tidyCorpus(), and the minimum count of document per term (default to 5)
# Outputs: A character vector. Is the sorted list of keywords with their normalized (0.01 to 1) score. 
#          First keywords on the list are the most generic
get_TDIDF <- function(a_tidyCorpus, infrequent_terms_threshold = 5) {
  # Document term frequency
  dtm <- DocumentTermMatrix(a_tidyCorpus, control = list(bounds = list(global = c(infrequent_terms_threshold, Inf))))
  # Document counts
  dtm_counts <- dtm
  dtm_counts$v <- rep(1, length(dtm_counts$v))
  # Get tf idf values
  term.doc_frec <- col_sums(dtm_counts)
  term.freq <- col_sums(dtm) - term.doc_frec + 1
  term.idf <- log10(nrow(dtm) / term.doc_frec)
  tfidf <- term.freq * term.idf
  tfidf <- tfidf %>% log %>% linMap(., 0.001, 1) %>% sort
  plot(tfidf, xlab = "number of keywords", ylab = "threshold")
  #print(tfidf[1:30])
  return(tfidf)
}

# Function to get the definitive list of keywords to use on the topic model.
# By removing a list of stopwords based on the tfidf threshold
# Note: English and custom stopwords, and infrequent terms were removed in previous steps.
# INPUT: the result from get_TFIDF(), and a tfidf threshold that can be any of the following values:
#         "average" : The default option. Removes terms below tfidf average
#         "firstq": Removes values below the 1st quartile
#          0.001 to 1 : any number in that range. Keywords with a tfidf below that number are removed
# OUTPUT: A list with 2 lists: 
#           The vocabulary to use in the topic model
#           The tfidf stopwords
get_vocabulary <-  function(a_tfidf, a_tfidf_thr = "average") {
  if (a_tfidf_thr == "average") {thr <- mean(a_tfidf)}
  if (a_tfidf_thr == "firstq") {thr <- summary(a_tfidf)[[2]]}
  if (is.numeric(a_tfidf_thr)) {thr <- a_tfidf_thr}
  
  vocab <- a_tfidf[a_tfidf >= thr] %>% sort(., decreasing = TRUE) %>% names
  stopwords <- a_tfidf[a_tfidf < thr] %>% names
  
  return(list("vocabulary" = vocab, "tfidf_stopwords" = stopwords))
}



# Mapping function. 
linMap <- function(x, from, to) {
  # Shifting the vector so that min(x) == 0
  x <- x - min(x)
  # Scaling to the range of [0, 1]
  x <- x / max(x)
  # Scaling to the needed amplitude
  x <- x * (to - from)
  # Shifting to the needed level
  x + from
}


get_overly_frequent_terms <- function(a_corpus, a_threshold) {
  tmp <- DocumentTermMatrix(a_corpus, control=list(weighting=function(x) weightBin(x)))
  test <- slam::col_sums(tmp)
  names(test) <- tmp$dimnames$Terms
  test <- sort(test, decreasing = TRUE)
  return(test[test>(nrow(myDataCorrect) * a_threshold)] %>% names())
}

#########################################
# Get the overly frequent terms
overly_frequent_terms <- get_overly_frequent_terms(myCorpusText, overly_frequent_terms_threshold)

# Compute tfidf
tf.idf <- get_TDIDF(myCorpusText, infrequent_terms_threshold)

# Get the vocabulary based on a tfidf threshold
custom_keywords <- get_vocabulary(tf.idf, tfidf_thresold)

# Define Vocabulary to use
vocab <- custom_keywords[[1]]
vocab <- vocab[!(vocab %in% overly_frequent_terms)]
print("tfidf_vocab")
print(vocab[1:100])

# Update the term.table
term.table <- term.table[vocab]

# Check removed terms
# This variable is not used elsewhere
tfidf_stopwords <- custom_keywords[[2]]
print("tfidf_stopwords")
print(tfidf_stopwords[1:100])






