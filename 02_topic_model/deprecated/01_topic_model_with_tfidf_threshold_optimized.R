#########################################
#########################################
dataset <- as.data.frame(dataset)
#########################################

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

# Function to clean text.
# Input: A character string, or a character vector (e.g. a column of text)
# Output: The same input but cleaned. (No numbers, no punctuation, stopwords removed, lowercased)
# Dependencies: library(tm)
# Params: rm_punt. Loguc. "Remove punctuation"
#         useSteaming. Logic.
#         myStopwords. A Character vector with my choosen stopwords
get_tidy_text <- function(a_char_vector, rm_punct = TRUE, useStemming = TRUE, myStopWords = c()) {
  text <- Corpus(VectorSource(a_char_vector))
  text <- tm_map(text, content_transformer(tolower))
  text <- tm_map(text, removeWords, stopwords("english"))
  text <- tm_map(text, removeWords, myStopWords)
  if (rm_punct) {
    text <- tm_map(text, removePunctuation)
  }
  text <- tm_map(text, removeNumbers)
  text <- tm_map(text, stripWhitespace)
  text <- tm_map(text, stripWhitespace)
  if (useStemming) {
    text <- tm_map(text, stemDocument, language = "english")
  }
  text <- tm_map(text, removeWords, myStopWords)
  text <- unlist(sapply(1:length(text), function(x) {
    return(text[[x]]$content)
  }))
  return(text)
}

#########################################
#########################################
# Functions for improved text mining
#########################################
# Function to remove the copyright from abstracts.
# Input: A char string or vector. Ususally the abstracts from WOS
# Output: The imput without the copyritgh statements.
# Dependencies: None.
remove_copyright_statements <- function(a_text) {
  return(gsub(" [A-z0-9 ]*?\\(C\\).*$", "", a_text))
}


# Simple Keyword extractor by spliting sentences in stopwords and punctuation.
# Input: A character vector
# Output: A character vector with the keywords parsed (They will be in lowercase)
# Dependencies: Library(tm)
get_keywords_by_stopword_method <- function(a_text) {
  temp <- strsplit(tolower(a_text), " ")[[1]]
  sw_idx <- temp %in% stopwords("english")
  temp[sw_idx] <- "---"
  temp <- paste(temp, collapse = " ")
  temp <- strsplit(temp, "--- | --- | ---|- | - | -|\\.|,|;|:|\\(|\\)|!||#|$|%|\\&|\\'|\\*|\\+|/|<|=|>|\\?|@\\[|\\|\\]|^|_|`|~|\\{|\\}|\\|")
  temp <- sapply(temp, trimws)
  temp <- sapply(temp, trimws)
  temp <- temp[temp != ""]
  return(temp)
}


#########################################
#########################################
# Functions for LDA package
#########################################
# Get the TFIDF for all keywords based on the compete dataset
# Inputs: The result from tidyCorpus(), and the minimum count of document per term (default to 5)
# Outputs: A character vector. Is the sorted list of keywords with their normalized (0.01 to 1) score.
#          First keywords on the list are the most generic
get_TDIDF <- function(a_tidyCorpus, frequency_thr = 5) {
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
  tfidf <- tfidf %>%
    log() %>%
    linMap(., 0.001, 1) %>%
    sort()
  plot(tfidf, xlab = "number of keywords", ylab = "threshold")
  print(tfidf[1:30])
  return(tfidf)
}

# Function to get the difinitive list of keywords to use on the topic model.
# By removing a list of stopwords based on the tfidf threshold
# Note: English and custom stopwords, and infrequent terms were removed in previous steps.
# INPUTs: the result from get_TFIDF(), and a tfidf threshold that can be any of the following values:
#         "average" : The default option. Removes terms below tfidf average
#         "firstq": Removes values below the 1st quartile (Removes less than the other option)
#          0.001 to 1 : any number in that range. Keywords with a tfidf below that number are removed
# OUTPUT: A list with 2 lists:
#           The vocabulary to use in the topic model
#           The tfidf stopwords
get_vocabulary <- function(a_tfidf, a_tfidf_thr = "average") {
  print("Distribution of TFIDF values")
  boxplot(a_tfidf)
  if (a_tfidf_thr == "average") {
    thr <- mean(a_tfidf)
  }
  if (a_tfidf_thr == "firstq") {
    thr <- summary(a_tfidf)[[2]]
  }
  if (is.numeric(a_tfidf_thr)) {
    thr <- a_tfidf_thr
  }

  vocab <- a_tfidf[a_tfidf >= thr] %>%
    sort(., decreasing = TRUE) %>%
    names()
  stopwords <- a_tfidf[a_tfidf < thr] %>% names()

  return(list("vocabulary" = vocab, "tfidf_stopwords" = stopwords))
}

# Put the documents into the format required by the lda package:
get_terms <- function(x) {
  index <- match(x, vocab)
  index <- index[!is.na(index)]
  rbind(as.integer(index - 1), as.integer(rep(1, length(index))))
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


#########################################
#########################################
# Clean and generate the text to be used in the topic model
#########################################
# We are using title, abstract, DE, and ID keywords only.
# To use Abstract keywords we need to merge them with the title.
# In the case of using abstracs, is advised to remove copyright statements

# Append Title and Abstract keywords
title_text <- tolower(iconv(myDataCorrect$TI, "UTF-8", "UTF-8", sub = ""))
ab_text <- remove_copyright_statements(tolower(iconv(myDataCorrect$AB, "UTF-8", "UTF-8", sub = "")))

title_keywords <- paste(title_text, ab_text, sep = ". ") %>% tolower()
title_keywords <- lapply(title_keywords, get_keywords_by_stopword_method)
title_keywords <- sapply(title_keywords, function(x) {
  return(paste(x, collapse = "; "))
})

# Create the vector containing the keywords for each paper. By combining DE and ID
all_keywords <- paste(myDataCorrect$DE, sep = "; ") %>% tolower()
all_keywords <- paste(all_keywords, title_keywords, sep = "; ")

# Custom corrections: Remove hyphens and replace abbreviations.
all_keywords <- gsub("[[:digit:]]", "", all_keywords)
all_keywords <- gsub("\\'", "", all_keywords)
all_keywords <- gsub('\\"', "", all_keywords)
all_keywords <- gsub("-", " ", all_keywords)
all_keywords <- gsub("   ", " ", all_keywords)
all_keywords <- gsub("  ", " ", all_keywords)
all_keywords <- gsub(" ;", ";", all_keywords)
all_keywords <- gsub("; ce;", "; circular economy;", all_keywords)
all_keywords <- gsub("; lca;", "; life cycle assessment;", all_keywords)
all_keywords <- gsub("; life cycle assessment lca;", "; life cycle assessment;", all_keywords)

##############################################################################################
# Correspondence vectors
# Create the stem-to-normal conversion vector
all_unique_raw_keywords <- strsplit(all_keywords, "; ") %>%
  unlist() %>%
  table()
all_unique_raw_keywords <- data.frame(all_unique_raw_keywords, stringsAsFactors = FALSE)
colnames(all_unique_raw_keywords) <- c("keyword", "counts")


from_raw_to_stem <- get_tidy_text(all_unique_raw_keywords$keyword)
names(from_raw_to_stem) <- all_unique_raw_keywords$keyword

from_stem_to_raw <- all_unique_raw_keywords$keyword[order(all_unique_raw_keywords$counts, decreasing = TRUE)]
from_stem_to_raw <- from_stem_to_raw[!duplicated(from_stem_to_raw)]
names(from_stem_to_raw) <- from_raw_to_stem[from_stem_to_raw]

##############################################################################################
# Tidy vector
# THIS PART IS SLOW!!!
clean_keywords <- strsplit(all_keywords, "; ")
clean_keywords <- lapply(clean_keywords, function(x) {
  temp <- c(x)
  # temp <- unique(temp) #This line no needed for topic model. We need repeated values in each document. We need this line in cluster aggregation only, when each keyword should only appear once per document because they will be aggregated.
  temp <- from_raw_to_stem[temp]
  # Custom correction can also be done here
  # temp <- mesh_conversion_table$mesh_root_stem[match(temp, mesh_conversion_table$mesh_syn_stem)]
  temp <- temp[!is.na(temp)]
  temp <- paste(temp, collapse = "; ")
  return(temp)
})
clean_keywords <- unlist(clean_keywords)

# A posteriori custom corrections.
# Repair keywords we found after running all code
clean_keywords <- gsub("life cycl assess lca|assess lca", "life cycl assess", clean_keywords)
clean_keywords <- gsub("; lca;", "; life cycl assess;", clean_keywords)
clean_keywords <- gsub("^lca;", "life cycl assess;", clean_keywords)
clean_keywords <- gsub("; lca$", "; life cycl assess", clean_keywords)

# Preparation steps needed for topic model map
papersText <- clean_keywords
papersText <- gsub("; ", "wxwxw", papersText)
papersText <- gsub(" ", "zqzq", papersText)
papersText <- gsub("wxwxw", " ", papersText)
papersText <- gsub(";", "", papersText)
papersText <- gsub("NA ", "", papersText)


#########################################
#########################################
# Execute
#########################################
# Get the clean text to feed the topic model
myCorpusText <- tidyText(papersText,
  useStemming = FALSE,
  myStopWords = myStopWords
)
myText <- corpusToText(myCorpusText)


# Obtain the documents that are not blank
blankLines <- unname(sapply(myText, nchar))
myDataCorrect <- dataset[blankLines > 2, ]
myText <- myText[blankLines > 2]

###################################################
# Preparation
# From http://cpsievert.github.io/LDAvis/reviews/reviews.html

# tokenize on space and output as a list:
doc.list <- strsplit(myText, "[[:space:]]+")
doc.list <- lapply(doc.list, function(x) x[which(nchar(x) > 1)]) # Ensure we remove white spaces

# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# Remove words that appear less or equal to:
infrequent_terms_threshold <- 5

# Compute tfidf
tf.idf <- get_TDIDF(myCorpusText, infrequent_terms_threshold)

# a threshold to remove corpus specific stopwords based on tfidf score.
# valid range: from 0.001 to 1, or "average" or "firstq"
tfidf_thresold <- "firstq"

# Get the vocabulary based on a tfidf threshold
custom_keywords <- get_vocabulary(tf.idf, tfidf_thresold)
vocab <- custom_keywords[[1]]
print("Top keywords in Vocab")
gsub("zqzq", " ", vocab[1:100])
tfidf_stopwords <- custom_keywords[[2]]
print("Top keywords in tfidf_stopwords")
gsub("zqzq", " ", tfidf_stopwords[1:100])

# compute the table of terms:
term.table <- table(unlist(doc.list))
term.table <- sort(term.table, decreasing = TRUE)
term.table <- term.table[vocab]

# LDA formatted documents
documents <- lapply(doc.list, get_terms)

######################################################
# model fitting

# Compute some statistics related to the data set:
D <- length(documents) # number of documents
W <- length(vocab) # number of terms in the vocab
doc.length <- sapply(documents, function(x) sum(x[2, ])) # number of tokens per document
N <- sum(doc.length) # total number of tokens in the data
term.frequency <- as.integer(term.table) # frequencies of terms in the corpus
