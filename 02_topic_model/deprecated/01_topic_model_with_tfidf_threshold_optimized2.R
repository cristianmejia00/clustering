#########################################
#########################################
dataset <- as.data.frame(dataset)
myDataCorrect$Country <- myDataCorrect$C1
myDataCorrect$DE <- myDataCorrect$WC

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
  if (useStemming) {text <- tm_map(text, stemDocument, language = "english")}
  text <- tm_map(text, removeWords, myStopWords)
  return(text)
}

# Transfor back a tm corpus to normal text. Ideal to use after cleaning.
# Inputs:
# a_corpus = a tm corpus (e.g. created with function tidyText)
# Output:
# A character vector with the text re-assembled
corpusToText <- function(a_corpus){
  text <- unlist(sapply(1:length(a_corpus), function(x){return(a_corpus[[x]]$content)}))
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
  if (rm_punct) {text <- tm_map(text, removePunctuation)}
  text <- tm_map(text, removeNumbers)
  text <- tm_map(text, stripWhitespace)
  text <- tm_map(text, stripWhitespace)
  if (useStemming) {text <- tm_map(text, stemDocument, language = "english")}
  text <- tm_map(text, removeWords, myStopWords)
  text <- unlist(sapply(1:length(text), function(x){return(text[[x]]$content)}))
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

# Patterns for the split
my_pattern <- "--- | --- | ---|- | - | -|\\.|,|;|:|\\(|\\)|!|#|\\$|%|\\&|\\'|\\*|\\+|/|<|=|>|\\?|@|\\[|\\]|\\^|_|`|~|\\{|\\}|\\|"
my_stops_space <- paste(" ", stopwords("english"), " ", sep = "") %>% paste(collapse = "|")
my_stops_starts <- paste("^", stopwords("english"), " ", sep = "") %>% paste(collapse = "|")
my_stops_ends <- paste(" ", stopwords("english"), "$", sep = "") %>% paste(collapse = "|")
my_stops_colon <- paste(" ", stopwords("english"), ";", sep = "") %>% paste(collapse = "|")

# Patterns to be removed
my_specials <- "\\\\\\u|\\\\\\;"

# Splitting function
get_keywords_by_stopword_method <- function(content_vector, useStemming = TRUE, myStopWords) {
  text <- SimpleCorpus(VectorSource(enc2utf8(content_vector)))
  text <- tm_map(text, content_transformer(tolower))

  text <- tm_map(text, content_transformer(function(x, a_pattern) gsub(a_pattern, "; ", x)), my_stops_space)
  text <- tm_map(text, content_transformer(function(x, a_pattern) gsub(a_pattern, "", x)), my_stops_starts)
  text <- tm_map(text, content_transformer(function(x, a_pattern) gsub(a_pattern, "", x)), my_stops_ends)
  text <- tm_map(text, content_transformer(function(x, a_pattern) gsub(a_pattern, "; ", x)), my_pattern)
  text <- tm_map(text, content_transformer(function(x, a_pattern) gsub(a_pattern, "; ", x)), my_specials)
  text <- tm_map(text, content_transformer(function(x, a_pattern) gsub(a_pattern, "; ", x)), my_stops_colon)
  
  text <- tm_map(text, content_transformer(function(x, a_pattern) gsub(a_pattern, "; ", x)), my_stops_space)
  text <- tm_map(text, content_transformer(function(x, a_pattern) gsub(a_pattern, "", x)), my_stops_starts)
  text <- tm_map(text, content_transformer(function(x, a_pattern) gsub(a_pattern, "", x)), my_stops_ends)
  text <- tm_map(text, content_transformer(function(x, a_pattern) gsub(a_pattern, "; ", x)), my_pattern)
  text <- tm_map(text, content_transformer(function(x, a_pattern) gsub(a_pattern, "; ", x)), my_specials)
  text <- tm_map(text, content_transformer(function(x, a_pattern) gsub(a_pattern, "; ", x)), my_stops_colon)
  
  text <- tm_map(text, content_transformer(function(x, a_pattern) gsub(a_pattern, "; ", x)), my_stops_space)
  text <- tm_map(text, content_transformer(function(x, a_pattern) gsub(a_pattern, "", x)), my_stops_starts)
  text <- tm_map(text, content_transformer(function(x, a_pattern) gsub(a_pattern, "", x)), my_stops_ends)
  text <- tm_map(text, content_transformer(function(x, a_pattern) gsub(a_pattern, "; ", x)), my_pattern)
  text <- tm_map(text, content_transformer(function(x, a_pattern) gsub(a_pattern, "; ", x)), my_specials)
  text <- tm_map(text, content_transformer(function(x, a_pattern) gsub(a_pattern, "; ", x)), my_stops_colon)
  
  text <- tm_map(text, content_transformer(function(x, a_pattern) gsub(a_pattern, "; ", x)), my_stops_space)
  text <- tm_map(text, content_transformer(function(x, a_pattern) gsub(a_pattern, "", x)), my_stops_starts)
  text <- tm_map(text, content_transformer(function(x, a_pattern) gsub(a_pattern, "", x)), my_stops_ends)
  text <- tm_map(text, content_transformer(function(x, a_pattern) gsub(a_pattern, "; ", x)), my_pattern)
  text <- tm_map(text, content_transformer(function(x, a_pattern) gsub(a_pattern, "; ", x)), my_specials)
  text <- tm_map(text, content_transformer(function(x, a_pattern) gsub(a_pattern, "; ", x)), my_stops_colon)

  text <- tm_map(text, removeWords, myStopWords)
  text <- tm_map(text, removeNumbers)
  text <- tm_map(text, stripWhitespace)
  text <- tm_map(text, stripWhitespace)
  if (useStemming) {text <- tm_map(text, stemDocument, language = "english")}
  text <- tm_map(text, removeWords, myStopWords)
  return(text)
}


#########################################
#########################################
# Clean and generate the text to be used in the topic model
#########################################
# We are using title, abstract, DE, and ID keywords only.
# To use Abstract keywords we need to merge them with the title.
# In the case of using abstracs, is advised to remove copyright statements

# text preparation of each column
title_text <- tolower(iconv(myDataCorrect$TI, "UTF-8", "UTF-8", sub=''))
ab_text <- remove_copyright_statements(tolower(iconv(myDataCorrect$AB, "UTF-8", "UTF-8",sub='')))
de_keywords <- myDataCorrect$DE %>% tolower()

# Concatenate the strings
tiab_keywords_raw <- paste(title_text, ab_text, sep = ". ") %>% tolower()
tiab_keywords <- get_keywords_by_stopword_method(tiab_keywords_raw, myStopWords = myStopWords, useStemming = FALSE) %>% corpusToText()
tiab_keywords <- gsub("-", " ", tiab_keywords)
tiab_keywords <- gsub("(; )+", "; ", tiab_keywords)
all_keywords <- paste(tiab_keywords, de_keywords, sep = "; ")

# Get the unique raw keywords (raw = as they appear in the text)
all_unique_raw_keywords <- gsub(";$| ;$|^; |^ |^a | s | d |\\\\\\b", "", all_keywords) %>% strsplit(split = ";") %>% unlist %>% trimws %>% trimws %>% table
all_unique_raw_keywords <- all_unique_raw_keywords[!names(all_unique_raw_keywords) %in% c("","NA","a","b","c","d","e", "f","g","h","i","j","k","l","m","n","o","p", "q","r","s","t","u","v","w","x","y","z")]
all_unique_raw_keywords <- data.frame(keywords = names(all_unique_raw_keywords), 
                                      counts = as.numeric(all_unique_raw_keywords),
                                      stringsAsFactors = FALSE)

# Get stems
from_raw_to_stem <- sapply(all_unique_raw_keywords$keyword, function(x) {
  tmp <- strsplit(x, split = " ") %>% unlist %>% trimws %>% SnowballC::wordStem()
  return(paste(tmp, collapse = " "))
})
names(from_raw_to_stem) <- all_unique_raw_keywords$keyword

# Conversion vector
from_stem_to_raw <- all_unique_raw_keywords$keyword[order(all_unique_raw_keywords$counts, decreasing = TRUE)]
from_stem_to_raw <- from_stem_to_raw[!duplicated(from_stem_to_raw)]
names(from_stem_to_raw) <- from_raw_to_stem[from_stem_to_raw]

##############################################################################################
# Tidy vector
# THIS PART IS SLOW!!!
clean_keywords <- strsplit(all_keywords, "; ")
clean_keywords <- lapply(clean_keywords, function(x) {
  temp <- trimws(x)
  #temp <- unique(temp) #This line no needed for topic model. We need repeated values in each document. We need this line in cluster aggregation only, when each keyword should only appear once per document because they will be aggregated. 
  temp <- from_raw_to_stem[temp]
  # Custom correction can also be done here
  #temp <- mesh_conversion_table$mesh_root_stem[match(temp, mesh_conversion_table$mesh_syn_stem)]
  temp <- temp[!is.na(temp)]
  temp <- paste(temp, collapse = "; ")
  return(temp)
})
clean_keywords <- unlist(clean_keywords)

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
                         myStopWords = myStopWords)
myText <- corpusToText(myCorpusText)


# Obtain the documents that are not blank
blankLines <- unname(sapply(myText, nchar))
myDataCorrect <- dataset[blankLines>2,]
myText <- myText[blankLines>2]

###################################################
# Preparation
# From http://cpsievert.github.io/LDAvis/reviews/reviews.html

# tokenize on space and output as a list:
doc.list <- strsplit(myText, "[[:space:]]+")
doc.list <- lapply(doc.list, function(x) x[which(nchar(x)>1)]) #Ensure we remove white spaces

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# Remove words that appear less or equal to:
infrequent_terms_threshold <- 5

# Compute tfidf
tf.idf <- get_TDIDF(myCorpusText, infrequent_terms_threshold)

# a threshold to remove corpus specific stopwords based on tfidf score. 
# valid range: from 0.001 to 1, or "average" or "firstq"
tfidf_thresold <- "average"

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
D <- length(documents)  # number of documents
W <- length(vocab)  # number of terms in the vocab
doc.length <- sapply(documents, function(x) sum(x[2, ]))  # number of tokens per document
N <- sum(doc.length)  # total number of tokens in the data
term.frequency <- as.integer(term.table)  # frequencies of terms in the corpus
