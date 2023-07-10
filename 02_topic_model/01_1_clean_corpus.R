
# In this file I ...
# Compute `Corpus` and `myText`
# Copute the `doc.list` and `term.table`

#########################################

library(slam)

#########################################
dataset <- as.data.frame(dataset)
columns <- rp$text_columns
# Functions
# Functions that facilitate identifying meaningless keywords by TFIDF threshold
tidyCorpus <- function(myData, columns, useStemming, myStopWords) {
  documents <- apply(myData[columns], 1, paste, collapse = ". ")
  text <- SimpleCorpus(VectorSource(enc2utf8(documents)))
  text <- tm_map(text, content_transformer(tolower))
  text <- tm_map(text, removeWords, stopwords("english"))
  text <- tm_map(text, content_transformer(function(x, a_pattern) gsub(a_pattern, "children", x)), "child")
  text <- tm_map(text, content_transformer(function(x, a_pattern) gsub(a_pattern, "children", x)), "childrenren")
  text <- tm_map(text, removeWords, myStopWords)
  text <- tm_map(text, removePunctuation)
  text <- tm_map(text, removeNumbers)
  text <- tm_map(text, stripWhitespace)
  text <- tm_map(text, stripWhitespace)
  if (tmo$useStemming) {text <- tm_map(text, stemDocument, language = "english")}
  text <- tm_map(text, removeWords, myStopWords) #We do it twice to remove stemed versions
  text <- tm_map(text, stripWhitespace)
  return(text)
}

# Transform a tidy corpus to a vector of normal, but cleaned text.
# Inputs: The result from tidyCorpus()
# Output: A vector of characters. The text to be used in the topic model
corpusToText <- function(a_tidyCorpus) {
  text <- unlist(sapply(1:length(a_tidyCorpus), function(x){return(a_tidyCorpus[[x]]$content)}))
  return(text)
}

# Put the documents into the format required by the lda package:
get_terms <- function(x) {
  index <- match(x, vocab)
  index <- index[!is.na(index)]
  rbind(as.integer(index - 1), as.integer(rep(1, length(index))))
}

# Function to remove the copyright from abstracts.
# Input: A char string or vector. Ususally the abstracts from WOS
# Output: The imput without the copyritgh statements.
# Dependencies: None.
remove_copyright_statements <- function(a_text) {
  return(gsub(" [A-z0-9 ]*?\\(C\\).*$", "", a_text))
}

#########################################
# EXECUTE
#########################################
# Remove the copyright statement in academic papers from WOS.
if (params$type_of_dataset == "papers") {
  dataset$AB <- remove_copyright_statements(dataset$AB)
}

# Get the clean text to feed the topic model
myCorpusText <- tidyCorpus(dataset, columns, useStemming, myStopWords)
myText <- corpusToText(myCorpusText)

# Obtain the documents that are not blank
blankLines <- unname(sapply(myText, nchar))
myDataCorrect <- dataset[blankLines>2,]
myText <- myText[blankLines>2]

#########################################
# Preparation
# From http://cpsievert.github.io/LDAvis/reviews/reviews.html

# tokenize on space and output as a list:
doc.list <- strsplit(myText, "[[:space:]]+")
doc.list <- lapply(doc.list, function(x) x[which(nchar(x)>1)]) #Ensure we remove white spaces.

# compute the table of terms:
term.table <- table(unlist(doc.list))
term.table <- sort(term.table, decreasing = TRUE)