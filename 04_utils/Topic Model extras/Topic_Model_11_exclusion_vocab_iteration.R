# 20180523 Iterative cleaning function.

# After some discussion with ranjit, we try to get improve the filtering dictionaries.
# The goal is to remve as much noise as posible, without compromising the quality of topic models.

# So far we have been used three filtering vocabularies:
# 1.- Stopwords
# 2.- ERP (Enterprise Resource Planing) vocabulary, used to extract "patent vocabulary" 
# 3.- A list of most frequent terms

# All list above are based on unigrams.

# However, it happens that some NGRAMS that may be thought as relevan are missed due to this very restrictive approach
# Mainly becuase we also remove unigrams that may be valuable in some context.

# Then the strategy is: Once we have a topic model solution cleaned with the 3 lists above, we also 
# obtain ngrams commig from the natural words, were only the 1.- stopwords were elimnated.
# We create a list of "natural ngrams" per topic, and select the most frequent ones.
# The threshold is set to normalized >= 50.
# This list of natural ngrams contain unigrams that are not present in the original topic model ngram solution
# Then, we compare the two list of unigrams The natural, and the cleaned from the topic model
# Those words that appear in the natural ngram list, that are not present in the other list
# might be worth to bring them back.
# Thus a new, more refinde dictionary 2 can be created.
# However, still some ovious words that are not stopwords can caise problem like "whose", ore domain specific 
# keywords. For those obvious, a manual list of white list and black list terms is created
# And incorporated to the dictionaries above.

#########################################
# Functions
# Text cleaner 2 To remove just stopwords, numbers and puntuation
tidytext2 <- function(myData, columns, useStemming, myStopWords) {
  documents <- apply(myData[columns], 1, paste, collapse = ". ")
  text <- Corpus(VectorSource(documents))
  text <- tm_map(text, content_transformer(tolower))
  text <- tm_map(text, stripWhitespace)
  text <- tm_map(text, removeWords, stopwords("english"))
  #text <- tm_map(text, removeWords, myStopWords)
  text <- tm_map(text, removePunctuation)
  text <- tm_map(text, removeNumbers)
  text <- tm_map(text, stripWhitespace)
  if (useStemming) {text <- tm_map(text, stemDocument, language = "english")}
  #text <- tm_map(text, removeWords, myStopWords) #We do it twice to remove stemed versions
  text <- tm_map(text, stripWhitespace)
  #if (remove_stems) {text <- tm_map(text, removeWords, ERP_vocab)} 
  #text <- tm_map(text, stripWhitespace)
  text <- unlist(sapply(1:length(text), function(x){return(text[[x]]$content)}))
  return(text)
}

myText[[100]] == myText2[[100]]
#########################################
# Execute
# Get the clean text to feed the topic model
myText2 <- tidytext2(myDataCorrect, columns, useStemming, myStopWords)

# Obtain the documents that are not blank
blankLines <- unname(sapply(myText2, nchar))
myDataCorrect2 <- myDataCorrect[blankLines>2,]
myText2 <- myText2[blankLines>2]


#########################################

ngrams_per_topic_2 <- lapply(id_com, function(x){
  cluster_text_content <- paste(myText2[myDataCorrect2$"X_C"==x], collapse = " ")
  cluster_ngram4 <- ngram_finder(cluster_text_content, n_size = 4)
  cluster_ngram3 <- ngram_finder(cluster_text_content, n_size = 3)
  cluster_ngram2 <- ngram_finder(cluster_text_content, n_size = 2)
  cluster_ngram_total <-  rbind.fill(cluster_ngram2, cluster_ngram3, cluster_ngram4)
  cluster_ngram_total$ngrams <- trimws(cluster_ngram_total$ngrams)
  cluster_ngram_total <- cluster_ngram_total[!duplicated(cluster_ngram_total$ngrams),]
  cluster_ngram_total <- cluster_ngram_total[cluster_ngram_total$normalized >= 50,]
  cluster_ngram_total$type <- "ngram"
  cluster_ngram_total$topic <- x
  return(cluster_ngram_total)
})
ngrams_per_topic_df_2 <-  rbind.fill(ngrams_per_topic_2)
names(ngrams_per_topic_df_2) <- c("term", "freq", "prop",  "word count","normalized", 'type' ,"topic")


#########################################
unigrams_from_ngrams <- unlist(ngrams_per_topic_df_2$term) %>% strsplit(split = " ") %>% unlist %>% table
unigrams_from_ngrams_terms <- names(unigrams_from_ngrams)

write.csv(unigrams_from_ngrams[re_inclusion], file = "Reincluded_Vocabulary_with_freq.csv", row.names = FALSE)

unigrams_from_ngrams
#########################################
re_inclusion <- intersect(ERP_vocab, unigrams_from_ngrams_terms)
re_inclusion <- setdiff(re_inclusion, manual_selection)
re_inclusion <- c(re_inclusion, "rfid")

re_exclusion <- setdiff(ERP_vocab, re_inclusion)
#ERP_vocab_backup <- ERP_vocab
#ERP_vocab <- ERP_vocab_backup
ERP_vocab <- re_exclusion

write.csv(re_exclusion, file = "excluded_vocabulary_from_ERP.csv", row.names = FALSE)
write.csv(myStopWords, file = "excluded_vocabulary_highly_freq.csv", row.names = FALSE)

"rfid" %in% re_exclusion

# Words to neglect from analysis. In natural language.
# Re_including: "system"
myStopWords <- c("patent", "claim", "device", "data", "module", "network", "control" , 
                 "base","method", "methods","terminal", "information", 
                 "connect", "connects", "connection", "communication", "internet", "things", "thing")

manual_selection <- c("end",
                      "whose",
                      "output",
                      "involv",
                      "input",
                      "use",
                      "one",
                      "provid",
                      "number",
                      "side",
                      "inner",
                      "money",
                      "eg",
                      "good",
                      "right",
                      "least",
                      "obtain",
                      "correspond",
                      "fix",
                      "actual",
                      "ad",
                      "outer",
                      "outlet"
)
