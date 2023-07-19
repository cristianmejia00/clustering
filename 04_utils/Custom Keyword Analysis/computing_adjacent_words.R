# 20160203
# Find adjacent words.

# 20210131
# Code checked and used to compute the adjacent keywords of performance for the paper 
# on Manufacturing Enterprise Performance and Environmental Sustainability


library(stringr)
library(tm)
library(dplyr)

#Read the files .csv
dataset <- read.csv(file.choose())

#Read the files .tsv
dataset <- read.table(file.choose(), header=T, sep="\t", fill= T, quote ="", 
                   row.names=NULL, stringsAsFactors = F, check.names=FALSE)

#Verify columns
names(dataset)

#Merge title and abstract
dataset$TIAB <- paste(dataset$"TI", dataset$"AB", sep=". ")

#Clean and format the text
remove_stems <- FALSE
#########################################
dataset <- as.data.frame(dataset)

# Functions
# Text cleaner
tidytext <- function(myData, columns, useStemming, settings$stopwords$myStopWords) {
  documents <- apply(myData[columns], 1, paste, collapse = ". ")
  text <- Corpus(VectorSource(documents))
  text <- tm_map(text, content_transformer(tolower))
  text <- tm_map(text, stripWhitespace)
  text <- tm_map(text, removeWords, stopwords("english"))
  text <- tm_map(text, removeWords, settings$stopwords$myStopWords)
  text <- tm_map(text, removePunctuation)
  text <- tm_map(text, removeNumbers)
  text <- tm_map(text, stripWhitespace)
  if (useStemming) {text <- tm_map(text, stemDocument, language = "english")}
  text <- tm_map(text, removeWords, settings$stopwords$myStopWords) #We do it twice to remove stemed versions
  text <- tm_map(text, stripWhitespace)
  if (remove_stems) {text <- tm_map(text, removeWords, ERP_vocab)} 
  text <- tm_map(text, stripWhitespace)
  text <- unlist(sapply(1:length(text), function(x){return(text[[x]]$content)}))
  return(text)
}

data_TIAB <- tidytext(as.data.frame(dataset), c("TIAB"), useStemming = TRUE, settings$stopwords$myStopWords = c('article'))
data_TIAB <- tolower(dataset$TIAB)
#data_TIAB <- dataset$"full_text"
# data_TIAB <- dataset$TIAB
# data_TIAB <- unique(data_TIAB)
# ########
# #Data prepocessing
# #everything to lowercase
# data_TIAB <- tolower(data_TIAB)

##########################################################################
## Extra cleaning
#remove apostrophe ' and quotation marks "
data_TIAB <- gsub("'", "", data_TIAB)
data_TIAB <- gsub("\"", "", data_TIAB)

#remove plural ecosystems to ecosystem
data_TIAB <- gsub("ecosystems", "ecosystem", data_TIAB)
data_TIAB <- gsub("services", "service", data_TIAB)
data_TIAB <- gsub("processes", "process", data_TIAB)
data_TIAB <- gsub("types", "type", data_TIAB)
data_TIAB <- gsub("changes", "change", data_TIAB)
data_TIAB <- gsub("impacts", "impact", data_TIAB)
data_TIAB <- gsub("models", "model", data_TIAB)
data_TIAB <- gsub("effects", "effect", data_TIAB)
data_TIAB <- gsub("metrics", "metric", data_TIAB)
data_TIAB <- gsub("measures", "measurement", data_TIAB)
data_TIAB <- gsub("measurements", "measurement", data_TIAB)
data_TIAB <- gsub("indicators|indices|index|indexes", "indicator", data_TIAB)
data_TIAB <- gsub("analyses", "analysis", data_TIAB)
data_TIAB <- gsub("methodologies|methodology|methods", "method", data_TIAB)
##########################################################################

######## When needed to remove stopwords.

#Using the TM package, remove unnecesary spaces, stopwords, and numbers.
text <- SimpleCorpus(VectorSource(data_TIAB))
text <- tm_map(text, removeWords, stopwords("english"))
text <- tm_map(text, removeNumbers)
text <- tm_map(text, stripWhitespace)

#the return to normal characters
#data_TIAB2 <-data.frame(text=unlist(sapply(text, '[', "content")), stringsAsFactors=FALSE)
#data_TIAB2 <-sapply(1:nrow(data_TIAB2), function(x) {data_TIAB2[x,1]})
#data_TIAB2[[2]]

test <- sapply(c(1:length(text)), function(x){
  text[[x]]["content"]
})
data_TIAB2 <- unlist(test) %>% unname()
data_TIAB3 <- SnowballC::wordStem(data_TIAB2)

######### search for this keyword

query <- "scientometr[:alpha:]+"

query_list <- c(
  paste(query, "[:alnum:]+", sep = " "),
  paste("[:alnum:]+", query, sep = " "),
  paste(query, "[:alnum:]+[:space:][:alnum:]+", sep = " "),
  paste("[:alnum:]+[:space:][:alnum:]+", query, sep = " "),
  paste("[:alnum:]+", query, "[:alnum:]+", sep = " ")
)

######### Extract values
# Note: str_extract_all extract every instance of the keyword. If the word is repeated in title
# and abstract it will count 2 per document. 
# To count documents we need to obtain the list of keywords and then use grepl.
right_1 <- SnowballC::wordStem(unlist(str_extract_all(data_TIAB2, query_list[[1]])))
sort(table(right_1), decreasing = T)[1:100]

tmp <- sort(table(right_1), decreasing = T)[1:100]
scientometrics_kwds <- tmp[1:20]
write.csv(tmp, file = "informetrics_keywords.csv")

left_1 <- unlist(str_extract_all(data_TIAB2, query_list[2]))
sort(table(left_1), decreasing = T)[1:100]



right_2 <-unlist(str_extract_all(data_TIAB, query_list[3]))
sort(table(right_2), decreasing = T)[1:50]

left_2 <- unlist(str_extract_all(data_TIAB, query_list[4]))
sort(table(left_2), decreasing = T)[1:50]

center <- unlist(str_extract_all(data_TIAB, query_list[5]))
sort(table(center), decreasing = T)[1:50]


# Aggregated report
final <- lapply(list(right_13, left_13, right_23, left_23, center3), function(x) {
  xxx <- table(x)
  xxx <- sort(xxx, decreasing = TRUE)
  xxx <- as.data.frame.table(xxx, stringsAsFactors = FALSE, optional = TRUE)
}) %>% bind_rows()
