# 20210806

# Before Fukan System we need to narrow down the dataset of design and NPD (Q193)
# Only have records published in journals ranked 2 or above 
# AND, that belong to determined abs fields

# Input: We need the "dataset" object, created from "Read multiple from WOS.R" or by any other means.
# i.e. the input is the dataset downloaded from Web of Science

# Output: 
# Computation of papers remaining for different values
# A file formatted like the WOS export to be used in Fukan System

##########################################################################################################
## FIRST APPROACH
# Process overview:
# •	Step 1: Get articles on “design and NPD” from any field of research from the database (i.e. the dataset)
# •	Step 2: Retain articles published in journals from valid management fields according to the ABS list: "ENT-SBM", "ETHICS-CSR-MAN", "IB&AREA", "INFO MAN", "INNOV", "MKT", "OPS&TECH", "OR&MANSCI", "ORG STUD", "SOC SCI", "STRAT"
# •	Step 3: Retain articles published on journals with rankings X or above
# •	Step 4: Retain articles that contain artificial intelligence keywords


##########################################################################################################
# Read the Academic Journal Guide 2021
abs_journals <- read.csv("C:\\Users\\crist\\OneDrive\\Documentos\\00-Research projects\\47 - NPD Design\\04-Materials\\ABS Journal Guide\\ABS 2021 journal rankings.csv"
                         , stringsAsFactors = FALSE, check.names = FALSE)

# Format this file properly. 
abs_journals <- abs_journals[!duplicated(abs_journals$ISSN),]
abs_journals$abs_field <- abs_journals$Field
abs_journals$abs_rank <- abs_journals$"AJG 2021"
abs_journals$ISSN <- abs_journals[,1]
abs_journals[,1] <- NULL
abs_journals$journals <- enc2utf8(abs_journals$"Journal Title") %>% tolower
abs_journals$journals <- gsub(" & ", " and ", abs_journals$journals)

# Prepare journal names
dataset$SO <- gsub(" & ", " and ", dataset$SO)

# Add the abs values to the dataset
dataset <- dataset[,-c(38:43)]
dataset <- merge(dataset, abs_journals[,c("ISSN", "abs_rank", "abs_field")], by.x = "SN", by.y = "ISSN", all.x = TRUE, all.y = FALSE)
dataset <- merge(dataset, abs_journals[,c("journals", "abs_rank", "abs_field")], by.x = "SO", by.y = "journals", all.x = TRUE, all.y = FALSE)

dataset$abs_field <- sapply(c(1:nrow(dataset)), function(x){
  if (!is.na(dataset$abs_field.x[x])) {tmp <- dataset$abs_field.x[x]} else {
  if (!is.na(dataset$abs_field.y[x])) {tmp <- dataset$abs_field.y[x]} else {tmp <- NA}}
  return(tmp)
})

dataset$abs_rank <- sapply(c(1:nrow(dataset)), function(x){
  if (!is.na(dataset$abs_rank.x[x])) {tmp <- dataset$abs_rank.x[x]} else {
  if (!is.na(dataset$abs_rank.y[x])) {tmp <- dataset$abs_rank.y[x]} else {tmp <- 0}}
  return(tmp)
})

dataset <- dataset[,!colnames(dataset) %in% c("abs_rank.x", "abs_field.x", "abs_rank.y", "abs_field.y")]

# Valid ABS fields
valid_abs_fields <- c("ENT-SBM", "ETHICS-CSR-MAN", "IB&AREA", "INFO MAN", "INNOV", "MKT", "OPS&TECH", "OR&MANSCI", "ORG STUD", "SOC SCI", "STRAT")

# Valid AI keywords
ai_keywords <- c("artificial intelligence", "case based reasoning", "computer vision", "cognitive computing", "cognitive science", 
                 "data mining", "data science", "deep learning", "expert system", "fuzzy linguistic modeling", "fuzzy logic", "genetic algorithm", "image recognition", 
                 "k means", "knowledge based system", "logic programming", "machine learning", "machine vision", "natural language processing", 
                 "neural network", "pattern recognition", "recommendation system", "recommender system", "semantic network", "speech recognition", "support vector machine", 
                 "svm", "text mining", "topic model", "supervised learning", "unsupervised learning", "semi supervised learning", "reinforcement learning", "dimensionality reduction", 
                 "cluster analysis", "decision tree", "regression analysis", "bayesian network", "genetic algorithm")

ai_keywords_regex_pattern <- paste(ai_keywords, collapse = "|")

#preparing text for matching
my_text <- paste(dataset$TI, dataset$AB, dataset$DE, dataset$ID, sep = " " ) %>% enc2utf8 %>% tolower %>% gsub("[[:punct:]]", "", .)

# Mark records having the keywords
dataset$ai <- grepl(ai_keywords_regex_pattern, my_text)


#############################################################################################
# Get datasets that follow different rank criteria
dataset_1 <- dataset[dataset$abs_rank >= 1 & dataset$abs_field %in% valid_abs_fields,]
dataset_2 <- dataset[dataset$abs_rank >= 2 & dataset$abs_field %in% valid_abs_fields,]
dataset_3 <- dataset[dataset$abs_rank >= 3 & dataset$abs_field %in% valid_abs_fields,]


############################################################################################
# For each dataset, how many articles refer to AI?
table(dataset_1$ai)
table(dataset_2$ai)
table(dataset_3$ai)

# Write the file
article_report <- dataset_3[dataset_3$ai, c("AU", "PY", "DI", "TI", "AB", "Z9", "DE", "SO", "WC", "UT", "abs_rank", "abs_field")]
write.csv(article_report, file = "ai_design_npd_opt7_20210807.csv", row.names = FALSE)


############################################################################################
############################################################################################
############################################################################################
############################################################################################
## SECOND APPROACH
# Process overview:
# •	Step 1: Get articles on “design and NPD” from any field of research from the database (i.e. the dataset)
# •	Step 2: ---> Articles in WC c("management", "business", "operations research & management science")))
# •	Step 3: Retain articles published on journals with rankings X or above
# •	Step 4: Retain articles that contain artificial intelligence keywords
############################################################################################

# Label the articles in the selected WC categories
dataset$management <- sapply(tolower(dataset$WC), function(x) {
  tmp <- strsplit(x, "; ") %>% unlist
  return(any(tmp %in% c("management", "business", "operations research & management science")))
})

table(dataset$management)
table(dataset$abs_rank, dataset$management)


############################################################################################
# Get datasets that follow different rank criteria
dataset_0 <- dataset[dataset$management,]
dataset_1 <- dataset[dataset$abs_rank >= 1 & dataset$management,]
dataset_2 <- dataset[dataset$abs_rank >= 2 & dataset$management,]
dataset_3 <- dataset[dataset$abs_rank >= 3 & dataset$management,]


############################################################################################
# For each dataset, how many articles refer to AI?
table(dataset_0$ai)
table(dataset_1$ai)
table(dataset_2$ai)
table(dataset_3$ai)


# Save the desired option to be analyzed by Fukan System
write_for_fukan(dataset_3[dataset_3$ai, c(1:37)], file_name = "ai-design-npd-opt7.tsv")

# Save the object
save(dataset_3, file = "dataset_3_opt7.rdata")

# Verify that the 3 categories are not present in the remaining dataset
test <- dataset[!dataset$management,]
test <- strsplit(test$WC, "; ") %>% unlist %>% table()
c("management", "business", "operations research & management science") %in% names(test)

table(dataset_3$abs_rank)
