# 20210803

# Comparing 2 datasets.
# This code was created to compare two clustering results.
# We want to know how articles from other dataset appear in the in the present dataset.
# Also, to measure what clusters have a particular concentration of a set of keywords.

# First load the environment of the smaller dataset.
# In this case we need the one 280 article of ai design and NPD
# An save the dataset as an object.
ai_dataset <- all_dataset
save(ai_dataset, file = "ai_dataset.rdata")

# Then, load the larger dataset. The one with the clustering solution we one to analyse
# Also, load the dataset of the smaller dataset we just created

# Search what articles of the smaller appear in the bigger.
# Assignations:
dataset_minimal$ai <- dataset_minimal$UT %in% ai_dataset$UT
table(dataset_minimal$ai)

orphans$ai <- orphans$UT %in% ai_dataset$UT
table(orphans$ai)

# Notice that "dataset" and "dataset_minimal" contain the same articles
# But dataset mininmal is sorted in the order of the nodes in the network, while dataset is sorted as they come 
# from Fukan System.
dataset$ai <- dataset$UT %in% ai_dataset$UT
table(dataset$ai)

# Check the appeareance
table(dataset$level0, dataset$ai)
table(dataset$subcluster_label1, dataset$ai)
table(dataset$subcluster_label2, dataset$ai)
table(dataset$subcluster_label3, dataset$ai)

# Check how a set of keywords is distributed in the clusters 
my_text2 <- paste(dataset$TI, dataset$AB, dataset$DE, dataset$ID, sep=" ") %>% enc2utf8() %>% tolower
my_query <- "artificial intelligence|machine learning|natural language processing|deep learning|neural network"
my_results <- grepl(my_query, my_text2)

table(my_results)
ttt <- table(dataset$subcluster_label3, my_results)
ttt <- data.frame(cluster = rownames(ttt),
                  size = ttt[,1],
                  ai = ttt[,2])
ttt$prop <- ttt$ai / ttt$size
