# 20180804
# Create a explorer view for topic models
# Based on dendrograms
# Inspired  by: http://wanko-sato.hatenablog.com/entry/2017/07/16/161406

# Input objects needed: dataset, optional=h_matrix
# Input file needed: WC_color_scheme.csv
# Main characteristics: dataset will have a X_C column representing clusterts/topics and X_N an index column from 1 to n.

library(RJSONIO)
library(treemap)
library(d3treeR) # devtools::install_github("timelyportfolio/d3treeR")

# Select the color Scheme to use. These are files that must be located in the same working directory
wc_color <- read.csv("IPC4_color_scheme.csv")

##############################################
# Correction of table names
# Rename index
dataset$X_N <- c(1:nrow(dataset))

# Assign topics
dataset$X_C <- myDataCorrect$topic

##############################################
# Dendrogram
##############################################

# Compute distance matrix
# At this point we have different sources to compute the distance matrix
# From the cosine simialrity of probable terms, (Best results)
# From the cosine similarity of exclusive terms,
# From the t(Phi) table, the Topic by terms
# From the t(Theta) table, The Document by Topic (Worst results)

# The only condition is that it needs to be formated with the topics as rows.

# Plug the table
input_table <- t(theta) #h_matrix # #h_matrix #t(phi)
row.names(input_table) <- c(1:K)
distance_matrix <- dist(input_table, method = "euclidean")

# Hierarchical clustering
clusterWC = hclust(distance_matrix, method = "ward.D") 

# Plot the dendrogram
par(cex = 0.5) #Change label size
plot(clusterWC, hang = -1)


#############################################
# Assign colors to each category. #Similar categories will have similar color
# This is done thanks the file "wc_color_scheme.csv" made from my previous study on correlation of WC categories
# wc_color <- read_from_others_csv(file.choose())
# wc_color$color <- rainbow(nrow(wc_color), alpha = 0.7)
# wc_color <- rbind(wc_color, data.frame("WC"="blank","Group" = "V100", "color" = "#FFFFFF"))

# Get the first WC category. We color the nodes based on this
first_WC <- sapply(dataset$WC, function(x) {
  temp <- strsplit(x, "; ")
  if (length(temp) > 0) {temp <- temp[[1]][1]} else {temp <- "blank"}
  return(unname(temp))
})

# To map article colors
WC_col <- as.character(wc_color$color)
names(WC_col) <- wc_color$WC
article_color <- WC_col[first_WC]

# To map article size based on cites
article_size <- dataset$Z9 + 10
names(article_size) <- dataset$X_N

# To map article title
article_title <- dataset$TI
names(article_title) <- dataset$X_N

# Second line of text
# Get the first author.
first_AU <- sapply(dataset$AU, function(x) {
  temp <- strsplit(x, "; ")
  if (length(temp) > 0) {temp <- temp[[1]][1]} else {temp <- "blank"}
  return(unname(temp))
})
biblio <- paste(first_AU, dataset$PY, dataset$J9, first_WC, sep = '; ')

# To map article category
article_biblio <- biblio
names(article_biblio) <- dataset$X_N


#############################################
#Create levels for topics
K_level_1 <- cutree(clusterWC, k = 2)
K_level_2 <- cutree(clusterWC, k = ceiling(sqrt(K)))
K_level_3 <- cutree(clusterWC, k = K)

#############################################
#Create levels for documents
d_level_4 <- dataset$X_N
d_level_3 <- dataset$X_C
d_level_2 <- K_level_2[dataset$X_C]
d_level_1 <- K_level_1[dataset$X_C]

#############################################
hierStr <- as.data.frame(cbind(rep(PROJECT, nrow(dataset)), d_level_1, d_level_2, d_level_3, d_level_4))
colnames(hierStr) <- c("root","level1","level2","level3", "level4")
hierStr$level4 <- as.numeric(as.character(hierStr$level4)) #last level must be numeric
row.names(hierStr) <- dataset$X_N



hierStr <- data.frame(hierStr,
                      size=sqrt(dataset$Z9 + 1),
                      #size=c(1000),
                      id=dataset$X_N,
                      name=dataset$TI,
                      category=first_WC)



################################################
indexList <- c("root","level1","level2","level3","level4")
#treedat <- treemap(hierStr, index=indexList, vSize='size')
hierStr$color <- article_color
treedat <- treemap(hierStr, index=indexList, vSize='size', vColor = "color", type = 'color')

treedat <- treedat$tm
treedat$color <- article_color[as.numeric(as.character(treedat$level4))]
#treedat$vColor <- article_color[as.numeric(as.character(treedat$level4))]
treedat$size <- article_size[as.numeric(as.character(treedat$level4))]
treedat$title <- article_title[as.numeric(as.character(treedat$level4))]
treedat$biblio <- article_biblio[as.numeric(as.character(treedat$level4))]


res <- d3treeR:::convert_treemap(treedat, rootname="flare")
json <- RJSONIO::toJSON(res, auto_unbox = TRUE)
json <- gsub("color","fill",json)
writeLines(json,"treeJson.json")
getwd()
