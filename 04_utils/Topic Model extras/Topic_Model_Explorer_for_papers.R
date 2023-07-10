# 20180804
# Create a explorer view for topic models
# Based on dendrograms
# Inspired  by: http://wanko-sato.hatenablog.com/entry/2017/07/16/161406

# Input objects needed: myDataCorrect, optional=h_matrix
# Input file needed: WC_color_scheme.csv
# Main characteristics: myDataCorrect will have a X_C column representing clusterts/topics and X_N an index column from 1 to n.

library(RJSONIO)
library(treemap)
#devtools::install_github("timelyportfolio/d3treeR")
library(d3treeR) 


# Select the color Scheme to use. These are files that must be located in the same working directory
# "WC_color_scheme.csv"     For papers
# "IPC3_color_scheme.csv"   For patents
# "IPC4_color_scheme.csv"   For patents
wc_color <- read.csv("WC_color_scheme.csv")

##############################################
# Correction of table names
# Rename index
myDataCorrect$X_N <- c(1:nrow(myDataCorrect))

# Rename topics or clusters: "topic" for topics or "_C" for fukan clusters
if (!"X_C" %in% colnames(myDataCorrect)) {
  setnames(myDataCorrect, "topic", "X_C")
}


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
row.names(input_table) <- c(1:nrow(input_table))
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
first_WC <- sapply(myDataCorrect$WC, function(x) {
  temp <- strsplit(x, "; ")
  if (length(temp) > 0) {temp <- temp[[1]][1]} else {temp <- "blank"}
  return(unname(temp))
})

# To map article colors
WC_col <- as.character(wc_color$color)
names(WC_col) <- wc_color$WC
article_color <- WC_col[first_WC]

# To map article size based on cites
article_size <- myDataCorrect$Z9 + 10
names(article_size) <- myDataCorrect$X_N

# To map article title
article_title <- myDataCorrect$TI
names(article_title) <- myDataCorrect$X_N

# Second line of text
# Get the first author.
first_AU <- sapply(myDataCorrect$AU, function(x) {
  temp <- strsplit(x, "; ")
  if (length(temp) > 0) {temp <- temp[[1]][1]} else {temp <- "blank"}
  return(unname(temp))
})
biblio <- paste(first_AU, myDataCorrect$PY, myDataCorrect$J9, first_WC, sep = '; ')

# To map article category
article_biblio <- biblio
names(article_biblio) <- myDataCorrect$X_N

K=244
#############################################
#Create levels for topics
K_level_1 <- cutree(clusterWC, k = 2)
K_level_2 <- cutree(clusterWC, k = ceiling(sqrt(K)))
K_level_3 <- cutree(clusterWC, k = K)

#############################################
#Create levels for documents
d_level_4 <- myDataCorrect$X_N
d_level_3 <- myDataCorrect$X_C
d_level_2 <- K_level_2[myDataCorrect$X_C]
d_level_1 <- K_level_1[myDataCorrect$X_C]

#############################################
hierStr <- as.data.frame(cbind(rep(PROJECT, nrow(myDataCorrect)), d_level_1, d_level_2, d_level_3, d_level_4))
colnames(hierStr) <- c("root","level1","level2","level3", "level4")
hierStr$level4 <- as.numeric(as.character(hierStr$level4)) #last level must be numeric
row.names(hierStr) <- myDataCorrect$X_N



hierStr <- data.frame(hierStr,
                      size=sqrt(myDataCorrect$Z9 + 1),
                      #size=c(1000),
                      id=myDataCorrect$X_N,
                      name=myDataCorrect$TI,
                      category="test")#first_WC)



################################################
indexList <- c("root","level1","level2","level3","level4")
treedat <- treemap(hierStr, index=indexList, vSize='size')
treedat <- treedat$tm
treedat$color <- article_color[as.numeric(as.character(treedat$level4))]
treedat$size <- article_size[as.numeric(as.character(treedat$level4))]
treedat$title <- article_title[as.numeric(as.character(treedat$level4))]
treedat$biblio <- article_biblio[as.numeric(as.character(treedat$level4))]


res <- d3treeR:::convert_treemap(treedat, rootname="flare")
json <- RJSONIO::toJSON(res, auto_unbox = TRUE)
json <- gsub("color","fill",json)
writeLines(json,"treeJson.json")
getwd()
