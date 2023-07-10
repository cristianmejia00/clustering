# 20210207
# Create an interactive dendrogram of the relationship between subclusters

# Requirements
# Load the environment of the level of subclustering to plot
# OR just get the "myDataCorrect" object
# For more info we need an RCS object too.

# Install and load libraries
#install.packages("rlang")
#install.packages("digest")
#devtools::install_github("AdeelK93/collapsibleTree")
library(collapsibleTree)

################################################################################################
# Prepare the file
hierStr <- myDataCorrect[!duplicated(myDataCorrect$X_C),c("level0", "level1", "level2", "X_C")]
hierStr <- hierStr[order(hierStr$X_C),]
# Add info from the RCS object. If we dont have it, just comment out this.
hierStr$info <- sapply(hierStr$X_C, function(x){
  idx <- which(rcs_complete$Cluster == x)
  info <- paste(as.character(rcs_complete$Nodes[[idx]]), " articles; ",
                as.character(rcs_complete$`Ave. Year`[[idx]]), " ave. year; ",
                " keywords: ", substr(rcs_complete$`Frequent Keywords`[[idx]], 1, 80),
                collapse = "")
})
hierStr[hierStr == 0] <- NA

# Plot the dendrogram:
p <- collapsibleTree(hierStr, c("level0","level1","info"), collapsed = TRUE)
p

