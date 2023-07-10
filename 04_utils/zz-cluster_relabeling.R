# 20220928

# Custom cluster relabeling
# With this code I manually remove and update the cluster numbering of a finished analysis.
# This happens when we find off-topic clusters that we wont include on the paper soi we remove them from 
# the datasets. Such a removal imply renumbering the clusters.

# However, renumbered also need to take care of the cluster 99.

# In this code we haddle all that.
# After this code is applied we need to run `00_run_me.R` to get updated reports with the new reordering.

# Note: Is good to copy the folder of results if we need to have the original. Because this process will
# overwrite the original folder. 

##################################################################################################
# Add years to network
V(g1)$PY <- myDataCorrect$PY[match(V(g1)$name, as.character(myDataCorrect$X_N))]

#  Sort myDataCorrect to have the same order as g1
myDataCorrect_g1 <- myDataCorrect[match(V(g1)$name, as.character(myDataCorrect$X_N) ),]

# verify order
all(myDataCorrect_g1$X_N == V(g1)$name)


# Update myDataCorrect
myDataCorrect <- myDataCorrect_g1

##################################################################################################
# Custom cluster relabels
myDataCorrect <- myDataCorrect[!myDataCorrect$X_C %in% c(16,17),]
table(myDataCorrect$X_C)

for (i in c(18,19,20,21)) {
  myDataCorrect$X_C[myDataCorrect$X_C==i] <- i-2
  if (i != 21) {
    myDataCorrect$level0[myDataCorrect$level0==i] <- i-2
    my_rgx <- paste("^", as.character(i), "-", sep = "")
    new <- paste(as.character(i-2), "-", sep = "")
    myDataCorrect$subcluster_label1 <- gsub(my_rgx, new,  myDataCorrect$subcluster_label1)
    myDataCorrect$subcluster_label2 <- gsub(my_rgx, new,  myDataCorrect$subcluster_label2)
    myDataCorrect$subcluster_label3 <- gsub(my_rgx, new,  myDataCorrect$subcluster_label3)
  }
}

##################################################################################################
# Updating dataset
dataset <- myDataCorrect
dataset$cluster_code <- NULL

# Verify that clusters where relabeled
table(myDataCorrect$X_C)
table(dataset$X_C)
table(dataset$level0)

##################################################################################################
# Remove objects that need to be updated when re-running the code
rm(myDataCorrect_SAMPLE, myDataCorrect_SAMPLE2, myDataCorrect_xe, papersText)
