# transforms from a dataset to .ris
library(RefManageR)
library(tools)
library(stringr)

# If not yet, a column X_N having an ID for each paper must be appended to the dataframe
# X_N <- c(1:nrow(dataset)) # dataset$UT

#################################################################################################
# A function to generate a bibtext file that can be used in any reference manager like Mendeley
# It takes a bibliometric dataframe with the headers of the Web of Science. (At least: TI, AU, PY, SO, VL, IS, BP, EP, DI)
# Also column Z9, the number of citations receive, if we use the top parameter. (i.e. we only want the most cited ones)
# Additionally it must have columns X_N with a paper ID (can be anything) and
# Column X_C, the cluster. 
write_refs_from_cluster <- function (a_data_frame, cluster_number, top = 10) {
  my_data <- subset(a_data_frame, X_C == 80)
  my_data <- my_data[order(my_data$Z9, decreasing = TRUE)[1:10],]
    bib_docs <- lapply(c(1:nrow(my_data)), function(x) {
    roww <- my_data[x,]
    bib <- c(bibtype = "article", 
             key = roww$X_N,
             volume = roww$VL %>% gsub("V","",.),
             number = roww$IS,
             pages = paste(roww$BP, roww$EP, sep = "--") %>% gsub("P|U","",.),
             title = str_to_sentence(roww$TI),
             author = toTitleCase(roww$AU) %>% gsub(";", ". and", .) %>% paste(., ".", sep = ""), 
             journal = roww$SO %>% tolower() %>% toTitleCase(), 
             year = roww$PY,
             doi = roww$DI)
    return(bib)
  }) %>% as.BibEntry()
  file_name <- paste("references_cluster_", cluster_number, sep = "")
  WriteBib(bib_docs, file = file_name)
}



#################################################################################################
# OPTION B
# Similar to the previous but it assumes the data was filtered outside the function.
# A function to generate a bibtext file that can be used in any reference manager like Mendeley
# It takes a bibliometric dataframe with the headers of the Web of Science. (At least: TI, AU, PY, SO, VL, IS, BP, EP, DI)
# Additionally it must have column X_N with a paper ID (can be anything)
write_refs_from_df <- function (a_data_frame, file_name = "references.bib") {
  bib_docs <- lapply(c(1:nrow(a_data_frame)), function(x) {
    roww <- a_data_frame[x,]
    bib <- c(bibtype = "article", 
             key = roww$X_N,
             volume = roww$VL %>% gsub("V","",.),
             number = roww$IS,
             pages = paste(roww$BP, roww$EP, sep = "--") %>% gsub("P|U","",.),
             title = str_to_sentence(roww$TI),
             author = toTitleCase(roww$AU) %>% gsub(";", ". and", .) %>% paste(., ".", sep = ""), 
             journal = roww$SO %>% tolower() %>% toTitleCase(), 
             year = if(is.na(roww$PY)) {1800} else {roww$PY},
             doi = roww$DI)
    return(bib)
  }) %>% as.BibEntry()
  WriteBib(bib_docs, file = file_name)
}


#################################################################################################
# Use case.
# Get data from a list of cluster numbers
# And save the 10 most cited articles.

# The number of papers per cluster based on the largest X_E
nnn <- 5
chosen_clusters <- c(4,5,6,7,8,9,10)
#chosen_clusters <- c(4,16,17,18,20,21,23,26,27,29,41,46,72,73,89,105,107,116,117,139,152,154,163,164,165)
myDataCorrect$SO[is.na(myDataCorrect$SO)] <- ""

for (cl in chosen_clusters) {
  print(cl)
  cl <- as.numeric(cl)
  file_name <- paste("bib_", cl, ".bib", sep = "")
  temp <- subset(myDataCorrect, myDataCorrect$X_C == cl)
  temp <- temp[temp$SO != "",]
  #temp <- temp[temp$PY >= 2018,]
  temp <- temp[order(temp$X_E, decreasing = TRUE)[1:nnn],]
  write_refs_from_df(temp, file_name)
}

getwd()
# OR
# Selected papers
articles_selected <- lapply(rcs_with_maxs$max_cluster_label, function(cl) {
  cl <- as.numeric(cl)
  print("-----")
  print(cl)
  temp <- subset(article_report_new, article_report_new$Cluster == cl)
  print(nrow(temp))
  temp <- temp[temp$Year >= 2018,]
  print(nrow(temp))
  temp <- temp[order(temp$Citations.received, decreasing = TRUE)[1:nnn],]
  return(temp)
}) %>% rbind_list()



articles_selected <- merge(articles_selected, rcs_with_maxs[,c("max_cluster_label","legend")], by.x = "Cluster", by.y = "max_cluster_label")
write.csv(articles_selected, file="articles_selected_2018.csv")
write.csv(rcs_with_maxs, "rcs_with_maxs.csv")

table(rcs_with_maxs$max_cluster_label)
