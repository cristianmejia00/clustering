# 20190418
# Files to create auxiliar files 
## Bibtext for references
## A file for exploring the contents of each cluster


library(dplyr)
library(tools)

# Pick the clusters to consider
# Emergent clusters:
chosen_clusters <- c(1:13)

# References to save:
number_to_check <- 15

# Column to use as indicator
indicator <- "X_E"

#################################################################################################
# Save the references
# Load the R code wos_to_bibtex.R!
# Get data from a list of cluster numbers
# And save the 10 most cited articles.
# FLAG!!! ---- Z9 vs X_E
for (cl in chosen_clusters) {
  file_name <- paste("bib_", cl, ".bib", sep = "")
  temp <- subset(myDataCorrect, myDataCorrect$X_C == cl)
  #temp <- temp[temp$PY >= 2018,]
  temp <- temp[order(temp[indicator], decreasing = TRUE)[1:number_to_check],]
  write_refs_from_df(temp, file_name)
}
getwd()

#######################################################################################
# Get the contents (Title and Abstract) of chosen clusters (see above)
abstract_text <- lapply(chosen_clusters, function(cl) {
  temp <- subset(myDataCorrect, myDataCorrect$X_C == cl)
  temp <- temp[temp$PY >= 2018,]
  temp <- temp[order(temp[indicator], decreasing = TRUE)[1:number_to_check], c("X_C", "cluster_code", "AU", "PY","TI", "AB", "SO", "WC")] 
  temp$cluster <- cl
  return(temp)
}) %>% bind_rows()
write.csv(abstract_text, "emergent_references_2018.csv", row.names = FALSE)

getwd()
