# 20211106
# Get a data frame to use in Fukan System

# Input: 
# - Open a finished analysis environment where `myDataCorrect` and `dataset` objects are present.
# - A csv file whith the list of selected articles. In this case wee look for the file
#   `selected_articles.csv` having the columns ID being the UT of the article and the Final judgement by coauthors.
#   This file is expected to be in the Materials folder of the project.


# Output:
# - A .tsv file to use in Fukan System

#####
# Libraries
library(Opener5)

# Open files
# - Load environ
# - Open `selected_articles.csv`
selected_articles <- read.csv(file.choose(), stringsAsFactors = FALSE, header = TRUE)

# Get the list of selected articles
tmp <- c(selected_articles$ID[selected_articles$Final == 1])

# Get the data frame of selected articles
df <- dataset[dataset$UT %in% tmp,]

# Remove columns not needed in Fukan System
colnames(df)
df[,c(1:14,52:66)] <- NULL
colnames(df)

# Write the output file
write_for_fukan(df, file_name = "selected_articles_for_fukan.tsv")
