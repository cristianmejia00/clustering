# 20221212

# Create the inital files for the clustering:

#########################################################################
# Approach 1. 
# To be used with `settings.R`
# Load the Factiva data environment 
# Load the news_vos_format_with_sentiment using readr

dataset <- newsVOS
dataset <- merge(dataset, news_vos_format_with_sentiment[,c("UT", "sentiment")], by="UT")
dataset <- merge(dataset, news[,c("AN", "TD")], by.x="UT", by.y="AN", all.x=TRUE)
dataset$AN <- NULL
rm(list=setdiff(ls(), "dataset"))
save.image("data.rdata")   
getwd()
rm("dataset")

# Then, move the data.rdata object to the /bibliometrics folder

#########################################################################
# Approach 2.
# To be used with `settings_facets.R`
# We need the dataset having the X_C column assigned from Approach 1.
# While the environment is still loaded we do:
dataset$X_C_name <- as.character(dataset$X_C)
save(dataset, file = "facet_dataset.rdata")
getwd()

# Then, we adjust the `settings_facet.rdata`
