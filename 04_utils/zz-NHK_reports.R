

#("tv" OR "t.v." OR "radio" OR "media" OR "multimedia" OR "digital" OR "video" OR "movie" OR "film" OR "documentary" OR "news" OR "music" OR "streaming" OR "production company")
media_query <- "tv|radio|media|multimedia|digital|video|movie|film|documentary|news|music|streaming|production company"
all_query <- "tv|radio|media|multimedia|digital|video|movie|film|documentary|news|music|streaming|production company"
mytext <- paste(dataset$TI, dataset$AB, sep = " ") %>% tolower()

dataset$serendipity <- grepl("serendip", mytext)
dataset$media <- grepl(media_query, mytext)
dataset$all_query <- dataset$serendipity & dataset$media

extra_data <- data.frame("cluster" = c(1:12),
                         "serendipity_papers" = tapply(dataset$serendipity, dataset$X_C, sum, na.rm=TRUE),
                         "media_papers" = tapply(dataset$media, dataset$X_C, sum, na.rm=TRUE),
                         "both" = tapply(dataset$all_query, dataset$X_C, sum, na.rm=TRUE))

write.csv(extra_data, "extra_data_CL3.csv", row.names = FALSE)

dataset_subset <- dataset[dataset$all_query,]
dataset_subset$...1 <- NULL
write.csv(dataset_subset, "serendipity_media_papers_CL3.csv", row.names = FALSE)
getwd()

# 
# dataset <- dataset[dataset$X_C == 3,]
# colnames(dataset)
# dataset$...1 <- NULL
# dataset$X_C <- NULL
# dataset <- dataset[,c(1:19)]
# save(dataset, file = "dataset.rdata")

myDataCorrect$X_C %>% table()
dataset$X_C %>% table

write.csv(dataset[,c("UT", "X_C")], "papers_subclusters.csv", row.names = FALSE)
write.csv(dataset, "cluster3_papers.csv", row.names = FALSE)
colnames(dataset)


tmp <- dataset[grepl("bbc", tolower(dataset$TI)),]
tmp <- dataset[grepl("bbc", tolower(dataset$AB))|grepl("bbc", tolower(dataset$TI)),]
table(tmp$X_C)
write.csv(tmp, "bbc.csv", row.names = FALSE)
