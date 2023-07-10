##################################################################################################
# Get the "smaller" dataset to be used in Fukan System
##################################################################################################
# Right after we get the results from the Fukan System for the large network we need to identify the subset of articles complying the criteria set for this article and detailed below:
# This code, helps identifyng such articles and created 3 sets to compare them in Fukan System
# The one only extracted from the MC of the large network, the one from orphans, and one having everything.
# The network from Orphans helsp identifying small disconnected clusters that are only visible in the orphans sections.
library(dplyr)
# Criteria:
# 1. articles published in Levels 4 and 4* journals (focus on 4 and 4* only justified by the rather large network already [204]);
# 2. articles published in Level 3 journals where these have EITHER been cited more than 100 times OR the journal has more than 5 articles on the topic;     
# 3. articles published in Level 2 journals where these have been cited more than 100 times.

# Read the ABS dataset (ABS >= level 2)
# C:\Users\crist\OneDrive\Documentos\Transfer from Google Drive\25 - Creativity\Suplementary material\abs_ranking_2018_class2to5.csv
# "C:\\Users\\crist\\OneDrive\\Documentos\\00-Research projects\\47 - Creativity in NPD\\Suplementary material\\abs_ranking_2018_class2to5.csv"
abs_journals <- read.csv("C:\\Users\\crist\\OneDrive\\Documentos\\00-Research projects\\47 - NPD Design\\04-Materials\\ABS Journal Guide\\ABS 2021 journal rankings.csv"
                         , stringsAsFactors = FALSE, check.names = FALSE)
abs_journals <- abs_journals[!duplicated(abs_journals$ISSN),]
abs_journals$abs_field <- abs_journals$Field
abs_journals$abs_rank <- abs_journals$"AJG 2021"
abs_journals$ISSN <- abs_journals[,1]
abs_journals[,1] <- NULL

# mission.facet all and orphans from here: https://academic-landscape.com/dataset/33077
dataset <- read.table(file.choose(), fill = TRUE, header = TRUE, stringsAsFactors = FALSE, quote = "", sep = "\t")
orphans <- read.table(file.choose(), fill = TRUE, header = TRUE, stringsAsFactors = FALSE, quote = "", sep = "\t")

all_dataset <- rbind.fill(dataset, orphans)
all_dataset <- as.data.frame(all_dataset)
all_dataset$MC <- !is.na(all_dataset$"X_C")
table(all_dataset$MC)

# Get the ISSN
abs_issn <- abs_journals$ISSN

# Find the count of articles per journal in our dataset
# We will be interested on journals having at least 5 articles on it
journal_5 <- data.frame(table(all_dataset$SN) %>% sort(.,decreasing = TRUE), stringsAsFactors = FALSE)
journal_5 <- journal_5[journal_5$Freq >= 5,]
journal_5 <- journal_5[-2,]

# Add the ABS ranking to the article report
all_dataset$ABS <- NULL
all_dataset     <- merge(all_dataset, abs_journals[c("ISSN", "ABS")], by.x = "SN", by.y = "ISSN", all.x = TRUE, all.y = FALSE)



# all_dataset     <- all_dataset[!duplicated(all_dataset$ID),]
all_dataset$ABS[is.na(all_dataset$ABS)] <- 0

# Highly cited (more or equal than 100)
all_dataset$Z9 <- as.numeric(all_dataset$Z9)
all_dataset$Z9[is.na(all_dataset$Z9)] <- 0
all_dataset$HC <- all_dataset$Z9 >= 100

# Does the journal has more than 5 articles on the topic
all_dataset$J5 <- all_dataset$SN %in% journal_5$Var1


# Check values
boxplot(dataset$Z9)
summary(dataset$Z9)
table(all_dataset$Z9 >= 7)
table(all_dataset$Z9 > 0, all_dataset$MC)
table(all_dataset$HC)
table(all_dataset$ABS)


# Judge if follow the criteria
# The article is either published in an ABS ranked 3+ journal or has more than average citations (i.e. >=15) 
all_dataset$criteria <- sapply(c(1:nrow(all_dataset)), function(x) {
  criteria <- if (all_dataset$ABS[x] >= 3 | all_dataset$Z9[x] >= 15) {TRUE} else {FALSE}
  return(criteria)
})

my_text <- paste(all_dataset$TI, all_dataset$AB, all_dataset$DE, all_dataset$ID, sep = " ") %>% tolower()





# Criteria_1 = the current. ABS >= 3 | Z9 >= 15 (98)
all_dataset$criteria_1 <- all_dataset$criteria

# Criteria_2 = ABS > 2 (62)
all_dataset$criteria_2 <- all_dataset$ABS >= 2

# Criteria_3 = WC == "Management|Business" (60)
all_dataset$criteria_3 <- grepl("Management|Business", all_dataset$WC)

# Criteria_4 = ABS >= 3 | (Z9 >= 15 & my_text == "firm|company|enterprise|corporation|organization") (49)
option_b <- (grepl("firm|company|enterprise|corporation|organization", my_text) & all_dataset$Z9 >= 15)
all_dataset$criteria_4 <- all_dataset$ABS >= 3 | option_b

# Criteria_5 = ABS >= 2 | (Z9 >= 15 & my_text == "firm|company|enterprise|corporation|organization") (69)
all_dataset$criteria_5 <- all_dataset$ABS >= 2 | option_b

# Criteria 6
all_dataset$criteria_6 <- all_dataset$ABS >= 2 | (grepl("Management|Business", all_dataset$WC) & all_dataset$Z9 > 15)


# Counts
table(all_dataset$criteria_1)
table(all_dataset$criteria_2)
table(all_dataset$criteria_3)
table(all_dataset$criteria_4)
table(all_dataset$criteria_5)
table(all_dataset$criteria_6)

table(all_dataset$criteria_2|all_dataset$criteria_3|all_dataset$criteria_4|all_dataset$criteria_5)
table(all_dataset$criteria_2&all_dataset$criteria_3&all_dataset$criteria_4&all_dataset$criteria_5)


table(all_dataset$X_C, all_dataset$ABS >= 3)

write.csv(all_dataset[all_dataset$criteria_2&all_dataset$criteria_3&all_dataset$criteria_4&all_dataset$criteria_5,], file = "article_report_core.csv", row.names = FALSE)
getwd()
###############################################################################################
# Option B: When we want to analyse the participation of criteria articles within the large network of Creativity
# i.e. We will retain the clusters and subclusters solutions of the large creativity network
# But we will generate reports based only on the criteria articles.

ABS_critera_converter <- all_dataset$criteria
names(ABS_critera_converter) <- all_dataset$UT

save(ABS_critera_converter, file = "ABS_criteria_converter.Rdata")


##################################################################################################
# General reports
##################################################################################################
# Large network MC vs Orphans (We expect it correspond to the Fukan System nodes)
table(all_dataset$MC)

# Articles from ABS journals ranked 2 or more, in the MC and Orphans
table(all_dataset$ABS >= 2, all_dataset$MC)

# Articles within criteria, in the MC and Orphans
table(all_dataset$criteria, all_dataset$MC)

##################################################################################################
# Save the selection criteria as an object
##################################################################################################
index_criteria <- all_dataset[,c("UT", "criteria")]
save(index_criteria, file = "index_criteria.rdata")
getwd()