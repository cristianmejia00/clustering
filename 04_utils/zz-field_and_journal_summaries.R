# 20211019
# Computing ABS classes for selected dataset

# Input:
# 1.- An environment from recursive clustering code. Because this code is for the overal dataset and not the clusters
# any level environment will work.
# 2.- The file: C:\Users\crist\OneDrive\Documentos\00-Research projects\51 - Poverty and Operations Management\04-Materials\selected_journals.csv
# OR, any other .csv with the list of ABS journals with their fields and rankins.

# Output:
# The journals summary. a .csv file with the list of journals and their article frequency. Also with the column of ABS Field
# ABS field summary.
# WOS WC summary
selected_journals <- read.csv(file.choose(), header = TRUE, stringsAsFactors = FALSE)

selected_journals$journals_correct <- tolower(selected_journals$Journal.Title.CORRECTED)
selected_journals$journals_correct <- gsub("[[:punct:]]", "", selected_journals$journals_correct)
selected_journals$journals_correct

dataset$journals_correct <- tolower(dataset$SO)
dataset$journals_correct <- gsub("[[:punct:]]", "", dataset$journals_correct)
dataset$journals_correct[dataset$journals_correct == "r  d management"] <- "rd management"
table(dataset$journals_correct)


dataset_2 <- merge(dataset, selected_journals, by = "journals_correct", all.x = TRUE, all.y = FALSE)
dataset_2$Field[dataset_2$journals_correct == "journal of computermediated communication"] <- "INFO MAN"
dataset_2$Field[dataset_2$journals_correct == "omegainternational journal of management science"] <- "OR&MANSCI"
dataset_2$Field[dataset_2$journals_correct == "rd management"] <- "INNOV"
dataset_2$Field[dataset_2$journals_correct == "transportation research part apolicy and practice"] <- "SECTOR"
dataset_2$Field[dataset_2$journals_correct == "transportation research part bmethodological"] <- "SECTOR"
dataset_2$Field[dataset_2$journals_correct == "transportation research part dtransport and environment"] <- "SECTOR"
dataset_2$Field[dataset_2$journals_correct == "transportation research part elogistics and transportation review"] <- "SECTOR"

dataset <- dataset_2
# dataset_usable <- dataset_2[dataset_2$Field %in% c("SECTOR", "OPS&TECH", "OR&MANSCI"),]
# dataset_usable <- dataset_usable[,-c(1:14,52:66)]
# write_for_fukan(dataset_usable, file_name = "dataset_usable.tsv")



# Field summary
table(is.na(dataset_2$Field))
abs_fields <- table(dataset_2$Field)
write.csv(abs_fields, file = "abs_fields.csv", row.names = FALSE)


# Journals summary
journals <- table(dataset_2$journals_correct) %>% sort(decreasing = TRUE) %>% as.data.frame()
journals$field <- dataset_2$Field[match(journals$Var1, dataset_2$journals_correct)]
write.csv(journals, file = "journal_summary.csv", row.names = FALSE)

# WC summary
wc_cats <- strsplit(dataset_2$WC, "; ") %>% unlist %>% table %>% sort(decreasing = TRUE)
write.csv(wc_cats, file = "wc_categories.csv", row.names = FALSE)

tmp <- TopSomething(ttt, coll = "WC", top = 20) %>% sort() %>% as.data.frame()
colnames(tmp) <- c("Categories", "Articles")
ggplot(tmp, aes(x=Categories, y=Articles)) + 
  geom_bar(fill=rgb(0.1,0.4,0.5,0.9), stat = "identity") +
  coord_flip()

