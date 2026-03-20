# Alternative Meat.
# Merging the datasets

library(readr)

foresight <- read_csv("~/Desktop/GitHub/PENDING/alternative_meats/output/annotated_papers.csv")
table(foresight$foresight)
table(foresight$protein_intake)

# Subcluster
article_report <- read_csv("~/Library/CloudStorage/GoogleDrive-cristianmejia00@gmail.com/My Drive/Bibliometrics_Drive/Q340_alternative_meat/a01_cn__f01_dc__c01_lv/louvain/0.9/level1/article_report_20251105.csv")
article_report <- read_csv("~/Library/CloudStorage/GoogleDrive-cristianmejia00@gmail.com/My Drive/Bibliometrics_Drive/Q340_alternative_meat/a01_cn__f01_dc__c01_lv/louvain/0.9/level1/article_report_20_20251105.csv")

# Cluster
article_report <- read_csv("~/Library/CloudStorage/GoogleDrive-cristianmejia00@gmail.com/My Drive/Bibliometrics_Drive/Q340_alternative_meat/a01_cn__f01_dc__c01_lv/louvain/0.9/level0/article_report_20251105.csv")
article_report <- read_csv("~/Library/CloudStorage/GoogleDrive-cristianmejia00@gmail.com/My Drive/Bibliometrics_Drive/Q340_alternative_meat/a01_cn__f01_dc__c01_lv/louvain/0.9/level0/article_report_20_20251105.csv")

# Merge
article_report_merged <- merge(article_report,
      foresight %>% select(paper_id, foresight, protein_intake),
      by.x = "ID",
      by.y = "paper_id",
      all.x = TRUE,
      all.y = FALSE)
colnames(article_report_merged)

# Remove column
article_report_merged$paper_id <- NULL

# Save
write.csv(article_report_merged, 
          file = "~/Library/CloudStorage/GoogleDrive-cristianmejia00@gmail.com/My Drive/Bibliometrics_Drive/Q340_alternative_meat/a01_cn__f01_dc__c01_lv/louvain/0.9/level0/article_report_20_20251211.csv",
          row.names = FALSE)

###########
