tmp <- results_df_2 %>% filter(is_science_diplomacy)
tmp <- tmp %>% filter(!(nchar(TI) > 50 & duplicated(TI)))

dataset <- dataset %>% filter(UT %in% tmp$UT)



# Read AI annotations
results_df_2 <- read_csv("~/Library/CloudStorage/GoogleDrive-cristianmejia00@gmail.com/My Drive/Science_Diplomacy/results/20250903/results_df_2.csv")

# Merge both
dataset1 = merge(article_report,
                results_df_2 %>% select(-TI),
                by.x = 'ID',
                by.y = 'UT',
                all.x = TRUE,
                all.y = FALSE)

# Check columns
colnames(dataset1)


# save
write.csv(dataset1, 
          row.names = FALSE,
          file = "~/Library/CloudStorage/GoogleDrive-cristianmejia00@gmail.com/My Drive/Bibliometrics_Drive/Q337_science_diplomacy/a01_tm__f01_e01__hdbs/level0/article_report_annotated.csv")

write.csv(dataset1 %>% filter(is_science_diplomacy), 
          row.names = FALSE,
          file = "~/Library/CloudStorage/GoogleDrive-cristianmejia00@gmail.com/My Drive/Bibliometrics_Drive/Q337_science_diplomacy/a01_tm__f01_e01__hdbs/level0/article_report_annotated_TRUE.csv")

write.csv(dataset1 %>% filter(!is_science_diplomacy), 
          row.names = FALSE,
          file = "~/Library/CloudStorage/GoogleDrive-cristianmejia00@gmail.com/My Drive/Bibliometrics_Drive/Q337_science_diplomacy/a01_tm__f01_e01__hdbs/level0/article_report_annotated_FALSE.csv")
