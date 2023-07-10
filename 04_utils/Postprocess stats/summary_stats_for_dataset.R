# 20210807

# Additional summaries for clusters and overall dataset
dataset_2 <- dataset 
dataset_2$ctrs <- getCountries(dataset_2$C1)

load("dataset_3_opt7.rdata")
dataset_3 <- dataset_3[dataset_3$ai,]
dataset_3 <- merge(dataset_2, dataset_3[,c("UT", "abs_rank", "abs_field", "ai")], by = "UT", all.x = TRUE, all.y = FALSE)


# Cluster summaries added to RCS
rcs_complete_2 <- cbind(rcs_complete,   table(dataset_3$X_C, dataset_3$abs_rank) %>% as.matrix() %>% as.data.frame.matrix())
rcs_complete_2 <- cbind(rcs_complete_2, table(dataset_3$X_C, dataset_3$abs_field) %>% as.matrix() %>% as.data.frame.matrix())
rcs_complete_2 <- cbind(rcs_complete_2, table(dataset_3$X_C, dataset_3$ai) %>% as.matrix() %>% as.data.frame.matrix())
write.csv(rcs_complete_2, file = PROJECTrcs2, row.names = FALSE)

# Overall summary as a singe file
ttt <- rbind(
  TopSomething(dataset_2, "ctrs", 15) %>% data.frame(type = "Countries") %>% setnames(c("Value", "Freq", "Type")),
  TopSomething(dataset_2, "SO", 15) %>% data.frame(type = "Journals") %>% setnames(c("Value", "Freq", "Type")),
  TopSomething(dataset_2, "WC", 15) %>% data.frame(type = "Categories") %>% setnames(c("Value", "Freq", "Type")),
  TopInstitutions(dataset_2) %>% data.frame(type = "Institutions") %>% setnames(c("Value", "Freq", "Type")),
  table(dataset_3$PY) %>% data.frame(type = "Years") %>% setnames(c("Value", "Freq", "Type")),
  table(dataset_3$abs_field) %>% data.frame(type = "ABS Fiels") %>% setnames(c("Value", "Freq", "Type")))
write.csv(ttt, file = paste(PROJECT, "_summary.csv", sep = ""), row.names = FALSE)


dataset$PY %>% table()


ttt <- table(dataset$SO)
table(dataset$PT)
