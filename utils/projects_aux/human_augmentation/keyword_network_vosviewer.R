# This code was created to analyze the keywords of the Human Aumentation project
# And we used the keyword analysis in VosViewer to generate a keywords network
# From where clusters were extracted and these clusters served as basis for the mapping.

sankey <- read_csv("~/Library/CloudStorage/GoogleDrive-cristianmejia00@gmail.com/My Drive/Bibliometrics_Drive/H006_Human-Aug_Sust_Wellbeing_QoL/sankey_df_with_deadends_0.33_selected.csv")


# Selected HA clusters for keyword analysis
# - Those in the sankey
# - Those that are relevant
dataset_ha <- dataset
rcs_merged_ha <- rcs_merged
dataset_ha$label <- gsub("---$", "", dataset_ha$subcluster_label1)
dataset_ha$label <- paste("HA-", dataset_ha$label, sep = "")
dataset_ha$label[1:10]

rcs_llm <- cluster_summary_assessment

valid_topic_names <- unique(sankey$source_topic)
valid_clusters <- rcs_llm$cluster_code[rcs_llm$cluster_name %in% valid_topic_names]
valid_clusters

# Get the dataset to be used in vos viewer having only selected clusters
dataset_filtered <- dataset %>% filter(cluster_code %in% valid_clusters)
dataset_filtered <- dataset_filtered[,-c(1:21)]
dataset_filtered$X_E <- NULL

# Save dataset for vosviewer
write.csv(dataset_filtered, file = "dataset_filtered_HA_VOSVIEWER.csv", row.names = FALSE)


# ------------------------------------------
# Selected clusters from the other datasets 
# We load 1 by one and rename the objects
dataset_sust <- dataset
rcs_merged_sust <- rcs_merged
dataset_sust$label <- gsub("---$", "", dataset_sust$subcluster_label1)
dataset_sust$label <- paste("Sust-", dataset_sust$label, sep = "")
dataset_sust$label[1:10]


dataset_qol <- dataset
rcs_merged_qol <- rcs_merged
dataset_qol$label <- gsub("---$", "", dataset_qol$subcluster_label1)
dataset_qol$label <- paste("QoL-", dataset_qol$label, sep = "")
dataset_qol$label[1:10]

dataset_wb <- dataset
rcs_merged_wb <- rcs_merged
dataset_wb$label <- gsub("---$", "", dataset_wb$subcluster_label1)
dataset_wb$label <- paste("WB-", dataset_wb$label, sep = "")
dataset_wb$label[1:10]


# Get the valid clusters
valid_topic_names <- unique(sankey$Dest[sankey$Similarity >= 0.40])
valid_clusters <- sapply(valid_topic_names, function(x) {
  tt <- strsplit(x, ": ") %>% unlist()
  return(tt[[1]])
})
valid_clusters <- valid_cluster[!grepl("99$", valid_clusters)]
valid_clusters


# Get the dataset to be used in vos viewer having only selected clusters
dt_sust <- dataset_sust %>% filter(label %in% valid_clusters) %>% sample_n(2500)
dt_qol <- dataset_qol %>% filter(label %in% valid_clusters) 
dt_wb <- dataset_wb %>% filter(label %in% valid_clusters)

colnames(dt_sust)
colnames(dt_qol)
colnames(dt_wb)
valid_header <- union(colnames(dt_sust), colnames(dt_qol))
valid_header <- union(valid_header, colnames(dt_wb))
dataset_filtered <- rbind.fill(dt_sust, dt_qol, dt_wb)
dataset_filtered <- dataset_filtered[,-c(1:21)]
dataset_filtered$X_E <- NULL

# Save dataset for vosviewer
write.csv(dataset_filtered, file = "dataset_filtered_Sust_QuL_WB_VOSVIEWER.csv", row.names = FALSE)



#####################
vos_assessment <- as.data.frame(vos_assessment)
my_regex <- lapply(c(1:9), function(x) {
  ttt <- vos_assessment$label[vos_assessment$cluster2 == x]
  ttt <- ttt[!is.na(ttt)]
  ttt <- paste(ttt, collapse = "|")
  ttt
})

ha_text <- tolower(paste(dataset$AB, dataset$TI, dataset$ID, dataset$ED, text = ". "))

dataset_ha$cluster2 <- ""
for (i in c(1:9)) {
  dataset_ha$cluster2[grepl(my_regex[[i]], ha_text)] <- i
}

for (i in c(1:9)) {
  dataset_ha[paste("tt", i, sep="")] <- grepl(my_regex[[i]], ha_text)
}

presence <- table(dataset_ha$subcluster_label1, dataset_ha$cluster2) %>% as.data.frame.array()

presence_correct <- dataset_ha %>%
  group_by(subcluster_label1) %>%
  summarise(
    tt1_count = sum(tt1, na.rm = TRUE),
    tt2_count = sum(tt2, na.rm = TRUE),
    tt3_count = sum(tt3, na.rm = TRUE),
    tt4_count = sum(tt4, na.rm = TRUE),
    tt5_count = sum(tt5, na.rm = TRUE),
    tt6_count = sum(tt6, na.rm = TRUE),
    tt7_count = sum(tt7, na.rm = TRUE),
    tt8_count = sum(tt8, na.rm = TRUE),
    tt9_count = sum(tt9, na.rm = TRUE)
  )

write.csv(presence_correct, file="presence_correct.csv")

#######################
# selection datasets
clt <- "22-0---"
pp <- 2
yyy <- dataset_ha %>% 
  filter(
    subcluster_label1 == clt,
    cluster2 == pp
  ) %>% 
  select(subcluster_label1, AU, PY, Z9, DI, TI, AB, SO)
write.csv(yyy, file = paste("HA-", clt, pp, ".csv"), row.names = FALSE)



xxx <- dataset_ha %>% filter(dataset_ha$DI == "https://doi.org/10.1007/s11948-009-9142-5")
