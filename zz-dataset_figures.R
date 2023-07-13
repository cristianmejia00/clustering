#####
##### DO NOT DELETE THIS CODE!!!!!!!!!!
#####



## BLUE bars
## AND INSTITUTIONS 

# Before the cluster level stats we need the dataset level stats

# Load plotting library
library(ggplot2)
rp <- data.frame("top_items" = 20)
dir.create(file.path(output_folder_reports, "stats")) 

################################################################################
# YEARLY TRENDS
################################################################################
yearly_trends <- dataset$PY %>% as.numeric %>% table %>% data.frame %>% setNames(c("Year", "Articles"))
yearly_trends <- yearly_trends[order(yearly_trends$Year, decreasing = TRUE),]
write.csv(yearly_trends, file=file.path(output_folder_reports, "stats", "data_yearly_trends.csv"), row.names = FALSE)

ggplot(yearly_trends[1:15,], aes(x=Year, y=Articles)) + 
  geom_bar(stat = "identity", width = 0.7, fill = "deepskyblue3") #+ #+ #, color = "black", fill = "white") +
#theme_classic()
#theme(axis.text=element_text(size=14))
#coord_flip() +
#scale_x_discrete(limits=rev)
ggsave(file.path(output_folder_reports, "stats", "fig_yearly_trends.jpg"))

################################################################################
# CLUSTER SIZE
################################################################################
# Sometimes 'X_C' others 'level0'?
stats_size <- dataset$X_C %>% table %>% data.frame() %>% setNames(c("Cluster", "Articles"))
write.csv(stats_size, file=file.path(output_folder_reports, "stats", "data_cluster_size.csv"), row.names = FALSE)

ggplot(stats_size, aes(x=Cluster, y=Articles)) + 
  geom_bar(stat = "identity", width = 0.7, fill = "deepskyblue3") + #, color = "black", fill = "white") +
  #theme(axis.text=element_text(size=14)) +
  coord_flip() +
  scale_x_discrete(limits=rev)
ggsave(file.path(output_folder_reports, "stats", "fig_cluster_size_h.jpg"))

ggplot(stats_size, aes(x=Cluster, y=Articles)) + 
  geom_bar(stat = "identity", width = 0.7, fill = "deepskyblue3") #+ #, color = "black", fill = "white") +
#theme(axis.text=element_text(size=14))
#coord_flip() +
#scale_x_discrete(limits=rev)
ggsave(file.path(output_folder_reports, "stats", "fig_cluster_size_v.jpg"))

################################################################################
# JOURNALS
################################################################################
stats_journals <- dataset$SO %>% as.character %>% tolower %>% toTitleCase %>% table %>% sort(decreasing = TRUE) %>% data.frame() %>% setNames(c("Journals", "Articles"))
write.csv(stats_journals, file=file.path(output_folder_reports, "stats", "data_journals.csv"), row.names = FALSE)


ggplot(stats_journals[c(1:rp$top_items),], aes(x=Journals, y=Articles)) + 
  geom_bar(stat = "identity", width = 0.7, fill = "deepskyblue3") + #, color = "black", fill = "white") +
  theme(axis.text=element_text(size=14)) +
  coord_flip() +
  scale_x_discrete(limits=rev)
ggsave(file.path(output_folder_reports, "stats", "fig_journals.jpg"))

################################################################################
# COUNTRIES
################################################################################
dataset$Countries <- getCountries(dataset$C1)
stats_regions <- dataset$Countries %>% strsplit("; ") %>% unlist %>% tolower %>% toTitleCase %>% gsub("Usa", "USA", .) %>% table %>% sort(decreasing = TRUE) %>% data.frame() %>% setNames(c("Regions", "Articles"))
write.csv(stats_regions, file=file.path(output_folder_reports, "stats", "data_regions.csv"), row.names = FALSE)

ggplot(stats_regions[c(1:rp$top_items),], aes(x=Regions, y=Articles)) + 
  geom_bar(stat = "identity", width = 0.7, fill = "deepskyblue3") + #, color = "black", fill = "white") +
  theme(axis.text=element_text(size=14)) +
  coord_flip() +
  scale_x_discrete(limits=rev)
ggsave(file.path(output_folder_reports, "stats", "fig_regions.jpg"))

################################################################################
# Institutions
################################################################################
if ("Institutions" %in% colnames(dataset)) {
  stats_institutions <- dataset$Institutions %>% tolower %>%  strsplit("; ") %>% unlist %>% toTitleCase  %>% gsub("Inst", "Inst.", .) %>% gsub("Univ", "Univ.", .) %>% table %>% .[!grepl("^Na$", names(.))] %>% sort(decreasing = TRUE) %>% data.frame() %>% setNames(c("Organizations", "Articles"))

  write.csv(stats_institutions, file=file.path(output_folder_reports, "stats", "data_organizations.csv"), row.names = FALSE)
  
  ggplot(stats_institutions[c(1:rp$top_items),], aes(x=Organizations, y=Articles)) + 
    geom_bar(stat = "identity", width = 0.7, fill = "deepskyblue3") + 
    theme(axis.text=element_text(size=14)) +
    coord_flip() +
    scale_x_discrete(limits=rev)
  ggsave(file.path(output_folder_reports, "stats", "fig_organizations.jpg"))
}


################################################################################
# CATEGORIES (or Patent IPC)
################################################################################
if ("WC" %in% colnames(dataset)) {
  stats_WC <- dataset$WC %>% tolower %>%  strsplit("; ") %>% unlist %>% toTitleCase %>% table %>% sort(decreasing = TRUE) %>% data.frame() %>% setNames(c("Categories", "Articles"))
  write.csv(stats_WC, file=file.path(output_folder_reports, "stats", "data_categories.csv"), row.names = FALSE)
  
  ggplot(stats_WC[c(1:rp$top_items),], aes(x=Categories, y=Articles)) + 
    geom_bar(stat = "identity", width = 0.7, fill = "deepskyblue3") + 
    theme(axis.text=element_text(size=14)) +
    coord_flip() +
    scale_x_discrete(limits=rev)
  ggsave(file.path(output_folder_reports, "stats", "fig_categories.jpg"))
}

################################################################################
# KEYWORDS (or Factiva topics)
################################################################################
if ("DE" %in% colnames(dataset)) {
  stats_DE <- dataset$DE %>% tolower %>% strsplit("; ") %>% unlist %>% toTitleCase %>% table %>% sort(decreasing = TRUE) %>% data.frame() %>% setNames(c("Keywords", "Articles"))
  #if (params$type_of_dataset == "news") { setnames(stats_DE, c("Keywords", "Articles"), c("Topic", "News"))}
  write.csv(stats_DE, file=file.path(output_folder_reports, "stats", "data_keywords.csv"), row.names = FALSE)
  
  ggplot(stats_DE[c(1:rp$top_items),], aes(x=Keywords, y=Articles)) + 
    geom_bar(stat = "identity", width = 0.7, fill = "deepskyblue3") + #, color = "black", fill = "white") +
    theme(axis.text=element_text(size=14)) +
    coord_flip() +
    scale_x_discrete(limits=rev)
  ggsave(file.path(output_folder_reports, "stats", "fig_keywords.jpg"))
}


################################################################################
# Authors (or Factiva types, or Patents inventors)
################################################################################
if ("AU" %in% colnames(dataset)) {
  stats_AU <- dataset$AU %>% toupper %>% strsplit("; ") %>% unlist %>% toTitleCase %>% table %>% sort(decreasing = TRUE) %>% data.frame() %>% setNames(c("Authors", "Articles"))
  #if (params$type_of_dataset == "news") { setnames(stats_DE, c("Authors", "Articles"), c("Types", "News"))}
  #if (params$type_of_dataset == "patents") { setnames(stats_DE, c("Authors", "Articles"), c("Inventors", "Patents"))}
  write.csv(stats_AU, file=file.path(output_folder_reports, "stats", "data_authors.csv"), row.names = FALSE)
  
  ggplot(stats_AU[c(1:rp$top_items),], aes(x=Authors, y=Articles)) + 
    geom_bar(stat = "identity", width = 0.7, fill = "deepskyblue3") + #, color = "black", fill = "white") +
    theme(axis.text=element_text(size=14)) +
    coord_flip() +
    scale_x_discrete(limits=rev)
  ggsave(file.path(output_folder_reports, "stats", "fig_authors.jpg"))
}
