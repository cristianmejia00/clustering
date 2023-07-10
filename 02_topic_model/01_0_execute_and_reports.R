
####################################################
# Reports
####################################################
# Verify dataset is correctly formatted
source("03_reports/00_verify_data.R")

# Generate report names
source("03_reports/00_names.R")

# Load helper functions for reports
source("02_topic_model/04_functions.R")

# Auxiliary functions
source("03_reports/zz_auxiliary_functions.R")


library(ggplot2)

# Yearly trends
yearly_trends <- myDataCorrect$PY %>% table %>% data.frame %>% setNames(c("Year", "Articles"))
yearly_trends <- yearly_trends[order(yearly_trends$Year, decreasing = TRUE),]
write.csv(yearly_trends, file="stats_yearly_trends.csv", row.names = FALSE)

ggplot(yearly_trends, aes(x=Year, y=Articles)) + 
  geom_bar(stat = "identity", width = 0.7) #+ #, color = "black", fill = "white") +
  #theme(axis.text=element_text(size=14))
  #coord_flip() +
  #scale_x_discrete(limits=rev)
ggsave("fig_yearly_trends.jpg")

# Cluster size
stats_size <- myDataCorrect$X_C %>% table %>% sort(decreasing = TRUE) %>% data.frame() %>% setNames(c("Cluster", "Articles"))
write.csv(stats_size, file="stats_cluster_size.csv", row.names = FALSE)

ggplot(stats_size, aes(x=Cluster, y=Articles)) + 
  geom_bar(stat = "identity", width = 0.7) + #, color = "black", fill = "white") +
  #theme(axis.text=element_text(size=14)) +
  coord_flip() +
  scale_x_discrete(limits=rev)
ggsave("fig_cluster_size_h.jpg")

ggplot(stats_size, aes(x=Cluster, y=Articles)) + 
  geom_bar(stat = "identity", width = 0.7) #+ #, color = "black", fill = "white") +
  #theme(axis.text=element_text(size=14))
  #coord_flip() +
  #scale_x_discrete(limits=rev)
ggsave("fig_cluster_size_v.jpg")

# Top 15 newspapers 
stats_journals <- myDataCorrect$SO %>% as.character %>% toTitleCase %>% table %>% sort(decreasing = TRUE) %>% data.frame() %>% setNames(c("Journals", "Articles"))
write.csv(stats_journals, file="stats_journals.csv", row.names = FALSE)


ggplot(stats_journals[c(1:15),], aes(x=Journals, y=Articles)) + 
  geom_bar(stat = "identity", width = 0.7) + #, color = "black", fill = "white") +
  theme(axis.text=element_text(size=14)) +
  coord_flip() +
  scale_x_discrete(limits=rev)
ggsave("fig_newspapers.jpg")

# Top 15 regions
stats_regions <- myDataCorrect$C1 %>% strsplit("; ") %>% unlist %>% table %>% sort(decreasing = TRUE) %>% data.frame() %>% setNames(c("Regions", "News"))
write.csv(stats_regions, file="stats_regions.csv", row.names = FALSE)

ggplot(stats_regions[c(1:15),], aes(x=Regions, y=News)) + 
  geom_bar(stat = "identity", width = 0.7) + #, color = "black", fill = "white") +
  theme(axis.text=element_text(size=14)) +
  coord_flip() +
  scale_x_discrete(limits=rev)
ggsave("fig_regions.jpg")

# Top 15 factiva topics
stats_categories <- myDataCorrect$DE %>% strsplit("; ") %>% unlist %>% table %>% sort(decreasing = TRUE) %>% data.frame() %>% setNames(c("Category", "News"))
write.csv(stats_categories, file="stats_categories.csv", row.names = FALSE)

ggplot(stats_categories[c(1:15),], aes(x=Category, y=News)) + 
  geom_bar(stat = "identity", width = 0.7) + #, color = "black", fill = "white") +
  theme(axis.text=element_text(size=14)) +
  coord_flip() +
  scale_x_discrete(limits=rev)
ggsave("fig_categories.jpg")

# Top 15 factiva news types
stats_types <- myDataCorrect$AU %>% strsplit("; ") %>% unlist %>% table %>% sort(decreasing = TRUE) %>% data.frame() %>% setNames(c("Type", "News"))
write.csv(stats_types, file="stats_types.csv", row.names = FALSE)

ggplot(stats_types[c(1:15),], aes(x=Type, y=News)) + 
  geom_bar(stat = "identity", width = 0.7) + #, color = "black", fill = "white") +
  theme(axis.text=element_text(size=14)) +
  coord_flip() +
  scale_x_discrete(limits=rev)
ggsave("fig_types.jpg")

# Reports
source("03_reports/01_document_report.R")
source("03_reports/02_rcs.R")
source("03_reports/03_general_summary.R")
source("03_reports/04_cluster_reports.R")

# Keyword reports
theta_original <- theta
phi_original <- phi
recursive_level <- 0
level_report <- 0
source("03_reports/05_heatmap_keywords_part_1.R")
if (!(exists("myDataCorrect_SAMPLE") & exists("papersText"))) {
  source("03_reports/05_heatmap_keywords_part_2.R")
}
source("03_reports/05_heatmap_keywords_part_3.R")


source("03_reports/zz_createJSON_cnet.R")
source("03_reports/06_citation_to_topic_model_converter.R")
source("03_reports/07_prob_exclu_keywords.R")
source("03_reports/08_all_keywords_report.R")
# Optional views
#source("03_reports/09_keywords_per_clusters.R")
#source("03_reports/10_rcs_keywords.R")

# Save environmet
save.image(rn$PROJECTenviron)
