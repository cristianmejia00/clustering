
# Auxiliary functions
source("03_reports/zz_auxiliary_functions.R")

# Get dataset stats from Factiva
library(ggplot2)

# Yearly trends
yearly_trends <- myDataCorrect$PY %>% table %>% data.frame %>% setNames(c("Year", "Articles"))
yearly_trends <- yearly_trends[order(yearly_trends$Year, decreasing = TRUE),]
write.csv(yearly_trends, file="stats_yearly_trends.csv", row.names = FALSE)

ggplot(yearly_trends[2:16,], aes(x=Year, y=Articles)) + 
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

# Top 15 journals
stats_journals <- myDataCorrect$SO %>% as.character %>% toTitleCase %>% table %>% sort(decreasing = TRUE) %>% data.frame() %>% setNames(c("Journals", "Articles"))
write.csv(stats_journals, file="stats_journals.csv", row.names = FALSE)


ggplot(stats_journals[c(1:15),], aes(x=Journals, y=Articles)) + 
  geom_bar(stat = "identity", width = 0.7) + #, color = "black", fill = "white") +
  theme(axis.text=element_text(size=14)) +
  coord_flip() +
  scale_x_discrete(limits=rev)
ggsave("fig_journals.jpg")

# Top 15 countries
myDataCorrect$Country <- gsub("peoples r ", "", myDataCorrect$Country)
stats_countries <- myDataCorrect$Country %>% as.character %>% toTitleCase %>% table %>% sort(decreasing = TRUE) %>% data.frame() %>% setNames(c("Country", "Articles"))
write.csv(stats_countries, file="stats_countries.csv", row.names = FALSE)
stats_countries <- stats_countries[stats_countries$Country != "-",]

ggplot(stats_countries[1:15,], aes(x=Country, y=Articles)) + 
  geom_bar(stat = "identity", width = 0.7) + #, color = "black", fill = "white") +
  theme(axis.text=element_text(size=14)) +
  coord_flip() +
  scale_x_discrete(limits=rev)
ggsave("fig_countries.jpg")

# Top 15 Author keywords
stats_keywords <- myDataCorrect$DE %>% strsplit("; ") %>% unlist %>% toTitleCase %>% table %>% sort(decreasing = TRUE) %>% data.frame() %>% setNames(c("Keyword", "Articles"))
write.csv(stats_keywords, file="stats_author_keywords.csv", row.names = FALSE)

ggplot(stats_keywords[1:15,], aes(x=Keyword, y=Articles)) + 
  geom_bar(stat = "identity", width = 0.7) + #, color = "black", fill = "white") +
  theme(axis.text=element_text(size=14)) +
  coord_flip() +
  scale_x_discrete(limits=rev)
ggsave("fig_author_keywords.jpg")
