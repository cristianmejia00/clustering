# Sentiment Analysis
# 2022-05-11

# Sentiment Analysis was computed using the Python Colab notebook "Sentiment Analysis"
# https://colab.research.google.com/drive/1clMpuBvB3m0cgfEuFjNBLgguS7n-kub9

# There, we created a file where each news was attached the sentiment-related columns.
# The file is named "news_vos_format_with_sentiment".

# Requirements (Inputs)
# The environment after creating a Topic Model. or at least the
# - RCS
# - article_report
# - news

# This one need to be loaded directly
# - news_vos_format_with_sentiment

library("reshape2")
library("ggplot2")
library("ggrepel")

################################################
# Adding topic level sentiment to the RCS
file.choose()
dataset_entity <- read.csv("palm_oil_news_entities_ready_to_use_in_research.csv")
dataset_entity$standard_name <- tolower(dataset_entity$standard_name)
dataset_entity$standard_name <- gsub("r.s.p.o", "rspo", dataset_entity$standard_name)
dataset_entity <- dataset_entity[dataset_entity$standard_name %in% rcs$X_C_name, ]
length(unique(dataset_entity$standard_name))
setdiff(rcs$X_C_name, unique(dataset_entity$standard_name))
dataset_entity$X_C_name <- dataset_entity$standard_name
dataset_entity$X_C <- rcs$cluster[match(dataset_entity$X_C_name, rcs$X_C_name)]
dataset_entity <- merge(dataset_entity, dataset[, c("UT", "PY")], by.x = "AN", by.y = "UT", all.x = TRUE)
dataset_entity$AN_firm <- paste(dataset_entity$AN, dataset_entity$X_C_name, sep = "_")
dataset_entity <- dataset_entity[!duplicated(dataset_entity$AN_firm), ]

write.csv(dataset_entity, file = "palm_oil_entities_cleaned.csv", row.names = FALSE)




sent_compound_mean <- tapply(dataset_entity$sentiment, dataset_entity$X_C_name, mean, na.rm = TRUE)
sent_compound_median <- tapply(dataset_entity$sentiment, dataset_entity$X_C_name, median, na.rm = TRUE)


# Cluster sentiments
cluster_sentiments <- data.frame(
  "cluster" <- names(sent_compound_mean),
  "compound_mean" <- sent_compound_mean,
  "compound_median" <- sent_compound_median
)
colnames(cluster_sentiments) <- c("firm", "mean", "median")


rcs$sentiment_entity_mean <- cluster_sentiments$mean[match(rcs$X_C_name, cluster_sentiments$firm)]
rcs$sentiment_entity_median <- cluster_sentiments$median[match(rcs$X_C_name, cluster_sentiments$firm)]

write.csv(rcs, file = file.path(output_folder_level, "rcs_sentiment_all.csv"), row.names = FALSE)

#######################################################################
# Sentiment Plots:
#######################################################################
# if (!(settings$params$unit_of_analysis %in% c("topic", "topics", "cluster", "clusters"))) {
#   if (!exists("rcs_tmp")) {
#     print("Excute zz-rcs_scatterplot.R to plot properly")
#   }
# } else {
#   rcs_tmp <- rcs
#   dataset_tmp <- dataset
# }

rcs_tmp <- rcs
dataset_tmp <- dataset_entity

# Boxplots sorted from the most negative to most positive
# Based on medians; ties are broken with the mean

# Plots
sentiment_long <- dataset_tmp[, c("X_C_name", "sentiment")]
sentiment_long$X_C <- as.character(sentiment_long$X_C_name)
sentiment_long$X_C <- factor(sentiment_long$X_C_name, levels = as.character(names(sent_compound_mean)[order(sent_compound_median, sent_compound_mean)]))

bp <- ggplot(sentiment_long, aes(x = X_C, y = sentiment)) +
  geom_boxplot(width = 0.7, fill = "deepskyblue3") +
  xlab("Cluster") +
  ylab("Sentiment")
if (!(settings$params$unit_of_analysis %in% c("topic", "topics", "cluster", "clusters"))) {
  bp + coord_flip()
}
if (K > 20) {
  bp <- bp + theme(axis.text.x = element_text(size = 6, angle = 90, vjust = 0.5, hjust = 1))
}
bp
ggsave(file.path(output_folder_level, "fig_clusters_sentiment_boxplot.jpg"))

#######################################################################
# Sentiment x size

p <- ggplot(rcs_tmp, aes(x = sentiment_mean, y = cluster_size)) +
  geom_point() +
  xlab("Sentiment") +
  ylab("Cluster Size")
# p + geom_text(aes(label=cluster))
p + geom_text_repel(aes(label = cluster))
ggsave(file.path(output_folder_level, "fig_clusters_sent_x_size.jpg"))

#######################################################################
# Sentiment x years

p <- ggplot(rcs_tmp, aes(x = sentiment_mean, y = cluster_year)) +
  geom_point() +
  xlab("Ave. Sentiment") +
  ylab("Ave. Year")
# p + geom_text(aes(label=cluster))
p + geom_text_repel(aes(label = cluster))
ggsave(file.path(output_folder_level, "fig_clusters_sent_x_year.jpg", sep = ""))

#######################################################################
# Sentiment trend reports
#######################################################################
# 20221201
# Input the "dataset"

# Add the X_C_name column to dataset from file or from
if (!("X_C_name") %in% colnames(dataset)) {
  dataset$X_C_name <- as.character(dataset$X_C)
}

# Summaries over time
facet_cluster <- dataset[!duplicated(dataset$X_C), c("X_C_name", "X_C")]
news_facet_year <- table(dataset$X_C, dataset$PY) %>%
  as.matrix() %>%
  as.data.frame.matrix()

facet_cluster_sorted <- facet_cluster[order(facet_cluster$X_C), ]
news_facet_year <- cbind(facet_cluster_sorted, news_facet_year)

# summarize sentiment
test_long <- dataset %>%
  group_by(X_C_name, PY) %>%
  summarise(sentiment_mean = mean(sentiment, na.rm = TRUE), sentiment_median = median(sentiment, na.rm = TRUE)) %>%
  ungroup()

# Widen them
facet_sent_mean <- reshape2::dcast(test_long, X_C_name ~ PY, value.var = "sentiment_mean")
facet_sent_median <- reshape2::dcast(test_long, X_C_name ~ PY, value.var = "sentiment_median")

# Add the ids, this just adds the X_C
facet_sent_mean <- merge(facet_sent_mean, facet_cluster, by = "X_C_name")
facet_sent_median <- merge(facet_sent_median, facet_cluster, by = "X_C_name")

# Format for printing
facet_sent_mean[is.na(facet_sent_mean)] <- 0
facet_sent_median[is.na(facet_sent_median)] <- 0
facet_sent_mean <- facet_sent_mean[order(facet_sent_mean$X_C), ]
facet_sent_median <- facet_sent_median[order(facet_sent_median$X_C), ]

# News per year
news_per_year <- table(dataset$PY) %>%
  as.matrix() %>%
  t() %>%
  as.data.frame.matrix()

# Positive, Negative, Neutral news per year
dataset$sentiment_factor <- "neutral"
dataset$sentiment_factor[dataset$sentiment > 0] <- "positive"
dataset$sentiment_factor[dataset$sentiment < 0] <- "negative"
sentiment_factor_per_year <- table(dataset$sentiment_factor, dataset$PY) %>%
  as.matrix() %>%
  as.data.frame.matrix()

# Ave. Sentiment per year
sentiment_per_year <- tapply(dataset$sentiment, dataset$PY, mean, na.rm = TRUE) %>%
  as.matrix() %>%
  t() %>%
  as.data.frame.matrix()

# Merge the 3 above.
year_summary <- rbind(news_per_year, sentiment_factor_per_year, sentiment_per_year)
rownames(year_summary) <- c("News", "Negative news", "Neutral news", "Positive news", "Average sentiment")

# Write reports
write.csv(news_facet_year, file.path(output_folder_level, "facet_news_per_year.csv"), row.names = FALSE)
write.csv(facet_sent_mean, file.path(output_folder_level, "facet_sentiment_mean_per_year.csv"), row.names = FALSE)
write.csv(facet_sent_median, file.path(output_folder_level, "facet_sentiment_median_per_year.csv"), row.names = FALSE)
write.csv(year_summary, file.path(output_folder_level, "year_sentiment_summary.csv"))
getwd()


############################################
save.image(file.path(output_folder_level, "environ_sentiment.rdata"))
