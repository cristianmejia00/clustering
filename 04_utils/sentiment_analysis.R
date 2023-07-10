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

news_vos_format_with_sentiment <- readr::read_csv(file.choose())
news <- readr::read_csv(file.choose()) #news_factiva_format.csv

################################################
# Adding the sentiment score to the article report
article_report_with_sent <- merge(article_report, news_vos_format_with_sentiment[,c("UT", "C1", "sentiment")], by.x = "ID", by.y="UT", all.x = TRUE)
#write.csv(article_report_with_sent, file = "article_report_with_sent.csv", row.names = FALSE)

# Adding the news text
article_report_with_sent_and_text <- merge(article_report_with_sent, news[,c("AN", "LP_clean", "TD")], by.x="ID", by.y="AN", all.x=TRUE)

article_report_with_sent_and_text <- article_report_with_sent_and_text[,c("X_C", "related_topics", "Year", "Headline", "LP_clean", "TD", "Newspaper", "Score2", "sentiment", "Keywords", "C1", "Author", "ID")]
colnames(article_report_with_sent_and_text) <- c("Cluster", "Related clusters", "Year", "Headline", "Lead Paragraph", "Contents", "Newspaper", "Topic Score", "Sentiment", "Factiva topics", "Regions", "Types", "ID")

write.csv(article_report_with_sent_and_text, file = file.path(output_folder_level, "article_report_with_sent_and_text.csv"), row.names = FALSE)



################################################
# Adding topic level sentiment to the RCS
sent_compound_mean <- tapply(article_report_with_sent$sentiment, article_report_with_sent$X_C, mean)
sent_compound_median <- tapply(article_report_with_sent$sentiment, article_report_with_sent$X_C, median)


# Cluster sentiments
cluster_sentiments <- data.frame("cluster" <- c(1:K),
                                 "compound_mean" <- sent_compound_mean,
                                 "compound_median" <- sent_compound_median)

rcs <- rcs[order(rcs$cluster),]
rcs$sentiment_mean <- cluster_sentiments$X.compound_mean.....sent_compound_mean
rcs$sentiment_median <- cluster_sentiments$X.compound_median.....sent_compound_median

rcs_sent <- rcs[,c("cluster", "cluster_size", "sentiment_mean", "sentiment_median", "hub_title", "SO", "AU", "DE")]
colnames(rcs_sent) <- c("Cluster", "Cluster Size",	"Sentiment Mean", "Sentiment Median",	"Hub Title", "Newspapers", "News types", 	"News categories")
write.csv(rcs_sent, file = file.path(output_folder_level, "rcs_sentiment.csv"), row.names = FALSE)

###############################################
# Sentiment Plots:
###############################################
# Boxplots sorted from the most negative to most positive
# Based on medians; ties are broken with the mean

library("reshape2")
library("ggplot2")

# Plots
sentiment_long <- article_report_with_sent[,c("X_C", "sentiment")]
sentiment_long$X_C <- as.character(sentiment_long$X_C)
sentiment_long$X_C <-factor(sentiment_long$X_C, levels = as.character(order(sent_compound_median, sent_compound_mean)))

bp <- ggplot(sentiment_long, aes(x = X_C, y = sentiment)) + geom_boxplot() + xlab("Cluster") + ylab("Sentiment")
#bp + coord_flip()
if (K > 20) {
  bp <- bp + theme(axis.text.x = element_text(size = 6, angle = 90, vjust = 0.5, hjust=1))
}
bp
ggsave(file.path(output_folder_level, "fig_clusters_sentiment_boxplot.jpg"))

############################################
# Sentiment x size
library("ggrepel")

p <- ggplot(rcs, aes(x=sentiment_mean, y=cluster_size)) + geom_point() + xlab("Sentiment") + ylab("Cluster Size")
p + geom_text(aes(label=cluster))
p + geom_text_repel(aes(label=cluster))
ggsave(file.path(output_folder_level, "fig_clusters_sent_x_size.jpg"))

############################################
save.image(file.path(output_folder_level, "environ_sentiment.rdata"))
