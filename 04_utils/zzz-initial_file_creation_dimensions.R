# 20221206

# This code helps me to create the input file for clustering analysis using dimensions data
# Dimensions data is collected from the Colab.

# There we save 2 files: wos_format, and wos_format_cluster.
# We need to load these here using reader. 


# Dimensions data formatter
wos_format$...1 <- NULL
wos_format_clusters$...1 <- NULL

dataset <- merge(wos_format, wos_format_clusters, by="UT")
orphans <- wos_format[!(wos_format$UT %in% wos_format_clusters$UT),]
table(dataset$X_C)

save(dataset, orphans, file="dataset.rdata")
rm(list = ls())
