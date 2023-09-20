table(dataset$fukan_X_C, dataset$X_C)

tmp <- table(dataset$fukan_X_C)
sapply(c(2:length(tmp)), function(x) {tmp[x] <= tmp[x-1]}) |> all()

tmp <- table(dataset$fukan_X_C) |> sort(decreasing = TRUE)
match(20, names(tmp))


cluster_comparison <- dataset[c('X_C', 'fukan_X_C', 'fukan_original_cluster_id', 'fukan_subcluster_label')]
cluster_comparison <- cluster_comparison[!duplicated(cluster_comparison$fukan_subcluster_label),]
cluster_comparison <- cluster_comparison[order(cluster_comparison$fukan_X_C),]
write.csv(cluster_comparison, file = file.path(output_folder_level, "cluster_id_comparison.csv"), row.names = FALSE)

table(dataset$X_C)
table(myDataCorrect$X_C)
table(myDataCorrect$level0)
table(myDataCorrect_SAMPLE$X_C)
