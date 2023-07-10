# 20181206
print("###################### reports/03_general_summary.R")

##  Create the orphans dataset if does not exists
if (!exists("orphans")) {
  orphans <- data.frame()
}

## Add common values
general_summary <- c(
  "total_articles" = nrow(orphans) + nrow(dataset),
  "nodes" = nrow(dataset),
  "orphans" = nrow(orphans),
  "orphans_percent" = round( nrow(orphans) / (nrow(orphans) + nrow(dataset)) , 2)
  )

## Add values depending the type of analysis
if (params$type_of_analysis == "citation_network") {
  general_summary <- c(
    general_summary,
    "edges" = ecount(g1),
    "clusters_level0" = nrow(edges_level1),
    "clusters_level1" = if (cno$recursive_level >= 1) {nrow(edges_level2)} else {0},
    "clusters_level2" = if (cno$recursive_level >= 2) {nrow(edges_level3)} else {0}
  )
} else {
  general_summary <- c(
    general_summary,
    "clusters" = tmo$K
  )
}

## Save the report
write.csv(as.data.frame.list(general_summary) %>% t(), 
          file=file.path(input_folder, analysis_metadata$query_id, "stats", "general_summary.csv"))
