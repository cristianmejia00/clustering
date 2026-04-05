print("###################### reports/03_general_summary.R")

# Initialize orphans if not present
if (!exists("orphans")) orphans <- data.frame()

total_articles <- nrow(orphans) + nrow(dataset)

# Build summary as a named list
general_summary <- list(
  total_articles  = total_articles,
  nodes           = nrow(dataset),
  orphans         = nrow(orphans),
  orphans_percent = round(nrow(orphans) / total_articles, 2)
)

# Add analysis-specific counts
if (settings$params$type_of_analysis %in% c("citation_network", "both")) {
  general_summary$edges <- ecount(g1)
  if (settings$params$recursive_level > 0) {
    general_summary$clusters_level0 <- nrow(edges_level1)
    general_summary$clusters_level1 <- if (settings$params$recursive_level >= 1) nrow(edges_level2) else 0
    general_summary$clusters_level2 <- if (settings$params$recursive_level >= 2) nrow(edges_level3) else 0
  }
} else {
  general_summary$clusters <- tmo$K
}

# Write as a human-readable text file
summary_lines <- paste(names(general_summary), general_summary, sep = ": ")
writeLines(summary_lines, file.path(network_folder_path, "general_summary.txt"))
