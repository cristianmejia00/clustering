library(dplyr)
library(glue)

# Create markdown content using glue
markdown_content <- rcs_merged2 %>%
  mutate(
    section = glue(
      "# Cluster {cluster_code}: {cluster_name}\n",
      "Documents: {documents}; Ave. Citations: {Z9_Mean}; Ave. Publication Year: {PY_Mean}\n\n",
      "{description}\n\n",
      "---\n"
    )
  ) %>%
  pull(section) %>%
  paste(collapse = "\n")

# Write to file
writeLines(markdown_content, "clusters_report.md")


##########################################################################
# Create bibtext entries
library(dplyr)
library(stringr)

# Step 1: Filter dataset to get top 3 cited papers per cluster
top_papers <- dataset %>%
  select(cluster_code, TI, AB, PY, Z9, DI, SO, AU) %>%
  group_by(cluster_code) %>%
  arrange(desc(Z9)) %>%
  slice_head(n = 3) %>%
  ungroup()

# Step 2: Function to create a BibTeX entry
create_bibtex_entry <- function(row, index) {
  # Create citation key (e.g., Author2023a)
  first_author <- str_extract(row$AU, "^[^;,]+") %>%
    str_replace_all("[^A-Za-z]", "") %>%
    str_to_title()
  citation_key <- paste0(first_author, row$PY, letters[index])
  
  # Clean and escape special characters if needed
  title <- str_replace_all(row$TI, "[{}]", "")
  abstract <- str_replace_all(row$AB, "[{}]", "")
  
  # Format authors (convert ; or , to 'and')
  authors <- str_replace_all(row$AU, "[;,]", " and")
  
  # Build BibTeX entry
  bibtex <- paste0(
    "@article{", citation_key, ",\n",
    "  author = {", authors, "},\n",
    "  title = {", title, "},\n",
    "  journal = {", row$SO, "},\n",
    "  year = {", row$PY, "},\n",
    if (!is.na(row$DI) && row$DI != "") paste0("  doi = {", row$DI, "},\n") else "",
    "  abstract = {", abstract, "},\n",
    "  note = {Cited by: ", row$Z9, "}\n",
    "}\n\n"
  )
  
  return(bibtex)
}

# Step 3: Create BibTeX files for each cluster
clusters <- unique(top_papers$cluster_code)

for (cluster in clusters) {
  # Get papers for this cluster
  cluster_papers <- top_papers %>%
    filter(cluster_code == cluster)
  
  # Generate BibTeX entries
  bibtex_content <- ""
  for (i in 1:nrow(cluster_papers)) {
    bibtex_content <- paste0(
      bibtex_content,
      create_bibtex_entry(cluster_papers[i, ], i)
    )
  }
  
  # Write to file
  filename <- paste0("cluster_", cluster, "_articles.bib")
  writeLines(bibtex_content, filename)
  
  cat("Created:", filename, "\n")
}
