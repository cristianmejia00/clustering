# 20190828
# Check requirements for the file myDataCorrect from where will we compute the reports

# Any file, passing through this code will have the following columns in the following format

# # INPUTS
# myDataCorrect
# settings$rp$top_documents
# settings$rp$column_labels
# rn$PROJECTarticlereport


print("###################### reports/01_document_report_with_abstract.R")

# Helper function to get urls from DOI
convert_doi_to_url <- function(a_list_of_DOI) {
  a_list_of_DOI[is.na(a_list_of_DOI)] <- ""
  sapply(a_list_of_DOI, function(x) {
    if (nchar(x) > 0) {
      paste("https://doi.org/", x, sep = "")
    } else {
      x
    }
  })
}

# Find which colnames exist.
# This define which columns and in which order they will appear in the article report
potential_columns <- c(
  "X_C", "cluster_code",
  "topic", "related_topics", "TD",
  "AU", "PY", "DI", "TI", "AB", "Z9", "X_E", "DE", "SO", "WC", "Countries", "sentiment", "sentiment_factor", "UT", "uuid",
  "global_degree", "global_in_degree", "global_page_rank",
  'ethnicities', 'race', 'is_japanese', 'nationality', 'gender',
  'nationality_processed', 'ethnicities_total_elements',
  'ethnicities_unique_elements',
  'ethnicities_variety_ratio',
  'ethnicities_hhi', 'ethnicities_hhi_normalized',
  'ethnicities_shannon_entropy', 'ethnicities_shannon_max',
  'ethnicities_shannon_evenness', 'ethnicities_simpson_diversity',
  'ethnicities_simpson_dominance', 'ethnicities_berger_parker_dominance',
  'ethnicities_effective_species_shannon',
  'ethnicities_effective_species_simpson', 'ethnicities_gini_coefficient',
  'ethnicities_coefficient_of_variation', 'race_total_elements',
  'race_unique_elements', 'race_variety_ratio', 'race_hhi',
  'race_hhi_normalized', 'race_shannon_entropy', 'race_shannon_max',
  'race_shannon_evenness', 'race_simpson_diversity',
  'race_simpson_dominance', 'race_berger_parker_dominance',
  'race_effective_species_shannon', 'race_effective_species_simpson',
  'race_gini_coefficient', 'race_coefficient_of_variation',
  'nationality_total_elements', 'nationality_unique_elements',
  'nationality_variety_ratio', 'nationality_hhi',
  'nationality_hhi_normalized', 'nationality_shannon_entropy',
  'nationality_shannon_max', 'nationality_shannon_evenness',
  'nationality_simpson_diversity', 'nationality_simpson_dominance',
  'nationality_berger_parker_dominance',
  'nationality_effective_species_shannon',
  'nationality_effective_species_simpson', 'nationality_gini_coefficient',
  'nationality_coefficient_of_variation', 'is_japanese_total_elements',
  'is_japanese_unique_elements', 'is_japanese_variety_ratio',
  'is_japanese_hhi', 'is_japanese_hhi_normalized',
  'is_japanese_shannon_entropy', 'is_japanese_shannon_max',
  'is_japanese_shannon_evenness', 'is_japanese_simpson_diversity',
  'is_japanese_simpson_dominance', 'is_japanese_berger_parker_dominance',
  'is_japanese_effective_species_shannon',
  'is_japanese_effective_species_simpson', 'is_japanese_gini_coefficient',
  'is_japanese_coefficient_of_variation', 'gender_total_elements',
  'gender_unique_elements', 'gender_variety_ratio', 'gender_hhi',
  'gender_hhi_normalized', 'gender_shannon_entropy', 'gender_shannon_max',
  'gender_shannon_evenness', 'gender_simpson_diversity',
  'gender_simpson_dominance', 'gender_berger_parker_dominance',
  'gender_effective_species_shannon', 'gender_effective_species_simpson',
  'gender_gini_coefficient', 'gender_coefficient_of_variation'
)

if (level_report == 0) {
  potential_columns <- c(potential_columns, "level0_page_rank",  "level0_degree", "level0_in_degree" )
}

if (level_report == 1) {
  potential_columns <- c(potential_columns, "level1_page_rank",  "level1_degree", "level1_in_degree" )
}

# Retain what's available
columns_in_myDataCorrect <- intersect(
  potential_columns,
  colnames(myDataCorrect)
)

# Create the file
article_report <- myDataCorrect %>% select(all_of(columns_in_myDataCorrect))

## Format columns
# Format DOI
if ("DI" %in% colnames(article_report)) {
  article_report$DI <- convert_doi_to_url(article_report$DI)
}

# Replace NA by empty character
article_report[is.na(article_report)] <- ""

# Filter to the top_documents of each cluster
if (settings$rp$top_documents != 0) {
  article_report <- article_report %>%
    group_by(X_C) %>%
    top_n(settings$rp$top_documents, X_E)
}

# Clean the columns
article_report$Z9 <- as.integer(article_report$Z9)
article_report$Z9[is.na(article_report$Z9)] <- 0

article_report$PY <- as.integer(article_report$PY)
article_report$PY[is.na(article_report$PY)] <- median(article_report$PY, na.rm = TRUE)

# Order the report 
article_report <- article_report[order(article_report$X_C, 
                                       -article_report$X_E, 
                                       -article_report$Z9, 
                                       -article_report$PY),]

# Change colnames to natural names
setnames(article_report, 
         names(settings$rp$column_labels), 
         unname(settings$rp$column_labels) %>% unlist(), 
         skip_absent = TRUE)


# Write the article report
write.csv(article_report, 
          file = rn$PROJECTarticlereport, 
          row.names = FALSE)

# Cleaning up
rm('columns_in_myDataCorrect')

# Filter to the top_documents of each cluster
if (settings$rp$top_documents == 0) {
  article_report_20 <- article_report %>%
    group_by(`Cluster Code`) %>%
    top_n(20, Degree)
}

# Write the article report
write.csv(article_report_20,
          file = gsub('_report', '_report_20', rn$PROJECTarticlereport),
          row.names = FALSE)
