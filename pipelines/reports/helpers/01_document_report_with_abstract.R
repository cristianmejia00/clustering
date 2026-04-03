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
  doi <- dplyr::coalesce(a_list_of_DOI, "")
  dplyr::if_else(
    nchar(doi) > 0,
    paste0("https://doi.org/", doi),
    doi
  )
}

# Find which colnames exist.
# This define which columns and in which order they will appear in the article report
potential_columns <- c(
  "X_C", "cluster_code",
  "topic", "related_topics", "TD",
  "AU", "PY", "DI", "TI", "AB", "Z9", "X_E", "DE", "SO", "WC", "Countries", "sentiment", "sentiment_factor", "UT", "uuid",
  "global_degree", "global_in_degree", "global_page_rank",
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

# Precompute values used in mutate
py_median <- suppressWarnings(as.integer(myDataCorrect$PY)) %>%
  median(na.rm = TRUE)

if (is.na(py_median)) {
  py_median <- 0L
}

# Build report with consistent dplyr flow
article_report <- myDataCorrect %>%
  dplyr::select(dplyr::all_of(columns_in_myDataCorrect)) %>%
  dplyr::mutate(
    dplyr::across(where(is.character), ~ dplyr::coalesce(.x, "")),
    DI = if ("DI" %in% names(.)) convert_doi_to_url(DI) else DI,
    Z9 = if ("Z9" %in% names(.)) dplyr::coalesce(suppressWarnings(as.integer(Z9)), 0L) else Z9,
    PY = if ("PY" %in% names(.)) dplyr::coalesce(suppressWarnings(as.integer(PY)), py_median) else PY
  )

# Filter to the top_documents of each cluster when requested
if (settings$rp$top_documents != 0 && all(c("X_C", "X_E") %in% names(article_report))) {
  article_report <- article_report %>%
    dplyr::group_by(X_C) %>%
    dplyr::slice_max(order_by = X_E, n = settings$rp$top_documents, with_ties = TRUE) %>%
    dplyr::ungroup()
}

# Order report rows
sort_columns <- intersect(c("X_C", "X_E", "Z9", "PY"), names(article_report))
if (length(sort_columns) > 0) {
  desc_columns <- intersect(c("X_E", "Z9", "PY"), sort_columns)
  article_report <- article_report %>%
    dplyr::arrange(
      dplyr::across(dplyr::all_of(setdiff(sort_columns, desc_columns))),
      dplyr::across(dplyr::all_of(desc_columns), dplyr::desc)
    )
}

# Change colnames to natural names
rename_from <- names(settings$rp$column_labels)
rename_to <- unname(unlist(settings$rp$column_labels))
rename_idx <- match(names(article_report), rename_from)
names(article_report)[!is.na(rename_idx)] <- rename_to[rename_idx[!is.na(rename_idx)]]


# Write the article report
write.csv(article_report, 
          file = rn$PROJECTarticlereport, 
          row.names = FALSE)

# Filter to the top_documents of each cluster
if (settings$rp$top_documents == 0) {
  article_report_20 <- article_report %>%
    dplyr::group_by(`Cluster Code`) %>%
    dplyr::slice_max(order_by = Degree, n = 20, with_ties = TRUE) %>%
    dplyr::ungroup()

  # Write the article report
  write.csv(article_report_20,
            file = gsub('_report', '_report_20', rn$PROJECTarticlereport),
            row.names = FALSE)
}
