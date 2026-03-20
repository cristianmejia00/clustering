# ==============================================================================
# a00_data_loader.R
#
# Reads raw Web of Science (WoS) tab-delimited export files, merges them into
# a single dataset, and produces a cleaned CSV ready for downstream analysis.
#
# Prerequisites:
#   1. Create a settings directive JSON via `_0_settings_dataset.R`.
#   2. Set the project folder and directive filename in `_1_entry_dataset.R`.
#
# Inputs:
#   - Raw WoS .txt files in the directory specified by settings$metadata
#   - Settings directive JSON (loaded via _1_entry_dataset.R)
#
# Outputs (written to <output_folder>/<project>/<filter_label>/):
#   - dataset_raw_cleaned.csv   : The cleaned, filtered dataset
#   - filtering_settings.json   : Copy of the filtering settings used
# ==============================================================================

source("_1_entry_dataset.R")
source("zz_utils/zz_auxiliary_functions.R")

# --- 1. Read and merge raw WoS files -----------------------------------------

raw_input_path <- file.path(
  settings$metadata$raw_input_directory,
  settings$metadata$raw_input_folder_name
)
paths_to_files <- list.files(
  path = raw_input_path,
  full.names = TRUE,
  pattern = "\\.txt$",
  recursive = TRUE
)

if (length(paths_to_files) == 0) {
  stop("No .txt files found in: ", raw_input_path)
}

list_of_all_files <- lapply(paths_to_files, function(path) {
  fread(path, sep = "\t", stringsAsFactors = FALSE,
        check.names = FALSE, encoding = "UTF-8")
})

cat(sprintf("Read %d files (%s rows each)\n",
            length(list_of_all_files),
            paste(sapply(list_of_all_files, nrow), collapse = ", ")))

dataset <- bind_rows(list_of_all_files) %>% as.data.frame()

# fread occasionally misreads the header, producing "V1" as the first column.
# When that happens, shift column names to restore the expected WoS layout.
if (colnames(dataset)[1] == "V1") {
  colnames(dataset) <- c("PT", colnames(dataset)[3:ncol(dataset)], "END")
  dataset[["END"]] <- NULL
}

# --- 2. Row filtering ---------------------------------------------------------

filter_label <- names(settings$filtering)

# Remove duplicate records by UT (unique WoS identifier)
if (settings$filtering[[filter_label]]$rows_filter$removed_duplicated_UT) {
  dataset <- dataset %>% filter(!duplicated(UT))
}

# Normalize empty strings to NA in date-related columns
for (col in intersect(c("PY", "EA", "CY"), colnames(dataset))) {
  dataset[[col]][dataset[[col]] == ""] <- NA
}

# Fill missing publication year (PY) with the most recent year from settings
fallback_year <- as.numeric(settings$filtering[[filter_label]]$rows_filter$most_recent_year)
dataset$PY[is.na(dataset$PY)] <- fallback_year

# Keep only records where PY is a valid 4-digit year
dataset <- dataset %>% filter(nchar(as.character(PY)) == 4)

# Convert all columns to UTF-8 character (required for consistent downstream processing)
dataset <- dataset %>% mutate(across(everything(), ~ enc2utf8(as.character(.x))))

# --- 3. Append derived columns ------------------------------------------------

# Row index and UUID
dataset <- dataset %>%
  mutate(
    X_N = row_number(),
    uuid = UUIDgenerate(n = n())
  )

# Countries and ISO codes (derived from the C1 affiliations column)
if (!"Countries" %in% colnames(dataset) && "C1" %in% colnames(dataset)) {
  dataset$Countries    <- getCountries(dataset$C1)
  dataset$IsoCountries <- getIsoCountries(dataset$Countries) %>%
    as.character() %>%
    gsub("NA; |; NA$|; NA", "", .)
  message("Countries column added")
} else {
  message("Countries column already present or C1 not available")
}

# Institutions (derived from the C1 affiliations column)
if (!"Institutions" %in% colnames(dataset) && "C1" %in% colnames(dataset)) {
  dataset$Institutions <- as.character(getInstitutions(dataset$C1))
  message("Institutions column added")
} else {
  message("Institutions column already present or C1 not available")
}

# --- 4. Clean abstracts -------------------------------------------------------

if ("AB" %in% colnames(dataset)) {
  dataset$AB <- dataset$AB %>%
    enc2utf8() %>%
    iconv(from = "UTF-8", to = "ASCII", sub = " ") %>%
    enc2utf8() %>%
    remove_copyright_statements() %>%
    remove_word_counts_line()
}

# --- 5. Column filtering ------------------------------------------------------

# Keep only the columns specified in settings, plus the derived columns
selected_cols <- c(
  "X_N", "uuid",
  intersect(
    colnames(dataset),
    c(unlist(settings$filtering[[filter_label]]$columns_filter$columns_selected),
      "Countries", "IsoCountries", "Institutions")
  )
)
dataset <- dataset %>% select(all_of(selected_cols))

# --- 6. Save outputs ----------------------------------------------------------

results_folder_path <- file.path(
  output_folder_path,
  project_folder_name,
  filter_label
)
dir.create(results_folder_path, showWarnings = FALSE, recursive = TRUE)

write.csv(dataset,
  file = file.path(results_folder_path, "dataset_raw_cleaned.csv"),
  row.names = FALSE
)

writeLines(
  RJSONIO::toJSON(settings$filtering, pretty = TRUE, auto_unbox = TRUE),
  file.path(results_folder_path, "filtering_settings.json")
)

cat(sprintf(
  "Done. %d records x %d columns saved to:\n  %s\n",
  nrow(dataset), ncol(dataset),
  file.path(results_folder_path, "dataset_raw_cleaned.csv")
))

rm(list = ls())
