# ==============================================================================
# Verify & Normalize Dataset for Reports
# ==============================================================================
# Sourced by: a05_reports.R
# Expects in environment: dataset (data.frame), settings (list with $rp)
#
# This script ensures the dataset has all required columns in the correct
# format before generating reports. It:
#   1. Validates critical columns (TI, AB, X_C) — stops on missing
#   2. Fills solvable missing columns with sensible defaults
#   3. Warns about optional missing columns
#   4. Clamps future publication years to the current year
#   5. Coerces key columns to numeric
# ==============================================================================

# --- Coerce to data.frame (guards against tibble/data.table edge cases) ------
dataset <- as.data.frame(dataset)
available_columns <- colnames(dataset)

# --- Critical columns (halt if missing) --------------------------------------
critical_cols <- c(TI = "TITLE", AB = "ABSTRACT", X_C = "CLUSTER")
for (col in names(critical_cols)) {
  if (!(col %in% available_columns)) {
    stop(glue("CRITICAL: Column '{col}' ({critical_cols[[col]]}) is missing from dataset."))
  }
}

# --- Solvable: fill missing columns with defaults ----------------------------
if (!("X_E" %in% available_columns) && "Z9" %in% available_columns) {
  dataset$X_E <- dataset$Z9
}
if (!("PY" %in% available_columns)) {
  dataset$PY <- settings$rp$most_recent_year
}
if (!("DT" %in% available_columns)) {
  dataset$DT <- "Article"
}
if (!("Z9" %in% available_columns)) {
  dataset$Z9 <- 1
}

# Generate semicolon-separated keywords from title text (fallback for DE/ID).
# Uses base R + stringr only — no dependency on the tm package.
get_keywords_split <- function(a_column) {
  text <- a_column %>%
    iconv(to = "UTF-8", sub = "byte") %>%
    iconv(to = "ASCII//TRANSLIT", sub = "") %>%
    tolower() %>%
    str_replace_all("[0-9]+", "") %>%              # remove numbers
    str_replace_all("[[:punct:]]+", " ") %>%       # remove punctuation
    str_squish()                                    # collapse whitespace

  # Remove common English stopwords (tm::stopwords equivalent)
  en_stopwords <- c(
    "i", "me", "my", "myself", "we", "our", "ours", "ourselves", "you",
    "your", "yours", "yourself", "yourselves", "he", "him", "his",
    "himself", "she", "her", "hers", "herself", "it", "its", "itself",
    "they", "them", "their", "theirs", "themselves", "what", "which",
    "who", "whom", "this", "that", "these", "those", "am", "is", "are",
    "was", "were", "be", "been", "being", "have", "has", "had", "having",
    "do", "does", "did", "doing", "would", "should", "could", "ought",
    "will", "shall", "can", "may", "might", "must", "need", "dare",
    "a", "an", "the", "and", "but", "if", "or", "because", "as",
    "until", "while", "of", "at", "by", "for", "with", "about",
    "against", "between", "through", "during", "before", "after",
    "above", "below", "to", "from", "up", "down", "in", "out", "on",
    "off", "over", "under", "again", "further", "then", "once", "here",
    "there", "when", "where", "why", "how", "all", "both", "each",
    "few", "more", "most", "other", "some", "such", "no", "nor", "not",
    "only", "own", "same", "so", "than", "too", "very", "just", "don",
    "now"
  )
  stopword_pattern <- paste0("\\b(", paste(en_stopwords, collapse = "|"), ")\\b")
  text <- str_replace_all(text, stopword_pattern, "")
  text <- str_replace_all(text, "\\s+", "; ")
  text <- str_replace_all(text, "^; |; $", "")
  return(text)
}

if (!("DE" %in% available_columns)) {
  dataset$DE <- get_keywords_split(dataset$TI)
}
if (!("ID" %in% available_columns)) {
  dataset$ID <- get_keywords_split(dataset$TI)
}

# --- Optional columns (warn if missing) --------------------------------------
optional_cols <- c("WC", "AU", "DI", "SO")
for (col in optional_cols) {
  if (!(col %in% available_columns)) {
    message(glue("Warning: Optional column '{col}' is missing from dataset."))
  }
}

# --- Clamp future publication years ------------------------------------------
# Near year-end, some papers have PY = next year (accepted/online-first).
# For bibliometric purposes we treat them as published this year.
this_year <- as.numeric(format(Sys.Date(), "%Y"))
dataset$PY[is.na(dataset$PY)] <- this_year
future_year_papers <- sum(dataset$PY > this_year)
if (future_year_papers > 0) {
  message(glue("{future_year_papers} paper(s) with a future publication year clamped to {this_year}."))
  dataset$PY[dataset$PY > this_year] <- this_year
}

# --- Coerce key columns to numeric -------------------------------------------
dataset$X_N <- as.numeric(as.character(dataset$X_N))
dataset$X_C <- as.numeric(as.character(dataset$X_C))
dataset$X_E <- as.numeric(dataset$X_E)
dataset$Z9  <- as.numeric(dataset$Z9)
dataset$PY  <- as.numeric(as.character(dataset$PY))

