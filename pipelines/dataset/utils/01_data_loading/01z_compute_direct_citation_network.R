# ==============================================================================
# 01z_compute_direct_citation_network.R
#
# Build a direct-citation edge list from a Web of Science (WoS) dataset.
#
# CONTEXT
# -------
# This script is sourced by a01_network.R, which provides `dataset` (a
# data.frame loaded from dataset_raw_cleaned.csv). It must NOT be run
# standalone.
#
# HOW IT WORKS
# ------------
# WoS records contain a CR (Cited References) column, where each reference is a
# semicolon-separated string in the format:
#   "Author, Year, Journal, Volume, Page, DOI <doi>"
#
# For every article i in our dataset we:
#   1. Build a compact "record key" from its own metadata (AU, PY, J9, VL, BP).
#   2. Extract its DOI from the DI column.
#   3. Parse every reference in its CR column and compare it against all record
#      keys and DOIs of articles *within* the dataset.
#
# A match means article i cites article j, so we emit the directed edge (i, j).
# Matching is attempted first by record key. If no record key matches but the
# DOI does, the DOI match is used as a fallback. This two-pass strategy
# maximises recall — record keys cover older references that lack DOIs, while
# DOI matching catches references whose metadata formatting varies.
#
# INPUTS  (expected in the calling environment)
# ------
#   dataset  : data.frame with at least columns AU, PY, CR, and UT.
#              Optional columns: J9, VL, BP (for record-key matching) and
#              DI (for DOI matching). Works with Derwent data when J9/VL/BP
#              are absent (DOI-only matching).
#
# OUTPUTS (left in the calling environment)
# -------
#   ncol_file : character matrix (Nx2) — edge list [citing_index, cited_index].
#               Used by some network-plotting utilities (e.g. LGL export).
#   network   : data.frame version of ncol_file, with columns V1 and V2.
#               Written to network.csv by a01_network.R.
# ==============================================================================

# --- 1. Assign synthetic UT to records that lack one --------------------------
blank_ut <- which(dataset$UT == "")
if (length(blank_ut) > 0) {
  dataset$UT[blank_ut] <- paste0("wos:ccc", seq_along(blank_ut))
}

# --- 2. Build record keys for dataset articles (record-based matching) --------
# Record key format: "first_author, year, journal, Vvolume, Ppage" (lowercased).
# When J9/VL/BP columns are missing (e.g. Derwent patents), this step is
# skipped and matching relies entirely on DOIs.

has_record_cols <- all(c("J9", "VL", "BP") %in% colnames(dataset))

if (has_record_cols) {
  first_author <- gsub(",", "", gsub("; .*$", "", dataset$AU))
  article_records <- paste(first_author, dataset$PY, dataset$J9,
                           paste0("V", dataset$VL),
                           paste0("P", dataset$BP),
                           sep = ", ") %>%
    # Remove fragments from missing volume/page (e.g. ", VNA", trailing ", P")
    gsub(", VNA|, P$", ",", .) %>%
    gsub("(, ,|,,)+", ",", .) %>%
    gsub(",$|, $", "", .) %>%
    tolower()
  rm(first_author)
} else {
  message("J9, VL, or BP column missing — record-key matching disabled (DOI only).")
  article_records <- rep(NA_character_, nrow(dataset))
}

# --- 3. Extract DOIs for dataset articles (DOI-based matching) ----------------

has_doi_col <- "DI" %in% colnames(dataset)

if (has_doi_col) {
  article_dois <- tolower(dataset$DI)
} else {
  message("DI column missing — DOI matching disabled.")
  article_dois <- rep("", nrow(dataset))
}

# --- 4. Parse cited references from CR column ---------------------------------
# Each element of `references` is a character vector of the individual
# references cited by that article.

references <- strsplit(dataset$CR, "; ")
n_refs     <- lengths(references)

# Expand into a flat table: one row per (citing article, cited reference).
# `rep()` repeats article index i by n_refs[i], aligning with `unlist()`.
references_df <- data.frame(
  article_index    = rep(seq_len(nrow(dataset)), n_refs),
  reference_raw    = tolower(unlist(references)),
  stringsAsFactors = FALSE
)
rm(references)

# --- 5. Separate record key and DOI within each cited reference ---------------
# WoS references embed the DOI after ", doi ". We split on that token.

if (has_record_cols) {
  split_parts <- str_split_fixed(references_df$reference_raw, ", doi ", 2)
  references_df$ref_record <- split_parts[, 1]
  references_df$ref_doi    <- split_parts[, 2]
  rm(split_parts)
} else {
  # Without record columns, treat the whole reference as a potential DOI
  references_df$ref_record <- ""
  references_df$ref_doi    <- references_df$reference_raw
}
references_df$reference_raw <- NULL

# --- 6. Flag references that match a dataset article --------------------------
# Only references pointing to articles *within* our dataset are useful.
# With hash-based %in%, this is O(n) per lookup vector.

valid_records <- if (has_record_cols) article_records else character(0)
valid_dois    <- if (has_doi_col) article_dois[article_dois != ""] else character(0)

references_df$match_record <- references_df$ref_record %in% valid_records
references_df$match_doi    <- references_df$ref_doi    %in% valid_dois

# Keep only references that match at least one strategy
references_df <- references_df[references_df$match_record | references_df$match_doi, ]

cat(sprintf(
  "Matched %d references: %d by record key, %d by DOI (%d DOI-only)\n",
  nrow(references_df),
  sum(references_df$match_record),
  sum(references_df$match_doi),
  sum(!references_df$match_record & references_df$match_doi)
))

# --- 7. Resolve each matched reference to a dataset row index -----------------
# Prefer record-key match; fall back to DOI when record key didn't match.

cited_index <- integer(nrow(references_df))

if (any(references_df$match_record)) {
  idx_rec <- references_df$match_record
  cited_index[idx_rec] <- match(references_df$ref_record[idx_rec], article_records)
}

doi_only <- !references_df$match_record & references_df$match_doi
if (any(doi_only)) {
  cited_index[doi_only] <- match(references_df$ref_doi[doi_only], article_dois)
}

references_df$cited_index <- cited_index

# --- 8. Build edge list -------------------------------------------------------
# ncol_file: character matrix kept for backward compatibility with LGL utilities.
# network:   data.frame written to CSV by a01_network.R.

ncol_file <- matrix(
  c(as.character(references_df$article_index),
    as.character(references_df$cited_index)),
  ncol = 2
)
network <- as.data.frame(ncol_file, stringsAsFactors = FALSE)

# --- Clean up intermediate objects --------------------------------------------
rm(references_df, cited_index, doi_only, n_refs,
   valid_records, valid_dois, blank_ut)
if (exists("idx_rec")) rm(idx_rec)