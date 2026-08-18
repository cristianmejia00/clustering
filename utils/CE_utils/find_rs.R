# ==============================================================================
# utils/CE_utils/find_rs.R
#
# Build a per-paper Circular Economy R-strategies dataset from dataset_comp.csv
# and dataset_minimal.csv.
#
# Output columns:
#   UT, level0, subcluster_label1,
#   refuse, reduce, reuse, rethink, repair, refurbish,
#   remanufacture, repurpose, recycle, recover, remine,
#   strategies
#
# The output file is written beside dataset_comp.csv as:
#   dataset_rs_strategies.csv
# ==============================================================================

normalize_text_for_matching <- function(text_vec) {
    text_vec <- ifelse(is.na(text_vec), "", as.character(text_vec))
    text_vec <- enc2utf8(text_vec)

    # Transliterate accents/special symbols and keep matching in ASCII space.
    text_vec <- iconv(text_vec, from = "UTF-8", to = "ASCII//TRANSLIT", sub = "")
    text_vec[is.na(text_vec)] <- ""

    text_vec <- tolower(text_vec)
    text_vec <- gsub("[[:punct:]]+", " ", text_vec)
    text_vec <- gsub("\\s+", " ", text_vec)
    trimws(text_vec)
}

get_rs_patterns <- function() {
    c(
        refuse = "\\brefus(?:e|es|ed|ing|al|als)\\b",
        reduce = "\\breduc(?:e|es|ed|ing|tion|tions)\\b",
        reuse = "\\bre[ -]?us(?:e|es|ed|ing|able|ability)\\b",
        rethink = "\\bre[ -]?think(?:s|ing|er|ers)?\\b|\\brethought\\b",
        repair = "\\brepair(?:s|ed|ing|able)?\\b",
        refurbish = "\\brefurbish(?:es|ed|ing|ment|ments)?\\b",
        remanufacture = "\\bre[ -]?manufactur(?:e|es|ed|ing|er|ers|ing)\\b",
        repurpose = "\\bre[ -]?purpos(?:e|es|ed|ing)\\b",
        recycle = "\\brecycl(?:e|es|ed|ing|able|ability)\\b",
        recover = "\\brecover(?:s|ed|ing|y|ies)?\\b",
        remine = "\\bre[ -]?min(?:e|es|ed|ing)\\b"
    )
}

build_rs_flags <- function(text_vec, rs_patterns = get_rs_patterns()) {
    text_vec <- normalize_text_for_matching(text_vec)

    if (length(text_vec) == 0) {
        empty_df <- as.data.frame(matrix(integer(0), nrow = 0, ncol = length(rs_patterns)))
        colnames(empty_df) <- names(rs_patterns)
        empty_df$strategies <- character(0)
        return(empty_df)
    }

    hits <- vapply(
        rs_patterns,
        function(pattern) grepl(pattern, text_vec, perl = TRUE),
        FUN.VALUE = logical(length(text_vec))
    )

    if (is.null(dim(hits))) {
        hits <- matrix(hits, ncol = 1)
        colnames(hits) <- names(rs_patterns)[1]
    }

    hits_df <- as.data.frame(hits, stringsAsFactors = FALSE, check.names = FALSE)
    for (nm in colnames(hits_df)) {
        hits_df[[nm]] <- as.integer(hits_df[[nm]])
    }

    strategy_names <- colnames(hits_df)
    hits_matrix <- as.matrix(hits_df)
    strategies <- apply(hits_matrix, 1, function(row_vals) {
        matched <- strategy_names[as.logical(row_vals)]
        if (length(matched) == 0) {
            ""
        } else {
            paste(matched, collapse = "; ")
        }
    })

    hits_df$strategies <- strategies
    hits_df
}

resolve_rs_input_paths <- function(settings, dataset_comp_path = NULL, dataset_minimal_path = NULL) {
    analysis_root <- file.path(
        settings$metadata$bibliometrics_folder,
        settings$metadata$project_folder,
        settings$metadata$analysis_id
    )

    if (is.null(dataset_comp_path)) {
        dataset_comp_path <- file.path(analysis_root, "dataset_comp.csv")
    }

    if (is.null(dataset_minimal_path)) {
        if (!is.null(settings$params$type_of_analysis) &&
            settings$params$type_of_analysis %in% c("citation_network")) {
            dataset_minimal_path <- file.path(
                analysis_root,
                settings$cno$clustering$algorithm,
                as.character(settings$cno$thresholding$threshold),
                "dataset_minimal.csv"
            )
        } else {
            dataset_minimal_path <- file.path(analysis_root, "dataset_minimal.csv")
        }
    }

    list(
        dataset_comp_path = dataset_comp_path,
        dataset_minimal_path = dataset_minimal_path
    )
}

find_rs_strategies <- function(settings,
                               dataset_comp_path = NULL,
                               dataset_minimal_path = NULL,
                               output_path = NULL) {
    paths <- resolve_rs_input_paths(
        settings = settings,
        dataset_comp_path = dataset_comp_path,
        dataset_minimal_path = dataset_minimal_path
    )

    if (!file.exists(paths$dataset_comp_path)) {
        stop("dataset_comp.csv not found: ", paths$dataset_comp_path)
    }
    if (!file.exists(paths$dataset_minimal_path)) {
        stop("dataset_minimal.csv not found: ", paths$dataset_minimal_path)
    }

    character_cols <- readr::cols(.default = readr::col_character())
    dataset_comp <- readr::read_csv(
        paths$dataset_comp_path,
        show_col_types = FALSE,
        col_types = character_cols,
        progress = FALSE
    )
    dataset_minimal <- readr::read_csv(
        paths$dataset_minimal_path,
        show_col_types = FALSE,
        col_types = character_cols,
        progress = FALSE
    )

    required_comp_cols <- c("UT", "TI", "AB")
    missing_comp_cols <- setdiff(required_comp_cols, colnames(dataset_comp))
    if (length(missing_comp_cols) > 0) {
        stop("dataset_comp is missing required column(s): ", paste(missing_comp_cols, collapse = ", "))
    }

    if (!("UT" %in% colnames(dataset_minimal))) {
        stop("dataset_minimal is missing required column: UT")
    }

    if (anyDuplicated(dataset_comp$UT) > 0) {
        warning("dataset_comp contains duplicated UT values; output preserves one row per input paper")
    }
    if (anyDuplicated(dataset_minimal$UT) > 0) {
        warning("dataset_minimal contains duplicated UT values; first matching row per UT will be used")
    }

    if (!("level0" %in% colnames(dataset_minimal))) {
        dataset_minimal$level0 <- NA
        warning("Column level0 not found in dataset_minimal; filling with NA")
    }
    if (!("subcluster_label1" %in% colnames(dataset_minimal))) {
        dataset_minimal$subcluster_label1 <- NA
        warning("Column subcluster_label1 not found in dataset_minimal; filling with NA")
    }

    minimal_idx <- match(dataset_comp$UT, dataset_minimal$UT)
    level0_vals <- dataset_minimal$level0[minimal_idx]
    subcluster_vals <- dataset_minimal$subcluster_label1[minimal_idx]

    combined_text <- paste(
        ifelse(is.na(dataset_comp$TI), "", dataset_comp$TI),
        ifelse(is.na(dataset_comp$AB), "", dataset_comp$AB),
        sep = ". "
    )

    rs_flags <- build_rs_flags(combined_text)

    output <- data.frame(
        UT = dataset_comp$UT,
        level0 = level0_vals,
        subcluster_label1 = subcluster_vals,
        rs_flags,
        stringsAsFactors = FALSE,
        check.names = FALSE
    )

    if (is.null(output_path)) {
        output_path <- file.path(dirname(paths$dataset_comp_path), "dataset_rs_strategies.csv")
    }

    if (nrow(output) != nrow(dataset_comp)) {
        stop("Unexpected output size mismatch: output rows do not match dataset_comp rows")
    }

    strategy_cols <- names(get_rs_patterns())
    invalid_flags <- vapply(
        strategy_cols,
        function(col) any(!(output[[col]] %in% c(0L, 1L))),
        logical(1)
    )
    if (any(invalid_flags)) {
        stop("Invalid strategy flags found in columns: ", paste(names(invalid_flags)[invalid_flags], collapse = ", "))
    }

    readr::write_csv(output, output_path, na = "")

    strategy_totals <- vapply(strategy_cols, function(col) sum(output[[col]], na.rm = TRUE), numeric(1))
    papers_with_any <- sum(output$strategies != "")

    message("RS strategies dataset created: ", output_path)
    message("Papers processed: ", nrow(output), " | Papers with >=1 strategy: ", papers_with_any)
    message("Per-strategy totals: ", paste(paste(names(strategy_totals), strategy_totals, sep = "="), collapse = ", "))

    invisible(output)
}
