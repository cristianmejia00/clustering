# 20181206 Cluster summaries
print("###################### reports/04_cluster_reports.R")

# Load utils
source("utils/auxiliary_functions.R")

# INPUT
list_of_clusters <- myDataCorrect %>%
  dplyr::distinct(X_C) %>%
  dplyr::arrange(X_C) %>%
  dplyr::pull(X_C)

available_columns <- colnames(myDataCorrect)

## Check the columns that are actually available
categorical_long_reports <- intersect(unlist(settings$rp$categorical_long_reports), available_columns)
categorical_simple_wide_reports <- intersect(unlist(settings$rp$categorical_simple_wide_reports), available_columns)
categorical_multi_wide_reports <- intersect(unlist(settings$rp$categorical_multi_wide_reports), available_columns)
numerical_reports <- intersect(unlist(settings$rp$numerical_reports), available_columns)


## DEFINITIONS

## A long table means that there are 3 columns:
## X_C
## The comparative column which is a factor (e.g. PY,Journals,Countries,etc.)
## The value

## A wide table means it is a matrix:
## X_C is the rows
## The values of the comparative column which is a factor (e.g. PY,Journals,Countries,etc.) represents
## as many columns needed
## The intersection is the value (e.g. of cluster 1 (row) vs Country US(column))

## A numerical report
## Takes a numeric column and computes
## min, 1q, mean, media, 3q, max, NAs

##################################################################
is_effectively_empty <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return(TRUE)
  }

  if (is.character(x)) {
    return(all(is.na(x) | trimws(x) == ""))
  }

  all(is.na(x))
}

#' Expand a multi-value column (split by ";") and compute per-term ave_PY / ave_Z9
#' for the papers in a given cluster data frame.
#' Returns a tibble: term (UPPERCASE), ave_PY, ave_Z9.
compute_term_stats <- function(cluster_data, a_column) {
  if (nrow(cluster_data) == 0 || !a_column %in% colnames(cluster_data)) {
    return(dplyr::tibble(term = character(), ave_PY = numeric(), ave_Z9 = numeric()))
  }

  raw_vals <- dplyr::coalesce(as.character(cluster_data[[a_column]]), "")
  py_vals  <- suppressWarnings(as.numeric(cluster_data$PY))
  z9_vals  <- suppressWarnings(as.numeric(cluster_data$Z9))

  split_terms <- strsplit(tolower(raw_vals), ";\\s*")
  row_idx     <- rep(seq_along(split_terms), lengths(split_terms))
  terms_flat  <- trimws(unlist(split_terms))

  valid       <- terms_flat != "" & !is.na(terms_flat)
  terms_flat  <- terms_flat[valid]
  row_idx     <- row_idx[valid]

  if (length(terms_flat) == 0) {
    return(dplyr::tibble(term = character(), ave_PY = numeric(), ave_Z9 = numeric()))
  }

  # Mirror TopSomething title-case for certain columns
  if (a_column %in% c("Countries", "Institutions", "AU", "SO")) {
    terms_flat <- stringr::str_to_title(terms_flat)
  }
  terms_flat <- toupper(terms_flat)   # match to_long_summary uppercase

  expanded <- dplyr::tibble(
    term = terms_flat,
    PY   = py_vals[row_idx],
    Z9   = z9_vals[row_idx]
  )

  expanded %>%
    dplyr::group_by(term) %>%
    dplyr::summarise(
      ave_PY = round(mean(PY, na.rm = TRUE), 1),
      ave_Z9 = round(mean(Z9, na.rm = TRUE), 2),
      .groups = "drop"
    )
}

to_long_summary <- function(a_table, a_cluster, top) {
  if (is.null(a_table) || length(a_table) == 0) {
    return(NULL)
  }

  if (is.data.frame(a_table)) {
    if (ncol(a_table) < 2) {
      return(NULL)
    }
    term <- a_table[[1]]
    freq <- a_table[[2]]
  } else {
    term <- names(a_table)
    freq <- unname(a_table)
  }

  out <- dplyr::tibble(
    term = as.character(term),
    Freq = suppressWarnings(as.numeric(freq))
  ) %>%
    dplyr::filter(!is.na(term), trimws(term) != "") %>%
    dplyr::slice_head(n = top) %>%
    dplyr::mutate(
      term = toupper(term),
      Cluster = a_cluster
    )

  if (nrow(out) == 0) {
    return(NULL)
  }

  out
}

#' @description
#' Creates a long report based on selected column.
#' @param df DATAFRAME. Usually `myDataCorrect`.
#' @param a_column STRING. the name of the column to summarize
#' @param clusters LIST[INTEGERS]. a list of clusters to include in the summary
#' @param top INTEGER. the number of results to include in the report (max 100)
#' @param with_all BOOL. if the summary including all data should be include as `Cluster 0`
#' @returns Nothing. --> It writes a .csv with the report.
generate_long_report <- function(df, a_column, clusters, top, with_all = TRUE) {
  # Build cluster_code lookup (X_C -> cluster_code) from df if available
  if ("cluster_code" %in% colnames(df)) {
    code_lookup <- df %>%
      dplyr::distinct(X_C, cluster_code) %>%
      dplyr::mutate(X_C_char = as.character(X_C))
  } else {
    code_lookup <- dplyr::tibble(X_C_char = character(), cluster_code = character())
  }

  cluster_results <- lapply(clusters, function(cluster_id) {
    cluster_data <- dplyr::filter(df, X_C == cluster_id)
    cluster_tops <- TopSomething(cluster_data, coll = a_column, top = top)
    res <- to_long_summary(cluster_tops, a_cluster = cluster_id, top = top)
    if (is.null(res)) return(NULL)

    avgs <- compute_term_stats(cluster_data, a_column)
    dplyr::left_join(res, avgs, by = "term")
  })

  result_list <- dplyr::bind_rows(cluster_results)

  if (with_all && a_column != "AU") {
    cluster_zero <- TopSomething(df, coll = a_column, top = top) %>%
      to_long_summary(a_cluster = 0, top = top)
    if (!is.null(cluster_zero)) {
      avgs_all <- compute_term_stats(df, a_column)
      cluster_zero <- dplyr::left_join(cluster_zero, avgs_all, by = "term")
    }
    result_list <- dplyr::bind_rows(cluster_zero, result_list)
  }

  if (nrow(result_list) == 0) {
    return(invisible(NULL))
  }

  # Attach cluster_code; synthetic "all" cluster (0) gets code "ALL"
  result_list <- result_list %>%
    dplyr::mutate(
      cluster_code = {
        looked_up <- code_lookup$cluster_code[
          match(as.character(Cluster), code_lookup$X_C_char)
        ]
        dplyr::coalesce(looked_up, ifelse(Cluster == 0, "ALL", as.character(Cluster)))
      }
    )

  result_list <- result_list %>%
    dplyr::rename(!!a_column := term) %>%
    dplyr::select(Cluster, cluster_code, dplyr::all_of(a_column), Freq, ave_PY, ave_Z9)

  write.csv(
    result_list,
    file = file.path(output_folder_level, paste0("report_", a_column, ".csv")),
    row.names = FALSE
  )
}


generate_categorical_simple_wide_reports <- function(df, a_column) {
  # Creates a `clusters x a_column` reports for frequencies and proportions based on selected column.
  # This column has a single value per record e.g. PY
  # df = a data frame. Usually `myDataCorrect` or any other with X_C column
  # a_column = the column to summarize
  # clusters = a list of clusters to include in the summary
  frequency_long <- df %>%
    dplyr::transmute(X_C, value = .data[[a_column]]) %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::count(X_C, value, name = "n")

  if (nrow(frequency_long) == 0) {
    return(invisible(NULL))
  }

  frequency_long <- frequency_long %>%
    dplyr::mutate(
      X_C = factor(X_C, levels = sort(unique(df$X_C))),
      value = factor(value)
    )

  cluster_frequencies <- xtabs(n ~ X_C + value, data = frequency_long, drop.unused.levels = FALSE)
  cluster_proportions <- prop.table(cluster_frequencies, margin = 2)

  cluster_frequencies <- as.data.frame.matrix(cluster_frequencies) %>%
    dplyr::mutate(cluster = rownames(.), .before = 1)
  rownames(cluster_frequencies) <- NULL

  cluster_proportions <- as.data.frame.matrix(cluster_proportions) %>%
    dplyr::mutate(cluster = rownames(.), .before = 1)
  rownames(cluster_proportions) <- NULL

  write.csv(cluster_frequencies,
    file = file.path(output_folder_level, paste0("report_", a_column, "_frequencies.csv")),
    row.names = FALSE
  )
  write.csv(cluster_proportions,
    file = file.path(output_folder_level, paste0("report_", a_column, "_proportions.csv")),
    row.names = FALSE
  )
}

generate_categorical_multi_wide_reports <- function(df, a_column, clusters) {
  # Creates a `clusters x a_column` reports for frequencies and proportions based on selected column.
  # This column has a multiple values per record e.g. WC because a paper can be "Engineering; Finances"
  # df = a data frame. Usually `myDataCorrect` or any other with X_C column
  # a_column = the column to summarize
  # clusters = a list of clusters to include in the summary
  split_values <- strsplit(dplyr::coalesce(as.character(df[[a_column]]), ""), ";\\s*")

  expanded <- dplyr::tibble(
    X_C = rep(df$X_C, lengths(split_values)),
    value = trimws(unlist(split_values))
  ) %>%
    dplyr::filter(value != "")

  if (nrow(expanded) == 0) {
    return(invisible(NULL))
  }

  top_values <- expanded %>%
    dplyr::count(value, sort = TRUE, name = "total") %>%
    dplyr::slice_head(n = 100)

  expanded_top <- expanded %>%
    dplyr::filter(value %in% top_values$value) %>%
    dplyr::mutate(
      X_C = factor(X_C, levels = clusters),
      value = factor(value, levels = top_values$value)
    )

  cluster_frequencies <- xtabs(~ X_C + value, data = expanded_top, drop.unused.levels = FALSE)
  totals <- colSums(cluster_frequencies)

  cluster_proportions <- sweep(cluster_frequencies, 2, totals, "/")
  if (any(totals == 0)) {
    cluster_proportions[, totals == 0] <- 0
  }

  cluster_frequencies <- as.data.frame.matrix(cluster_frequencies) %>%
    dplyr::mutate(cluster = rownames(.), .before = 1)
  rownames(cluster_frequencies) <- NULL

  cluster_proportions <- as.data.frame.matrix(cluster_proportions) %>%
    dplyr::mutate(cluster = rownames(.), .before = 1)
  rownames(cluster_proportions) <- NULL

  write.csv(cluster_frequencies,
    file = file.path(output_folder_level, paste0("report_", a_column, "_frequencies.csv")),
    row.names = FALSE
  )

  write.csv(cluster_proportions,
    file = file.path(output_folder_level, paste0("report_", a_column, "_proportions.csv")),
    row.names = FALSE
  )
}

generate_numerical_report <- function(df, a_column, clusters, with_all = TRUE) {
  # Creates a  numeric summary (min, mean, meadian, max, sd) report based on selected column.
  # df = a data frame. Usually `myDataCorrect` or any other with X_C column
  # a_column = the column to summarize
  # clusters = a list of clusters to include in the summary
  # with_all = if the summary including all data should be include as `Cluster 0`

  summarize_numeric <- function(values, cluster_id) {
    numeric_values <- suppressWarnings(as.numeric(values))
    non_missing <- numeric_values[!is.na(numeric_values)]

    if (length(non_missing) == 0) {
      return(dplyr::tibble(
        Min = NA_real_,
        `1st Qu.` = NA_real_,
        Median = NA_real_,
        Mean = NA_real_,
        `3rd Qu.` = NA_real_,
        Max = NA_real_,
        `NA's` = length(numeric_values),
        sd = NA_real_,
        cluster = cluster_id
      ))
    }

    dplyr::tibble(
      Min = min(non_missing),
      `1st Qu.` = unname(stats::quantile(non_missing, probs = 0.25, type = 7)),
      Median = stats::median(non_missing),
      Mean = mean(non_missing),
      `3rd Qu.` = unname(stats::quantile(non_missing, probs = 0.75, type = 7)),
      Max = max(non_missing),
      `NA's` = sum(is.na(numeric_values)),
      sd = round(stats::sd(non_missing), 3),
      cluster = cluster_id
    )
  }

  result_list <- lapply(clusters, function(cluster_id) {
    cluster_values <- df %>%
      dplyr::filter(X_C == cluster_id) %>%
      dplyr::pull(.data[[a_column]])

    summarize_numeric(cluster_values, cluster_id)
  }) %>%
    dplyr::bind_rows()

  if (with_all) {
    cluster_zero <- summarize_numeric(df[[a_column]], 0)
    result_list <- dplyr::bind_rows(cluster_zero, result_list)
  }

  write.csv(result_list,
    file = file.path(output_folder_level, paste0("report_", a_column, ".csv")),
    row.names = FALSE
  )
}

##################################################################

## Write reports
for (cc in categorical_long_reports) {
  if (cc != "is_japanese" && !is_effectively_empty(myDataCorrect[[cc]])) {
    generate_long_report(df = myDataCorrect, a_column = cc, clusters = list_of_clusters, top = 100)
  } else {
    print(glue("{cc} is totally empty. Report not created"))
  }
}

for (cc in categorical_simple_wide_reports) {
  if (!is_effectively_empty(myDataCorrect[[cc]])) {
    generate_categorical_simple_wide_reports(df = myDataCorrect, a_column = cc)
  } else {
    print(glue("{cc} is totally empty. Report not created"))
  }
}

for (cc in categorical_multi_wide_reports) {
  if (!is_effectively_empty(myDataCorrect[[cc]])) {
    generate_categorical_multi_wide_reports(df = myDataCorrect, a_column = cc, clusters = list_of_clusters)
  } else {
    print(glue("{cc} is totally empty. Report not created"))
  }
}

for (cc in numerical_reports) {
  if (!is_effectively_empty(myDataCorrect[[cc]])) {
    generate_numerical_report(df = myDataCorrect, a_column = cc, clusters = list_of_clusters)
  } else {
    print(glue("{cc} is totally empty. Report not created"))
  }
}
