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
#' @param top INTEGER. the number of results to include in the report
#' @param with_all BOOL. if the summary including all data should be include as `Cluster 0`
#' @returns Nothing. --> It writes a .csv with the report.
generate_long_report <- function(df, a_column, clusters, top, with_all = TRUE) {
  cluster_results <- lapply(clusters, function(cluster_id) {
    cluster_data <- dplyr::filter(df, X_C == cluster_id)
    cluster_tops <- TopSomething(cluster_data, coll = a_column, top = top)
    to_long_summary(cluster_tops, a_cluster = cluster_id, top = top)
  })

  result_list <- dplyr::bind_rows(cluster_results)

  if (with_all && a_column != "AU") {
    cluster_zero <- TopSomething(df, coll = a_column, top = top) %>%
      to_long_summary(a_cluster = 0, top = top)
    result_list <- dplyr::bind_rows(cluster_zero, result_list)
  }

  if (nrow(result_list) == 0) {
    return(invisible(NULL))
  }

  result_list <- result_list %>%
    dplyr::rename(!!a_column := term)

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
  split_values <- strsplit(as.character(dplyr::coalesce(df[[a_column]], "")), ";\\s*")

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
    generate_long_report(df = myDataCorrect, a_column = cc, clusters = list_of_clusters, top = settings$rp$top_items)
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
