# ==============================================================================
# utils/CE_utils/charts_rs.R
#
# Chart creator for Circular Economy R-strategy findings.
# Builds bar charts and bubble matrices from dataset_rs_strategies.csv.
# ==============================================================================

get_rs_all_strategies <- function() {
    c(
        "refuse", "reduce", "reuse", "rethink", "repair", "refurbish",
        "remanufacture", "repurpose", "recycle", "recover", "remine"
    )
}

get_rs_default_9_strategies <- function() {
    c(
        "recycle", "reduce", "recover", "reuse", "remanufacture",
        "repair", "refurbish", "repurpose", "rethink"
    )
}

normalize_strategy_selection <- function(strategies_to_show, available_cols) {
    if (is.null(strategies_to_show) || length(strategies_to_show) == 0) {
        strategies_to_show <- get_rs_default_9_strategies()
    }

    normalized <- tolower(trimws(as.character(strategies_to_show)))
    normalized <- normalized[normalized != ""]
    normalized <- unique(normalized)

    missing_cols <- setdiff(normalized, available_cols)
    if (length(missing_cols) > 0) {
        stop("Selected strategies not found in RS dataset: ", paste(missing_cols, collapse = ", "))
    }

    normalized
}

resolve_rs_dataset_path <- function(settings, rs_file_path = NULL) {
    if (!is.null(rs_file_path) && nzchar(trimws(rs_file_path))) {
        return(rs_file_path)
    }

    analysis_root <- file.path(
        settings$metadata$bibliometrics_folder,
        settings$metadata$project_folder,
        settings$metadata$analysis_id
    )

    file.path(analysis_root, "dataset_rs_strategies.csv")
}

clean_cluster_values <- function(x) {
    x <- as.character(x)
    x <- gsub("---", "", x)
    x <- gsub("-0$", "", x)
    x <- trimws(x)
    x[x == ""] <- NA_character_
    x
}

order_cluster_levels <- function(cluster_values) {
    vals <- unique(cluster_values[!is.na(cluster_values) & cluster_values != ""])
    if (length(vals) == 0) {
        return(character(0))
    }

    split_key <- function(v) {
        parts <- strsplit(v, "-", fixed = TRUE)[[1]]
        parts <- parts[parts != ""]
        nums <- suppressWarnings(as.integer(parts))
        if (length(parts) == 0) {
            return("99999")
        }
        if (all(!is.na(nums))) {
            paste(sprintf("%05d", nums), collapse = "-")
        } else {
            paste0("zz-", v)
        }
    }

    ord <- order(vapply(vals, split_key, character(1)), vals)
    vals[ord]
}

coerce_binary <- function(x) {
    num <- suppressWarnings(as.integer(as.character(x)))
    num[is.na(num)] <- 0L
    ifelse(num > 0L, 1L, 0L)
}

normalize_rows <- function(m) {
    if (length(m) == 0) {
        return(m)
    }
    row_totals <- rowSums(m)
    out <- sweep(m, 1, ifelse(row_totals == 0, 1, row_totals), "/")
    out[row_totals == 0, ] <- 0
    out
}

normalize_cols <- function(m) {
    if (length(m) == 0) {
        return(m)
    }
    col_totals <- colSums(m)
    out <- sweep(m, 2, ifelse(col_totals == 0, 1, col_totals), "/")
    out[, col_totals == 0] <- 0
    out
}

matrix_to_long <- function(m, row_name = "row", col_name = "col", value_name = "value") {
    if (length(m) == 0) {
        return(data.frame())
    }
    d <- as.data.frame(as.table(m), stringsAsFactors = FALSE)
    colnames(d) <- c(row_name, col_name, value_name)
    d
}

split_countries_full_count <- function(df, cluster_col = "cluster") {
    if (!("Countries" %in% colnames(df))) {
        return(data.frame(UT = character(0), cluster = character(0), country = character(0), stringsAsFactors = FALSE))
    }

    rows <- vector("list", nrow(df))
    for (i in seq_len(nrow(df))) {
        countries_raw <- as.character(df$Countries[i])
        if (is.na(countries_raw) || !nzchar(trimws(countries_raw))) {
            next
        }
        countries <- trimws(unlist(strsplit(countries_raw, ";", fixed = TRUE)))
        countries <- countries[countries != ""]
        countries <- unique(countries)
        if (length(countries) == 0) next

        rows[[i]] <- data.frame(
            UT = as.character(df$UT[i]),
            cluster = as.character(df[[cluster_col]][i]),
            country = countries,
            stringsAsFactors = FALSE
        )
    }

    out <- dplyr::bind_rows(rows)
    if (nrow(out) == 0) {
        return(out)
    }
    out <- out[!is.na(out$cluster) & out$cluster != "", , drop = FALSE]
    unique(out)
}

build_strategy_mentions <- function(df, strategy_cols, cluster_col = "cluster") {
    if (length(strategy_cols) == 0 || nrow(df) == 0) {
        return(data.frame(UT = character(0), cluster = character(0), strategy = character(0), stringsAsFactors = FALSE))
    }

    mat <- as.matrix(data.frame(lapply(df[, strategy_cols, drop = FALSE], coerce_binary), check.names = FALSE))
    idx <- which(mat == 1L, arr.ind = TRUE)

    if (nrow(idx) == 0) {
        return(data.frame(UT = character(0), cluster = character(0), strategy = character(0), stringsAsFactors = FALSE))
    }

    out <- data.frame(
        UT = as.character(df$UT[idx[, 1]]),
        cluster = as.character(df[[cluster_col]][idx[, 1]]),
        strategy = strategy_cols[idx[, 2]],
        stringsAsFactors = FALSE
    )

    out[!is.na(out$cluster) & out$cluster != "", , drop = FALSE]
}

ensure_matrix_columns <- function(m, col_levels) {
    if (is.null(colnames(m))) {
        colnames(m) <- character(ncol(m))
    }
    missing_cols <- setdiff(col_levels, colnames(m))
    if (length(missing_cols) > 0) {
        add <- matrix(0,
            nrow = nrow(m), ncol = length(missing_cols),
            dimnames = list(rownames(m), missing_cols)
        )
        m <- cbind(m, add)
    }
    m[, col_levels, drop = FALSE]
}

save_plot_formats <- function(plot_obj, file_base, formats = c("svg"), width = 7, height = 5) {
    formats <- unique(tolower(formats))
    for (fmt in formats) {
        ggplot2::ggsave(
            filename = paste0(file_base, ".", fmt),
            plot = plot_obj,
            width = width,
            height = height,
            units = "in"
        )
    }
}

plot_horizontal_bar <- function(df, category_col, value_col, title, x_label, y_label = "Documents") {
    if (nrow(df) == 0) {
        return(ggplot2::ggplot() +
            ggplot2::theme_void() +
            ggplot2::ggtitle(paste0(title, " (no data)")))
    }

    df <- df[order(df[[value_col]], decreasing = TRUE), , drop = FALSE]
    df[[category_col]] <- factor(df[[category_col]], levels = rev(df[[category_col]]))

    ggplot2::ggplot(df, ggplot2::aes(x = !!rlang::sym(category_col), y = !!rlang::sym(value_col))) +
        ggplot2::geom_col(fill = "#1496c6") +
        ggplot2::coord_flip() +
        ggplot2::labs(title = title, x = x_label, y = y_label) +
        ggplot2::theme_bw(base_size = 12) +
        ggplot2::theme(
            panel.grid.minor = ggplot2::element_blank(),
            panel.grid.major.y = ggplot2::element_blank()
        )
}

plot_bubble_matrix <- function(df, x_col, y_col, value_col, title, x_label = NULL, y_label = NULL) {
    if (nrow(df) == 0) {
        return(ggplot2::ggplot() +
            ggplot2::theme_void() +
            ggplot2::ggtitle(paste0(title, " (no data)")))
    }

    ggplot2::ggplot(df, ggplot2::aes(
        x = !!rlang::sym(x_col),
        y = !!rlang::sym(y_col),
        size = !!rlang::sym(value_col),
        fill = !!rlang::sym(value_col)
    )) +
        ggplot2::geom_point(shape = 21, color = "#4f7ea6", alpha = 0.95) +
        ggplot2::scale_size(range = c(1.5, 11), limits = c(0, max(df[[value_col]], na.rm = TRUE))) +
        ggplot2::scale_fill_gradient(low = "#edf6fd", high = "#0e4f96", limits = c(0, max(df[[value_col]], na.rm = TRUE))) +
        ggplot2::labs(title = title, x = x_label, y = y_label, size = "Relevance", fill = "Relevance") +
        ggplot2::theme_bw(base_size = 12) +
        ggplot2::theme(
            axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
            panel.grid.minor = ggplot2::element_blank()
        )
}

create_rs_charts <- function(settings,
                             rs_file_path = NULL,
                             output_dir = NULL,
                             level_filter = 0,
                             levels_to_run = 0,
                             strategies_to_show = NULL,
                             top_n_countries = 20,
                             country_counting = "full",
                             formats = c("svg")) {
    levels_to_run <- as.integer(levels_to_run)
    level_filter <- as.integer(level_filter)

    if (!(level_filter %in% levels_to_run)) {
        message("Skipping RS charts for level ", level_filter, " (not in levels_to_run)")
        return(invisible(NULL))
    }

    if (tolower(country_counting) != "full") {
        stop("Only country_counting='full' is currently supported")
    }

    rs_path <- resolve_rs_dataset_path(settings, rs_file_path)
    if (!file.exists(rs_path)) {
        stop("RS dataset not found: ", rs_path)
    }

    if (is.null(output_dir) || !nzchar(trimws(output_dir))) {
        output_dir <- dirname(rs_path)
    }
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

    rs_data <- readr::read_csv(rs_path, show_col_types = FALSE, progress = FALSE)
    if (!("UT" %in% colnames(rs_data))) {
        stop("RS dataset must contain UT column")
    }

    selected_strategies <- normalize_strategy_selection(strategies_to_show, colnames(rs_data))

    cluster_col <- if (level_filter == 0) {
        "level0"
    } else {
        paste0("subcluster_label", level_filter)
    }
    if (!(cluster_col %in% colnames(rs_data))) {
        warning("Cluster column ", cluster_col, " not found; falling back to level0")
        cluster_col <- "level0"
    }
    if (!(cluster_col %in% colnames(rs_data))) {
        stop("No usable cluster column found in RS dataset")
    }

    rs_data$cluster <- clean_cluster_values(rs_data[[cluster_col]])
    rs_data <- rs_data[!is.na(rs_data$cluster) & rs_data$cluster != "", , drop = FALSE]

    cluster_levels <- order_cluster_levels(rs_data$cluster)
    rs_data$cluster <- factor(rs_data$cluster, levels = cluster_levels)

    # -------------------------------------------------------------------------
    # Figure 1: year, strategy, country bars
    # -------------------------------------------------------------------------
    py_vec <- if ("PY" %in% colnames(rs_data)) trimws(as.character(rs_data$PY)) else rep("", nrow(rs_data))
    py_vec <- py_vec[!is.na(py_vec) & py_vec != ""]
    year_df <- as.data.frame(table(py_vec), stringsAsFactors = FALSE)
    colnames(year_df) <- c("PY", "n")
    if (nrow(year_df) > 0) {
        year_df$n <- as.integer(year_df$n)
        suppressWarnings(year_df$PY_num <- as.integer(year_df$PY))
        year_df <- year_df[order(year_df$PY_num, decreasing = TRUE, na.last = TRUE), c("PY", "n"), drop = FALSE]
    }

    strategy_totals <- vapply(
        selected_strategies,
        function(col) sum(coerce_binary(rs_data[[col]]), na.rm = TRUE),
        numeric(1)
    )
    strategy_df <- data.frame(
        strategy = selected_strategies,
        n = as.integer(strategy_totals),
        stringsAsFactors = FALSE
    )

    country_expanded <- split_countries_full_count(rs_data, cluster_col = "cluster")
    country_df <- if (nrow(country_expanded) > 0) {
        as.data.frame(table(country_expanded$country), stringsAsFactors = FALSE)
    } else {
        data.frame(Var1 = character(0), Freq = integer(0), stringsAsFactors = FALSE)
    }
    if (nrow(country_df) > 0) {
        colnames(country_df) <- c("country", "n")
        country_df$n <- as.integer(country_df$n)
        country_df <- country_df[order(country_df$n, decreasing = TRUE), , drop = FALSE]
        country_df <- utils::head(country_df, top_n_countries)
    } else {
        country_df <- data.frame(country = character(0), n = integer(0), stringsAsFactors = FALSE)
    }

    p_year <- plot_horizontal_bar(year_df, "PY", "n", "(a) Publication Year", "Publication year")
    p_strategy <- plot_horizontal_bar(strategy_df, "strategy", "n", "(b) R-strategy", "R strategy")
    p_country <- plot_horizontal_bar(country_df, "country", "n", "(c) Country", "Country")

    save_plot_formats(p_year, file.path(output_dir, "fig_rs_1_year"), formats, width = 5, height = 6)
    save_plot_formats(p_strategy, file.path(output_dir, "fig_rs_1_strategy"), formats, width = 5, height = 6)
    save_plot_formats(p_country, file.path(output_dir, "fig_rs_1_country"), formats, width = 5, height = 6)
    panel_1 <- gridExtra::arrangeGrob(p_year, p_strategy, p_country, ncol = 3)
    save_plot_formats(panel_1, file.path(output_dir, "fig_rs_1_panel"), formats, width = 16, height = 6)

    # -------------------------------------------------------------------------
    # Figure 3: strategy vs cluster bubble matrices
    # -------------------------------------------------------------------------
    strategy_mentions <- build_strategy_mentions(rs_data, selected_strategies, cluster_col = "cluster")
    if (nrow(strategy_mentions) > 0) {
        cs_counts <- as.matrix(table(strategy_mentions$cluster, strategy_mentions$strategy))
        cs_counts <- ensure_matrix_columns(cs_counts, selected_strategies)
        cs_counts <- cs_counts[cluster_levels, , drop = FALSE]

        cs_row <- normalize_rows(cs_counts)
        cs_col <- normalize_cols(cs_counts)

        cs_row_long <- matrix_to_long(cs_row, "cluster", "strategy", "value")
        cs_col_long <- matrix_to_long(cs_col, "cluster", "strategy", "value")

        cs_row_long$strategy <- factor(cs_row_long$strategy, levels = selected_strategies)
        cs_col_long$strategy <- factor(cs_col_long$strategy, levels = selected_strategies)
        cs_row_long$cluster <- factor(cs_row_long$cluster, levels = rev(cluster_levels))
        cs_col_long$cluster <- factor(cs_col_long$cluster, levels = rev(cluster_levels))

        p_cs_row <- plot_bubble_matrix(
            cs_row_long,
            x_col = "strategy", y_col = "cluster", value_col = "value",
            title = "Cluster-wise relevance of R-strategies (row-wise)",
            x_label = "R-strategy", y_label = "Cluster"
        )
        p_cs_col <- plot_bubble_matrix(
            cs_col_long,
            x_col = "strategy", y_col = "cluster", value_col = "value",
            title = "R-strategy-wise relevance of clusters (column-wise)",
            x_label = "R-strategy", y_label = "Cluster"
        )

        save_plot_formats(p_cs_row, file.path(output_dir, "fig_rs_3_cluster_strategy_row"), formats, width = 8, height = 6)
        save_plot_formats(p_cs_col, file.path(output_dir, "fig_rs_3_cluster_strategy_col"), formats, width = 8, height = 6)
        panel_3 <- gridExtra::arrangeGrob(p_cs_row, p_cs_col, ncol = 2)
        save_plot_formats(panel_3, file.path(output_dir, "fig_rs_3_panel"), formats, width = 14, height = 6)
    } else {
        warning("No strategy mentions found for selected strategies; skipping Figure 3 charts")
    }

    # -------------------------------------------------------------------------
    # Figure A.3.1: country vs cluster bubble matrices
    # -------------------------------------------------------------------------
    if (nrow(country_expanded) > 0) {
        cc_counts <- as.matrix(table(country_expanded$cluster, country_expanded$country))
        cc_counts <- cc_counts[cluster_levels, , drop = FALSE]

        top_countries <- names(sort(colSums(cc_counts), decreasing = TRUE))[seq_len(min(top_n_countries, ncol(cc_counts)))]
        cc_counts <- cc_counts[, top_countries, drop = FALSE]

        cc_row <- normalize_rows(cc_counts)
        cc_col <- normalize_cols(cc_counts)

        cc_row_long <- matrix_to_long(cc_row, "cluster", "country", "value")
        cc_col_long <- matrix_to_long(cc_col, "cluster", "country", "value")
        cc_row_long$country <- factor(cc_row_long$country, levels = top_countries)
        cc_col_long$country <- factor(cc_col_long$country, levels = top_countries)
        cc_row_long$cluster <- factor(cc_row_long$cluster, levels = rev(cluster_levels))
        cc_col_long$cluster <- factor(cc_col_long$cluster, levels = rev(cluster_levels))

        p_cc_row <- plot_bubble_matrix(
            cc_row_long,
            x_col = "country", y_col = "cluster", value_col = "value",
            title = "Country concentration per cluster (row-wise)",
            x_label = "Country", y_label = "Cluster"
        )
        p_cc_col <- plot_bubble_matrix(
            cc_col_long,
            x_col = "country", y_col = "cluster", value_col = "value",
            title = "Cluster concentration per country (column-wise)",
            x_label = "Country", y_label = "Cluster"
        )

        save_plot_formats(p_cc_row, file.path(output_dir, "fig_rs_A31_country_cluster_row"), formats, width = 10, height = 6)
        save_plot_formats(p_cc_col, file.path(output_dir, "fig_rs_A31_country_cluster_col"), formats, width = 10, height = 6)
        panel_a31 <- gridExtra::arrangeGrob(p_cc_row, p_cc_col, ncol = 2)
        save_plot_formats(panel_a31, file.path(output_dir, "fig_rs_A31_panel"), formats, width = 16, height = 6)

        # -----------------------------------------------------------------------
        # Figure A.3.2: country vs strategy bubble matrices
        # -----------------------------------------------------------------------
        if (nrow(strategy_mentions) > 0) {
            country_strategy <- merge(
                country_expanded[, c("UT", "cluster", "country"), drop = FALSE],
                strategy_mentions[, c("UT", "cluster", "strategy"), drop = FALSE],
                by = c("UT", "cluster"),
                all = FALSE
            )

            if (nrow(country_strategy) > 0) {
                cs2_counts <- as.matrix(table(country_strategy$country, country_strategy$strategy))
                cs2_counts <- ensure_matrix_columns(cs2_counts, selected_strategies)

                top_countries_cs <- names(sort(rowSums(cs2_counts), decreasing = TRUE))[seq_len(min(top_n_countries, nrow(cs2_counts)))]
                cs2_counts <- cs2_counts[top_countries_cs, selected_strategies, drop = FALSE]

                cs2_row <- normalize_rows(cs2_counts)
                cs2_col <- normalize_cols(cs2_counts)

                cs2_row_long <- matrix_to_long(cs2_row, "country", "strategy", "value")
                cs2_col_long <- matrix_to_long(cs2_col, "country", "strategy", "value")
                cs2_row_long$strategy <- factor(cs2_row_long$strategy, levels = selected_strategies)
                cs2_col_long$strategy <- factor(cs2_col_long$strategy, levels = selected_strategies)
                cs2_row_long$country <- factor(cs2_row_long$country, levels = rev(top_countries_cs))
                cs2_col_long$country <- factor(cs2_col_long$country, levels = rev(top_countries_cs))

                p_cs2_row <- plot_bubble_matrix(
                    cs2_row_long,
                    x_col = "strategy", y_col = "country", value_col = "value",
                    title = "Country-wise relevance of R-strategies (row-wise)",
                    x_label = "R-strategy", y_label = "Country"
                )
                p_cs2_col <- plot_bubble_matrix(
                    cs2_col_long,
                    x_col = "strategy", y_col = "country", value_col = "value",
                    title = "R-strategy-wise relevance of countries (column-wise)",
                    x_label = "R-strategy", y_label = "Country"
                )

                save_plot_formats(p_cs2_row, file.path(output_dir, "fig_rs_A32_country_strategy_row"), formats, width = 10, height = 8)
                save_plot_formats(p_cs2_col, file.path(output_dir, "fig_rs_A32_country_strategy_col"), formats, width = 10, height = 8)
                panel_a32 <- gridExtra::arrangeGrob(p_cs2_row, p_cs2_col, ncol = 2)
                save_plot_formats(panel_a32, file.path(output_dir, "fig_rs_A32_panel"), formats, width = 16, height = 8)
            } else {
                warning("No country-strategy intersections found; skipping Figure A.3.2 charts")
            }
        }
    } else {
        warning("No country data found; skipping country-based charts")
    }

    message(
        "RS charts created for level ", level_filter,
        " in ", output_dir,
        " using strategies: ", paste(selected_strategies, collapse = ", ")
    )

    invisible(list(
        rs_path = rs_path,
        output_dir = output_dir,
        level_filter = level_filter,
        strategies = selected_strategies
    ))
}
