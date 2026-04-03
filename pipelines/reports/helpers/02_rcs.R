print("###################### reports/02_rcs.R")

source("utils/auxiliary_functions.R")

# ---------------------------------------------------------------------------
# Preparation
# ---------------------------------------------------------------------------
myDataCorrect$PY <- suppressWarnings(as.integer(as.character(myDataCorrect$PY)))
myDataCorrect$TI <- myDataCorrect$TI %>%
  iconv(to = "UTF-8", sub = "byte") %>%
  iconv(to = "ASCII//TRANSLIT", sub = "")

clusters <- sort(unique(myDataCorrect$X_C))
network_year <- round(mean(myDataCorrect$PY, na.rm = TRUE), 1)

# Global values for year normalization
current_year <- max(myDataCorrect$PY, na.rm = TRUE)
global_mean_age <- mean(current_year - myDataCorrect$PY, na.rm = TRUE)
global_mean_py  <- mean(myDataCorrect$PY, na.rm = TRUE)

# Per-document Z9 normalizations (grouped by publication year)
myDataCorrect <- myDataCorrect %>%
  dplyr::group_by(PY) %>%
  dplyr::mutate(
    Z9_rank = dplyr::if_else(dplyr::n() > 1, percent_rank(Z9), 0.5),
    Z9_zs   = dplyr::if_else(
      dplyr::n() > 1 & sd(Z9, na.rm = TRUE) > 0,
      as.numeric(scale(Z9)),
      0
    ),
    Z9_log  = {
      temp_log <- log(Z9 + 1)
      dplyr::if_else(
        dplyr::n() > 1 & sd(temp_log, na.rm = TRUE) > 0,
        as.numeric(scale(temp_log)),
        0
      )
    }
  ) %>%
  dplyr::ungroup()

# Load color palette
fukan_palette <- jsonlite::fromJSON(
  file.path(getwd(), "assets", "fukan_colors.json")
)
fukan_colors <- c(fukan_palette$base, tolower(fukan_palette$extended))

valid_fields <- intersect(
  unlist(settings$rp$categorical_long_reports),
  colnames(myDataCorrect)
)

# ---------------------------------------------------------------------------
# RCS label logic (vectorized helper)
# ---------------------------------------------------------------------------
classify_rcs <- function(x, y) {
  dplyr::case_when(
    x > 0 & y > 0  ~ "Change Maker",
    x > 0 & y < 0  ~ "Incremental",
    x < 0 & y > 0  ~ "Breaktrough",
    x < 0 & y < 0  ~ "Matured",
    x == 0 & y > 0 ~ "B & CM",
    x == 0 & y < 0 ~ "M & I",
    x > 0 & y == 0 ~ "CM & I",
    x < 0 & y == 0 ~ "B & M",
    TRUE            ~ "CENTERED"
  )
}

# ---------------------------------------------------------------------------
# Growth rate from PY proportions (needs report_PY_proportions.csv)
# ---------------------------------------------------------------------------
compute_growth_rate <- function(prop_path, year_range, n_clusters) {
  if (!file.exists(prop_path)) return(rep(0, n_clusters))

  py_span <- max(myDataCorrect$PY, na.rm = TRUE) - min(myDataCorrect$PY, na.rm = TRUE)
  if (py_span <= year_range) return(rep(0, n_clusters))

  props <- read.csv(prop_path, check.names = FALSE)
  n <- ncol(props)
  recent  <- props[, (n - year_range + 1):n, drop = FALSE]
  earlier <- props[, (n - year_range):(n - 1), drop = FALSE]
  rowSums(recent - earlier) / (year_range - 1)
}

# ---------------------------------------------------------------------------
# Build core RCS: one row per cluster
# ---------------------------------------------------------------------------
rcs <- lapply(clusters, function(cl) {
  cd <- dplyr::filter(myDataCorrect, X_C == cl)

  cluster_size   <- nrow(cd)
  cluster_year   <- round(mean(cd$PY, na.rm = TRUE), 1)
  cluster_code   <- unique(cd$cluster_code)[1]
  sum_cites      <- sum(cd$Z9, na.rm = TRUE)
  ave_cites      <- sum_cites / cluster_size

  # Hub = document with highest degree
  hub_idx    <- which.max(cd$X_E)
  dmax       <- cd$X_E[hub_idx]
  hub_id     <- cd$X_N[hub_idx]
  hub_title  <- cd$TI[hub_idx]
  hub_year   <- dplyr::coalesce(cd$PY[hub_idx], as.integer(cluster_year))
  hub_type1  <- if (settings$params$dataset_source != "wos") "ARTICLE" else toupper(cd$DT[hub_idx])
  hub_type2  <- if (grepl("overview|review|survey", tolower(hub_title))) "REVIEW" else "ARTICLE"

  # Year normalization metrics
  PY_freq          <- table(cd$PY) %>% sort(decreasing = TRUE)
  weight_PY        <- PY_freq[as.character(cd$PY)]
  py_median        <- median(cd$PY, na.rm = TRUE)
  py_weighted_z9   <- if (sum_cites > 0) sum(cd$PY * cd$Z9, na.rm = TRUE) / sum_cites else cluster_year
  py_weighted_freq <- sum(cd$PY * weight_PY, na.rm = TRUE) / sum(weight_PY, na.rm = TRUE)
  py_price_index   <- mean(cd$PY >= (current_year - 5), na.rm = TRUE)
  py_mean_age      <- mean(current_year - cd$PY, na.rm = TRUE)
  py_norm_age      <- if (global_mean_age != 0) py_mean_age / global_mean_age else 0
  py_norm          <- if (global_mean_py != 0) cluster_year / global_mean_py else 0

  # Z9 normalized aggregates
  z9_ave_rank <- mean(cd$Z9_rank, na.rm = TRUE)
  z9_ave_zs   <- mean(cd$Z9_zs,   na.rm = TRUE)
  z9_ave_log  <- mean(cd$Z9_log,   na.rm = TRUE)

  # Top keywords per categorical field
  tops <- vapply(valid_fields, function(field) {
    paste(names(TopSomething(cd, coll = field)), collapse = "; ") %>% tolower()
  }, character(1))

  dplyr::tibble(
    cluster      = cl,
    cluster_code = cluster_code,
    network_year = network_year,
    cluster_year = cluster_year,
    PY_median           = py_median,
    PY_weighted_mean_z9 = py_weighted_z9,
    PY_weighted_mean_freq = py_weighted_freq,
    PY_price_index      = py_price_index,
    PY_mean_age         = py_mean_age,
    PY_norm_age         = py_norm_age,
    PY_norm             = py_norm,
    hub_year     = hub_year,
    hub_title    = hub_title,
    hub_ID       = hub_id,
    hub_type1    = hub_type1,
    hub_type2    = hub_type2,
    dmax         = dmax,
    sum_cites    = sum_cites,
    ave_cites    = ave_cites,
    Z9_ave_rank  = z9_ave_rank,
    Z9_ave_zs    = z9_ave_zs,
    Z9_ave_log   = z9_ave_log,
    cluster_size = cluster_size,
    !!!setNames(as.list(tops), valid_fields)
  )
}) %>%
  dplyr::bind_rows()

# ---------------------------------------------------------------------------
# Derived metrics and labels
# ---------------------------------------------------------------------------
rcs <- rcs %>%
  dplyr::mutate(
    participation = round(dmax / cluster_size, 3),
    X     = dplyr::coalesce(cluster_year - network_year, 0),
    Y     = dplyr::coalesce(hub_year - cluster_year, 0),
    label = classify_rcs(X, Y),
    growth_rate = compute_growth_rate(
      file.path(output_folder_level, "report_PY_proportions.csv"),
      year_range  = 4,
      n_clusters  = length(clusters)
    )
  )

# Write core RCS (kept for backward compatibility)
write.csv(rcs, file = rn$PROJECTrcs, row.names = FALSE)

# ---------------------------------------------------------------------------
# Merged RCS: enrich with numeric report summaries
# ---------------------------------------------------------------------------
print("###################### reports/rcs_merged.R")

# Load numeric report CSVs and prefix their columns
numeric_fields <- unlist(settings$rp$numerical_reports)
numeric_reports <- lapply(numeric_fields, function(field) {
  path <- file.path(output_folder_level, paste0("report_", field, ".csv"))
  if (!file.exists(path)) return(NULL)

  report <- read.csv(path, check.names = FALSE)
  cluster_col <- grep("^cluster$", colnames(report), ignore.case = TRUE, value = TRUE)
  if (length(cluster_col) == 0) return(NULL)

  non_cluster <- setdiff(colnames(report), cluster_col)
  colnames(report)[colnames(report) %in% non_cluster] <- paste(field, non_cluster, sep = "_")
  colnames(report)[colnames(report) == cluster_col] <- "cluster"
  report
})
numeric_reports <- Filter(Negate(is.null), numeric_reports)

# Build the merged report
rcs_merged <- rcs %>%
  dplyr::select(cluster, cluster_code) %>%
  dplyr::mutate(
    main_cluster        = as.numeric(sub("-.*$", "", as.character(cluster_code))),
    cluster_name        = "",
    documents           = rcs$cluster_size,
    documents_percent   = round(documents * 100 / sum(documents, na.rm = TRUE), 2),
    documents_cumulative = cumsum(documents_percent)
  )

# Join numeric reports
for (nr in numeric_reports) {
  rcs_merged <- dplyr::left_join(rcs_merged, nr, by = "cluster")
}

# Append core RCS fields
rcs_merged <- rcs_merged %>%
  dplyr::mutate(
    # Year metrics
    PY_ave              = rcs$cluster_year,
    PY_median           = rcs$PY_median,
    PY_weighted_mean_z9 = rcs$PY_weighted_mean_z9,
    PY_weighted_mean_freq = rcs$PY_weighted_mean_freq,
    PY_price_index      = rcs$PY_price_index,
    PY_mean_age         = rcs$PY_mean_age,
    PY_norm_age         = rcs$PY_norm_age,
    PY_norm             = rcs$PY_norm,
    global_age          = global_mean_age,
    global_py           = global_mean_py,
    # Citation metrics
    sum_cites     = rcs$sum_cites,
    ave_cites     = rcs$ave_cites,
    Z9_ave_rank   = rcs$Z9_ave_rank,
    Z9_ave_zs     = rcs$Z9_ave_zs,
    Z9_ave_log    = rcs$Z9_ave_log,
    # Cluster dynamics
    participation = rcs$participation,
    growth_rate   = rcs$growth_rate,
    rcs_label     = rcs$label,
    # Hub info
    hub_title     = rcs$hub_title,
    hub_year      = rcs$hub_year,
    hub_degree    = rcs$dmax,
    hub_id        = rcs$hub_ID,
    hub_type1     = rcs$hub_type1,
    hub_type2     = rcs$hub_type2
  )

# Append categorical long-report columns present in core rcs
for (field in valid_fields) {
  if (field %in% colnames(rcs)) {
    rcs_merged[[field]] <- rcs[[field]]
  }
}

# ---------------------------------------------------------------------------
# Cluster color assignment from fukan palette
# ---------------------------------------------------------------------------
unique_main_clusters <- sort(unique(rcs_merged$main_cluster))
cluster_colors <- setNames(
  rep_len(fukan_colors, length(unique_main_clusters)),
  unique_main_clusters
)
rcs_merged$cluster_color <- cluster_colors[as.character(rcs_merged$main_cluster)]

# Write final merged RCS
write.csv(
  rcs_merged,
  file.path(output_folder_level, "rcs_merged.csv"),
  row.names = FALSE
)
