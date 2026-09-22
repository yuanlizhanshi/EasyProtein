# -----------------------------------------------------------
# QC Functions for EasyProtein
# -----------------------------------------------------------

#' Fix duplicated protein IDs
#'
#' @description
#' Detect and automatically rename duplicated IDs in a proteomics matrix.
#'
#' @param df A data.frame containing protein information.
#' @param id_col Column name containing protein identifiers.
#'
#' @return A data.frame with fixed unique IDs.
#' @export
fix_duplicate_protein_ids <- function(df, id_col = "Protein.Ids") {
  ids <- df[[id_col]]
  dup_idx <- which(duplicated(ids) | duplicated(ids, fromLast = TRUE))

  if (length(dup_idx) > 0) {
    dup_vals <- unique(ids[dup_idx])
    warning(
      sprintf(
        "Detected duplicated %s values: %s\n",
        id_col,
        paste(dup_vals, collapse = ", ")
      ),
      call. = FALSE
    )

    for (dup in dup_vals) {
      idx <- which(ids == dup)
      ids[idx] <- paste0(dup, "_", seq_along(idx))
    }
    df[[id_col]] <- ids
  }

  return(df)
}


#' Normalize feature identifiers
#'
#' @description
#' Extract the leading token before the first semicolon, replace missing or
#' invalid identifiers with sequential \code{unknown} labels, and append
#' suffixes such as \code{.1}, \code{.2} to duplicated identifiers to ensure
#' uniqueness.
#'
#' @param x A character vector containing raw feature identifiers.
#'
#' @return A character vector of normalized, unique feature identifiers.
#' @keywords internal
normalize_feature_ids <- function(x) {
  feature_ids <- stringr::str_extract(x, "^[^;]+")
  feature_ids <- trimws(as.character(feature_ids))

  invalid_idx <- is.na(feature_ids) | feature_ids == ""
  if (any(invalid_idx)) {
    feature_ids[invalid_idx] <- paste0("unknown", seq_len(sum(invalid_idx)))
  }

  make_unique_ids <- function(ids) {
    counts <- new.env(parent = emptyenv())

    vapply(ids, function(id) {
      if (!exists(id, envir = counts, inherits = FALSE)) {
        assign(id, 0L, envir = counts)
        return(id)
      }

      current_count <- get(id, envir = counts, inherits = FALSE) + 1L
      assign(id, current_count, envir = counts)
      paste0(id, ".", current_count)
    }, character(1))
  }

  make_unique_ids(feature_ids)
}


#' Calculate median gene CV within each raw-data group
#'
#' @description
#' Read the same raw expression table accepted by \code{rawdata2se()}, parse
#' sample conditions using the same sample-name rule, calculate each gene's CV
#' across replicates within each condition, and return the median CV per
#' condition. Missing values are ignored.
#'
#' @param exp_file Path to the raw expression table file.
#' @param obs_col Name of the column in \code{exp_file} used to define feature
#'   identifiers. Only the substring before the first semicolon (\code{;}) will
#'   be used as the feature name.
#' @param raw_prefix Character string used to identify raw expression columns.
#'   If \code{"auto"} (default), raw intensity columns are detected using the
#'   same filename-style suffix rules as \code{rawdata2se()}.
#' @param group_by Column in the parsed sample metadata used for grouping.
#'   Default is \code{"condition"}.
#'
#' @return A data.frame with one row per group, including the median gene CV
#'   and the 95th percentile CV across replicated samples in that group.
#' @export
calc_group_median_gene_cv_from_exp_file <- function(
    exp_file,
    obs_col = "Genes",
    raw_prefix = "auto",
    group_by = "condition"
) {
  rawdata <- data.table::fread(exp_file) %>% as.data.frame()
  colnames(rawdata) <- gsub("\\\\", "/", colnames(rawdata))

  stopifnot(obs_col %in% colnames(rawdata))

  rawdata$feature <- normalize_feature_ids(rawdata[[obs_col]])

  detect_raw_cols <- function(nms, raw_prefix = "auto") {
    nms_lower <- tolower(nms)

    if (!identical(tolower(raw_prefix), "auto")) {
      return(stringr::str_detect(nms_lower, tolower(raw_prefix)))
    }

    suffix_pattern <- paste0(
      "(",
      "\\.(raw|mzml|mzxml|wiff|d|dia|txt)$",
      "|_raw$|_mzml$|_mzxml$|_wiff$|_dia$",
      "|raw$|mzml$|mzxml$|wiff$",
      ")"
    )

    detected <- stringr::str_detect(nms_lower, suffix_pattern)

    if (!any(detected)) {
      fallback_pattern <- "raw|mzml|mzxml|wiff|\\.d$|_d$"
      detected <- stringr::str_detect(nms_lower, fallback_pattern)
    }

    detected
  }

  raw_cols <- detect_raw_cols(colnames(rawdata), raw_prefix = raw_prefix)
  stopifnot(any(raw_cols))

  rawdata_mtx <- rawdata[, raw_cols, drop = FALSE]
  rawdata_mtx <- as.data.frame(lapply(rawdata_mtx, function(x) {
    as.numeric(as.character(x))
  }))
  rownames(rawdata_mtx) <- rawdata$feature
  colnames(rawdata_mtx) <- tools::file_path_sans_ext(
    basename(colnames(rawdata[, raw_cols, drop = FALSE]))
  )

  obs <- tibble::tibble(
    sample = colnames(rawdata_mtx),
    condition = stringr::str_extract(sample, "\\w+(?=_[^_]*$)"),
    rep = stringr::str_extract(sample, "\\d+$"),
    group = paste0(condition, "#", rep)
  ) %>% as.data.frame()

  if (all(is.na(obs$condition))) obs$condition <- obs$sample
  stopifnot(group_by %in% colnames(obs))

  group_values <- as.character(obs[[group_by]])
  group_levels <- unique(group_values)
  mat <- as.matrix(rawdata_mtx)
  feature_names <- rownames(mat)

  res_list <- lapply(group_levels, function(g) {
    idx <- which(group_values == g)
    submat <- mat[, idx, drop = FALSE]

    if (ncol(submat) < 2) {
      cv_val <- rep(NA_real_, nrow(submat))
    } else {
      mean_val <- rowMeans(submat, na.rm = TRUE)
      sd_val <- apply(submat, 1, stats::sd, na.rm = TRUE)
      cv_val <- sd_val / mean_val
      cv_val[mean_val == 0 | !is.finite(cv_val)] <- NA_real_
    }

    data.frame(
      feature = feature_names,
      group = g,
      CV = cv_val,
      stringsAsFactors = FALSE
    )
  })

  cv_df <- do.call(rbind, res_list)

  summarise_cv_stat <- function(x, fun) {
    x <- x[is.finite(x)]

    if (length(x) == 0) {
      return(NA_real_)
    }

    fun(x)
  }

  out <- cv_df %>%
    dplyr::group_by(group) %>%
    dplyr::summarise(
      median_CV = summarise_cv_stat(CV, stats::median),
      high95_CV = summarise_cv_stat(
        CV,
        function(x) stats::quantile(x, probs = 0.95, names = FALSE)
      ),
      .groups = "drop"
    )

  out$median_CV[is.nan(out$median_CV)] <- NA_real_
  out$high95_CV[is.nan(out$high95_CV)] <- NA_real_
  out$median_CV <- ifelse(
    is.na(out$median_CV),
    NA_character_,
    sprintf("%.3f", out$median_CV)
  )
  out$high95_CV <- ifelse(
    is.na(out$high95_CV),
    NA_character_,
    sprintf("%.3f", out$high95_CV)
  )
  colnames(out)[colnames(out) == "group"] <- group_by
  out
}


.impute_raw_matrix <- function(
    mat,
    condition,
    low_prob = 0.001,
    return_log2 = FALSE,
    seed = 1,
    enable_impute_with_replicate = TRUE
) {
  set.seed(seed)
  cond_levels <- sort(unique(condition))
  cond_cols <- split(seq_len(ncol(mat)), condition)[as.character(cond_levels)]

  out_log_full <- matrix(
    NA_real_,
    nrow = nrow(mat),
    ncol = ncol(mat),
    dimnames = list(rownames(mat), colnames(mat))
  )
  shifts <- numeric(length(cond_cols))
  rng_cells <- list()

  for (bi in seq_along(cond_cols)) {
    cols <- cond_cols[[bi]]
    sub <- mat[, cols, drop = FALSE]

    min_pos <- suppressWarnings(min(sub, na.rm = TRUE))
    shift <- if (is.finite(min_pos) && min_pos > 0) 0 else (abs(min_pos) + 1)
    mat_log <- log2(sub + shift)

    col_quant <- apply(mat_log, 2, function(v) {
      v2 <- v[is.finite(v)]
      if (length(v2)) {
        stats::quantile(v2, probs = low_prob, na.rm = TRUE)
      } else {
        NA_real_
      }
    })
    col_mins <- apply(mat_log, 2, function(v) {
      v2 <- v[is.finite(v)]
      if (length(v2)) min(v2, na.rm = TRUE) else NA_real_
    })

    cond_low_log <- suppressWarnings(
      stats::quantile(as.numeric(mat_log), probs = low_prob, na.rm = TRUE)
    )
    if (!is.finite(cond_low_log)) cond_low_log <- -20

    n_reps <- ncol(mat_log)
    miss_cnt <- rowSums(is.na(mat_log))
    out_log <- mat_log
    na_idx_all <- which(is.na(mat_log), arr.ind = TRUE)

    if (enable_impute_with_replicate) {
      take_mean <- which(miss_cnt > 0 & miss_cnt < n_reps / 2)
      if (length(take_mean) > 0) {
        row_means <- rowMeans(mat_log[take_mean, , drop = FALSE], na.rm = TRUE)
        cells <- na_idx_all[na_idx_all[, 1] %in% take_mean, , drop = FALSE]
        out_log[cells] <- row_means[match(cells[, 1], take_mean)]
      }
    }

    take_low <- which(
      miss_cnt > 0 &
        (!enable_impute_with_replicate | miss_cnt >= n_reps / 2)
    )
    if (length(take_low) > 0) {
      n_obs <- n_reps - miss_cnt
      cells <- na_idx_all[na_idx_all[, 1] %in% take_low, , drop = FALSE]
      cells <- cells[order(cells[, 1], cells[, 2]), , drop = FALSE]

      if (enable_impute_with_replicate) {
        zero_obs <- take_low[n_obs[take_low] == 0]
        if (length(zero_obs) > 0) {
          zcells <- cells[cells[, 1] %in% zero_obs, , drop = FALSE]
          out_log[zcells] <- cond_low_log
          cells <- cells[!(cells[, 1] %in% zero_obs), , drop = FALSE]
        }
      }

      if (nrow(cells) > 0) {
        q1 <- col_quant[cells[, 2]]
        m1 <- col_mins[cells[, 2]]
        bad_qm <- !is.finite(q1) | !is.finite(m1)
        lo <- pmin(m1, q1)
        hi <- pmax(m1, q1)
        use_quantile <- !bad_qm &
          (!is.finite(lo) | !is.finite(hi) | lo == hi)
        use_random <- !(bad_qm | use_quantile)

        out_log[cells[bad_qm, , drop = FALSE]] <- cond_low_log
        out_log[cells[use_quantile, , drop = FALSE]] <- q1[use_quantile]

        if (any(use_random)) {
          random_cells <- cells[use_random, , drop = FALSE]
          random_lo <- lo[use_random]
          random_hi <- hi[use_random]
          for (k in seq_len(nrow(random_cells))) {
            rng_cells[[length(rng_cells) + 1]] <- list(
              block = bi,
              row = random_cells[k, 1],
              col_local = random_cells[k, 2],
              lo = random_lo[k],
              hi = random_hi[k]
            )
          }
        }
      }
    }

    out_log_full[, cols] <- out_log
    shifts[bi] <- shift
  }

  if (length(rng_cells) > 0) {
    random_values <- stats::runif(length(rng_cells))
    for (k in seq_along(rng_cells)) {
      cell <- rng_cells[[k]]
      out_log_full[cell$row, cond_cols[[cell$block]][cell$col_local]] <-
        cell$lo + random_values[k] * (cell$hi - cell$lo)
    }
  }

  if (return_log2) return(out_log_full)

  out_linear <- out_log_full
  for (bi in seq_along(cond_cols)) {
    cols <- cond_cols[[bi]]
    out_linear[, cols] <- pmax(2^out_log_full[, cols, drop = FALSE] - shifts[bi], 0)
  }
  out_linear
}


.scale_matrix_by_row <- function(mtx) {
  row_means <- rowMeans(mtx)
  centered <- sweep(mtx, 1, row_means, "-")
  row_sds <- sqrt(rowSums(centered^2) / (ncol(mtx) - 1))
  out <- centered / row_sds
  colnames(out) <- colnames(mtx)
  out
}


#' Construct a SummarizedExperiment object from raw expression table
#'
#' This function reads a raw expression table, performs feature-level
#' filtering, optional outlier masking, missing-value imputation, and
#' stability filtering, and returns a \code{SummarizedExperiment} object
#' together with diagnostic gene lists.
#'
#' @param exp_file Path to the raw expression table file. The file must
#'   contain one column specifying feature identifiers and multiple
#'   columns containing raw expression values.
#'
#' @param obs_col Name of the column in \code{exp_file} used to define
#'   feature identifiers. Only the substring before the first semicolon
#'   (\code{;}) will be used as the feature name.
#'
#' @param raw_prefix Character string used to identify raw expression
#'   columns. If \'auto\' (default), raw intensity columns will be detected
#'   automatically from common raw file suffixes such as \'raw\', \'mzML\',
#'   \'mzXML\', and similar filename-style endings.
#'
#' @param enable_detect_outlier_gene Logical flag indicating whether to
#'   mask extreme outlier measurements within each condition based on
#'   fold-change from the median.
#'
#' @param fc_threshold Numeric threshold for fold-change-based outlier
#'   detection. Values exceeding this fold change relative to the
#'   condition median will be set to \code{NA} when outlier detection is
#'   enabled.
#'
#' @param min_valid_groups Minimum number of conditions in which a feature
#'   must pass the missing-value filter to be retained. Features with
#'   fewer valid conditions are removed.
#'
#' @param frac_NA_threshold Maximum allowed fraction of missing values
#'   within a condition for that condition to be considered valid for a
#'   given feature.
#'
#' @param min_stable_groups Minimum number of conditions in which a feature
#'   must show low variability (coefficient of variation below
#'   \code{cv_threshold}) to be retained.
#'
#' @param cv_threshold Numeric threshold on coefficient of variation (CV)
#'   used to define feature stability within each condition.
#'
#' @return A list with class \code{"RawDataSE"} containing:
#'   \itemize{
#'     \item \code{se}: A \code{SummarizedExperiment} object storing raw
#'       intensities, imputed intensities, CPM-normalized values, and
#'       z-scored expression.
#'     \item \code{cv_df}: A data frame containing a pre-imputation
#'       feature-by-condition CV matrix calculated before stability
#'       filtering.
#'     \item \code{un_stable_gene}: A data frame containing the original
#'       input rows of features removed due to high variability across
#'       conditions.
#'     \item \code{missing_gene_df}: A data frame containing the original
#'       input rows of features removed due to excessive missing values.
#'   }
#'
#' @export
rawdata2se <- function(
    exp_file,
    obs_col = "Genes",
    raw_prefix = "auto",
    enable_detect_outlier_gene = FALSE,
    fc_threshold = 5,
    min_valid_groups = 0,
    frac_NA_threshold = 0.5,
    min_stable_groups = 0,
    cv_threshold = 0.5
) {

  stage_start <- proc.time()[["elapsed"]]
  progress <- function(step) {
    elapsed <- proc.time()[["elapsed"]] - stage_start
    message(sprintf("[rawdata2se] %s took %.2f seconds", step, elapsed))
    stage_start <<- proc.time()[["elapsed"]]
  }

  rawdata <- data.table::fread(exp_file) %>% as.data.frame()
  colnames(rawdata) <- gsub("\\\\", "/", colnames(rawdata))

  stopifnot(obs_col %in% colnames(rawdata))

  rawdata$feature <- normalize_feature_ids(rawdata[[obs_col]])

  var <- data.frame(gene = rawdata$feature)
  rownames(var) <- var$gene

  progress("Reading input")

  detect_raw_cols <- function(nms, raw_prefix = "auto") {
    nms_lower <- tolower(nms)

    if (!identical(tolower(raw_prefix), "auto")) {
      return(stringr::str_detect(nms_lower, tolower(raw_prefix)))
    }

    suffix_pattern <- paste0(
      "(",
      "\\.(raw|mzml|mzxml|wiff|d|dia|txt)$",
      "|_raw$|_mzml$|_mzxml$|_wiff$|_dia$",
      "|raw$|mzml$|mzxml$|wiff$",
      ")"
    )

    detected <- stringr::str_detect(nms_lower, suffix_pattern)

    if (!any(detected)) {
      fallback_pattern <- "raw|mzml|mzxml|wiff|\\.d$|_d$"
      detected <- stringr::str_detect(nms_lower, fallback_pattern)
    }

    detected
  }

  raw_cols <- detect_raw_cols(colnames(rawdata), raw_prefix = raw_prefix)
  stopifnot(any(raw_cols))

  progress("Detecting raw columns")

  rawdata_mtx <- rawdata[, raw_cols, drop = FALSE]
  rownames(rawdata_mtx) <- rawdata$feature
  colnames(rawdata_mtx) <- tools::file_path_sans_ext(
    basename(colnames(rawdata_mtx))
  )

  obs <- tibble::tibble(
    sample = colnames(rawdata_mtx),
    condition = stringr::str_extract(sample, "\\w+(?=_[^_]*$)"),
    rep = stringr::str_extract(sample, "\\d+$"),
    group = paste0(condition, "#", rep)
  ) %>% as.data.frame()

  if (all(is.na(obs$condition))) obs$condition <- obs$sample
  rownames(obs) <- obs$sample

  single_rep <- all(table(obs$condition) <= 1)

  mat_raw <- as.matrix(rawdata_mtx)

  progress("Preprocessing")

  if (single_rep) {
    mat <- .impute_raw_matrix(mat_raw, obs$condition)
    mat <- mat[sort(rownames(mat), method = "radix"), , drop = FALSE]

    cpm_mtx <- edgeR::cpm(mat)
    cpm_mtx[!is.finite(cpm_mtx)] <- NA

    progress("Imputation")

    se <- SummarizedExperiment::SummarizedExperiment(
      assays = list(
        raw_intensity = rawdata_mtx[rownames(mat), colnames(mat)],
        intensity = mat,
        conc = cpm_mtx,
        zscale = .scale_matrix_by_row(cpm_mtx)
      ),
      rowData = S4Vectors::DataFrame(var[rownames(mat), , drop = FALSE]),
      colData = S4Vectors::DataFrame(obs[colnames(mat), ])
    )

    progress("Building SummarizedExperiment")

    progress("Done")
    return(structure(
      list(se = se, cv_df = NULL, un_stable_gene = NULL, missing_gene_df = NULL),
      class = "RawDataSE"
    ))
  }

  condition <- obs$condition
  condition_levels <- sort(unique(condition))
  condition_columns <- split(seq_along(condition), condition)[condition_levels]

  if (enable_detect_outlier_gene) {
    median_matrix <- matrix(
      NA_real_,
      nrow = nrow(mat_raw),
      ncol = length(condition_levels)
    )
    for (g in seq_along(condition_levels)) {
      median_matrix[, g] <- matrixStats::rowMedians(
        mat_raw[, condition_columns[[g]], drop = FALSE],
        na.rm = TRUE
      )
    }
    median_per_sample <- median_matrix[
      ,
      match(condition, condition_levels),
      drop = FALSE
    ]
    ratio <- mat_raw / median_per_sample
    fold_change <- ifelse(ratio < 1, 1 / ratio, ratio)
    fold_change[is.na(ratio)] <- NA_real_
    mat_raw[fold_change > fc_threshold] <- NA
  }

  fraction_missing <- matrix(
    NA_real_,
    nrow = nrow(mat_raw),
    ncol = length(condition_levels)
  )
  for (g in seq_along(condition_levels)) {
    fraction_missing[, g] <- rowMeans(
      is.na(mat_raw[, condition_columns[[g]], drop = FALSE])
    )
  }
  n_valid_groups <- rowSums(fraction_missing <= frac_NA_threshold)
  missing_feature <- rownames(mat_raw)[n_valid_groups < min_valid_groups]

  keep_feature <- !(rownames(mat_raw) %in% missing_feature)
  mat_raw <- mat_raw[keep_feature, , drop = FALSE]
  rawdata_mtx <- rawdata_mtx[keep_feature, , drop = FALSE]

  missing_gene_df <- rawdata %>%
    dplyr::filter(feature %in% missing_feature)

  progress("Missing-value filtering")

  cv_se <- SummarizedExperiment::SummarizedExperiment(
    assays = list(raw_intensity = mat_raw),
    rowData = S4Vectors::DataFrame(var[rownames(mat_raw), , drop = FALSE]),
    colData = S4Vectors::DataFrame(obs[colnames(mat_raw), ])
  )

  cv_long_df <- calc_gene_CV_by_condition(cv_se, assay_name = "raw_intensity")

  if (is.null(cv_long_df)) {
    un_stable_cv_df <- data.frame(feature = character(0), stringsAsFactors = FALSE)
    cv_df <- NULL
  } else {
    un_stable_cv_df <- cv_long_df %>%
      dplyr::group_by(feature) %>%
      dplyr::summarise(
        n_stable_groups = sum(CV < cv_threshold, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::filter(n_stable_groups < min_stable_groups)

    cv_df <- cv_long_df %>%
      dplyr::select(feature, condition, CV) %>%
      tidyr::pivot_wider(names_from = condition, values_from = CV)
  }

  keep_stable <- !(rownames(mat_raw) %in% un_stable_cv_df$feature)
  mat_raw <- mat_raw[keep_stable, , drop = FALSE]
  rawdata_mtx <- rawdata_mtx[keep_stable, , drop = FALSE]

  un_stable_gene_df <- rawdata %>%
    dplyr::filter(feature %in% un_stable_cv_df$feature)

  progress("Stability filtering")

  mat <- .impute_raw_matrix(mat_raw, condition)
  mat <- mat[sort(rownames(mat), method = "radix"), , drop = FALSE]
  mat <- mat[, obs$sample, drop = FALSE]

  cpm_mtx <- edgeR::cpm(mat)
  cpm_mtx[!is.finite(cpm_mtx)] <- NA

  progress("Imputation")

  se <- SummarizedExperiment::SummarizedExperiment(
    assays = list(
      raw_intensity = rawdata_mtx[rownames(mat), colnames(mat)],
      intensity = mat,
      conc = cpm_mtx,
      zscale = .scale_matrix_by_row(cpm_mtx)
    ),
    rowData = S4Vectors::DataFrame(var[rownames(mat), , drop = FALSE]),
    colData = S4Vectors::DataFrame(obs[colnames(mat), ])
  )

  progress("Building SummarizedExperiment")

  progress("Done")
  list(
    se = se,
    cv_df = cv_df,
    un_stable_gene = un_stable_gene_df,
    missing_gene_df = missing_gene_df
  )
}

