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
#'   columns. All columns whose names contain this prefix will be treated
#'   as raw intensity measurements.
#'
#' @param enable_detect_outlier_gene Logical flag indicating whether to
#'   mask extreme outlier measurements within each condition based on
#'   fold-change from the median.
#'
#' @param fc_threshold Numeric threshold for fold-change–based outlier
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
#'     \item \code{un_stable_gene}: A data frame of features removed due to
#'       high variability across conditions.
#'     \item \code{missing_gene_df}: A data frame of features removed due
#'       to excessive missing values.
#'   }
#'
#' @export
rawdata2se <- function(
    exp_file,
    obs_col = "Gene",
    raw_prefix = "raw",
    enable_detect_outlier_gene = FALSE,
    fc_threshold = 5,
    min_valid_groups = 0,
    frac_NA_threshold = 0.5,
    min_stable_groups = 0,
    cv_threshold = 0.5
) {

  rawdata <- data.table::fread(exp_file) %>% as.data.frame()
  colnames(rawdata) <- gsub("\\\\", "/", colnames(rawdata))

  stopifnot(obs_col %in% colnames(rawdata))

  ## feature
  rawdata$feature <- stringr::str_extract(rawdata[[obs_col]], "^[^;]+")

  rawdata <- rawdata %>%
    dplyr::filter(!is.na(feature), feature != "") %>%
    dplyr::filter(!duplicated(feature))

  ## rowData
  var <- data.frame(feature = rawdata$feature)
  rownames(var) <- var$feature

  ## raw intensity matrix
  raw_cols <- stringr::str_detect(colnames(rawdata), raw_prefix)
  stopifnot(any(raw_cols))

  rawdata_mtx <- rawdata[, raw_cols, drop = FALSE]
  rownames(rawdata_mtx) <- rawdata$feature
  colnames(rawdata_mtx) <- tools::file_path_sans_ext(
    basename(colnames(rawdata_mtx))
  )

  ## colData
  obs <- tibble::tibble(
    sample = colnames(rawdata_mtx),
    condition = stringr::str_extract(sample, "\\w+(?=_[^_]*$)"),
    rep = stringr::str_extract(sample, "\\d+$"),
    group = paste0(condition, "#", rep)
  ) %>% as.data.frame()

  if (all(is.na(obs$condition))) obs$condition <- obs$sample
  rownames(obs) <- obs$sample

  single_rep <- all(table(obs$condition) <= 1)

  rawdata_df <- rawdata_mtx %>%
    tibble::rownames_to_column("feature") %>%
    tidyr::pivot_longer(
      cols = -feature,
      names_to = "sample",
      values_to = "raw_value"
    ) %>%
    dplyr::left_join(obs, by = "sample")

  ## single replicate shortcut
  if (single_rep) {

    rawdata_impute_df <- impute_low1pct_or_median_raw(
      rawdata_df,
      id_col = "feature"
    )

    rawdata_impute_df_wide <- rawdata_impute_df %>%
      tidyr::pivot_wider(
        id_cols = feature,
        names_from = sample,
        values_from = raw_value
      )

    mat <- as.matrix(rawdata_impute_df_wide[, -1])
    rownames(mat) <- rawdata_impute_df_wide$feature
    mat <- mat[, obs$sample]

    cpm_mtx <- edgeR::cpm(mat)
    cpm_mtx[!is.finite(cpm_mtx)] <- NA

    se <- SummarizedExperiment::SummarizedExperiment(
      assays = list(
        raw_intensity = rawdata_mtx[rownames(mat), colnames(mat)],
        intensity = mat,
        conc = cpm_mtx,
        zscale = scale_mtx(cpm_mtx)
      ),
      rowData = S4Vectors::DataFrame(var[rownames(mat), ]),
      colData = S4Vectors::DataFrame(obs[colnames(mat), ])
    )

    return(structure(
      list(se = se, un_stable_gene = NULL, missing_gene_df = NULL),
      class = "RawDataSE"
    ))
  }

  ## FC outlier
  rawdata_df <- rawdata_df %>%
    dplyr::group_by(feature, condition) %>%
    dplyr::mutate(
      med_value = median(raw_value, na.rm = TRUE),
      fc = if_else(
        raw_value / med_value < 1,
        med_value / raw_value,
        raw_value / med_value
      )
    ) %>%
    dplyr::ungroup()

  if (enable_detect_outlier_gene) {
    rawdata_df$raw_value[rawdata_df$fc > fc_threshold] <- NA
  }

  ## missing filter
  missing_feature <- rawdata_df %>%
    dplyr::group_by(feature, condition) %>%
    dplyr::summarise(
      frac_NA = mean(is.na(raw_value)),
      .groups = "drop"
    ) %>%
    dplyr::mutate(valid = frac_NA < frac_NA_threshold) %>%
    dplyr::group_by(feature) %>%
    dplyr::summarise(n_valid_groups = sum(valid), .groups = "drop") %>%
    dplyr::filter(n_valid_groups < min_valid_groups)

  rawdata_df <- rawdata_df %>%
    dplyr::filter(!feature %in% missing_feature$feature)

  missing_gene_df <- rawdata %>%
    dplyr::filter(feature %in% missing_feature$feature)

  ## impute
  rawdata_impute_df <- impute_low1pct_or_median_raw(
    rawdata_df,
    id_col = "feature"
  )

  rawdata_impute_df_wide <- rawdata_impute_df %>%
    tidyr::pivot_wider(
      id_cols = feature,
      names_from = sample,
      values_from = raw_value
    )

  mat <- as.matrix(rawdata_impute_df_wide[, -1])
  rownames(mat) <- rawdata_impute_df_wide$feature
  mat <- mat[, obs$sample]

  cpm_mtx <- edgeR::cpm(mat)
  cpm_mtx[!is.finite(cpm_mtx)] <- NA

  se <- SummarizedExperiment::SummarizedExperiment(
    assays = list(
      raw_intensity = rawdata_mtx[rownames(mat), colnames(mat)],
      intensity = mat,
      conc = cpm_mtx,
      zscale = scale_mtx(cpm_mtx)
    ),
    rowData = S4Vectors::DataFrame(var[rownames(mat), ]),
    colData = S4Vectors::DataFrame(obs[colnames(mat), ])
  )

  ## CV filter
  cv_df <- calc_feature_CV_by_condition(se)

  un_stable_cv_df <- cv_df %>%
    dplyr::group_by(feature) %>%
    dplyr::summarise(
      n_stable_groups = sum(CV < cv_threshold, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::filter(n_stable_groups < min_stable_groups)

  un_stable_gene_df <- as.data.frame(
    rowData(se)[un_stable_cv_df$feature, ]
  )

  se <- se[!rownames(se) %in% un_stable_cv_df$feature, ]

  list(
    se = se,
    un_stable_gene = un_stable_gene_df,
    missing_gene_df = missing_gene_df
  )
}
