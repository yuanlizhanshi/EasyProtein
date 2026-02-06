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


#' Import raw data and construct SummarizedExperiment for QC
#'
#' @description
#' Read MaxQuant-like protein intensity matrix, perform QC filtering,
#' remove missing genes, impute missing values, filter unstable proteins,
#' and return a \code{SummarizedExperiment} object containing raw,
#' imputed, CPM, and z-scaled assays.
#'
#' @param exp_file Path to raw intensity file.
#' @param obs_col Column containing protein identifiers.
#' @param enable_detect_outlier_gene Logical, perform FC-based outlier removal.
#' @param fc_threshold Fold-change threshold for detecting outliers.
#' @param valid_group_cutoff Minimum number of valid groups after NA filtering.
#' @param frac_NA_threshold NA fraction threshold per group.
#' @param stable_group_cutoff Minimum number of groups with stable CV.
#' @param cv_threshod CV threshold for determining stable genes.
#'
#' @return A list containing:
#' \describe{
#'   \item{se}{A \code{SummarizedExperiment} object}
#'   \item{un_stable_gene}{Genes removed due to unstable CV}
#'   \item{missing_gene_df}{Genes removed due to excessive missingness}
#' }
#' @export
rawdata2se <- function(
    exp_file,
    enable_detect_outlier_gene  = FALSE,
    fc_threshold = 5,
    valid_group_cutoff = -1,
    frac_NA_threshold = 0.5,
    stable_group_cutoff = -1,
    cv_threshod = 0.5
) {

  rawdata <- data.table::fread(exp_file) %>% as.data.frame()
  colnames(rawdata) <- gsub("\\\\", "/", colnames(rawdata))

  stopifnot("Genes" %in% colnames(rawdata))


  rawdata$feature <- stringr::str_extract(rawdata$Genes, "^[^;]+")

  rawdata <- rawdata %>%
    dplyr::filter(!is.na(feature), feature != "") %>%
    dplyr::filter(!duplicated(feature))


  var <- rawdata %>%
    dplyr::select(feature, Genes) %>%
    as.data.frame()

  rownames(var) <- var$feature


  rawdata_mtx <- rawdata[, stringr::str_which(colnames(rawdata), "raw")]
  rownames(rawdata_mtx) <- rawdata$feature
  colnames(rawdata_mtx) <- tools::file_path_sans_ext(basename(colnames(rawdata_mtx)))


  obs <- tibble::tibble(
    sample = colnames(rawdata_mtx),
    condition = stringr::str_extract(sample, "\\w+(?=_[^_]*$)"),
    rep = stringr::str_extract(sample, "\\d+$"),
    group = paste0(condition, "#", rep)
  ) %>% as.data.frame()

  if (all(is.na(obs$condition))) obs$condition <- obs$sample
  rownames(obs) <- obs$sample

  rep_counts <- table(obs$condition)
  single_rep <- all(rep_counts <= 1)

  rawdata_df <- rawdata_mtx %>%
    tibble::rownames_to_column("feature") %>%
    tidyr::pivot_longer(
      cols = -feature,
      names_to = "sample",
      values_to = "raw_value"
    ) %>%
    dplyr::left_join(obs, by = "sample")


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

    rawdata_impute_df_mtx <- as.matrix(rawdata_impute_df_wide[, -1])
    rownames(rawdata_impute_df_mtx) <- rawdata_impute_df_wide$feature
    rawdata_impute_df_mtx <- rawdata_impute_df_mtx[, obs$sample]

    se <- SummarizedExperiment::SummarizedExperiment(
      assays = list(
        raw_intensity = rawdata_mtx[
          rownames(rawdata_impute_df_mtx),
          colnames(rawdata_impute_df_mtx)
        ],
        intensity = rawdata_impute_df_mtx,
        conc = edgeR::cpm(rawdata_impute_df_mtx),
        zscale = scale_mtx(edgeR::cpm(rawdata_impute_df_mtx))
      ),
      rowData = S4Vectors::DataFrame(var[rownames(rawdata_impute_df_mtx), ]),
      colData = S4Vectors::DataFrame(obs[colnames(rawdata_impute_df_mtx), ])
    )

    return(list(
      se = se,
      un_stable_gene = NULL,
      missing_gene_df = NULL
    ))
  }

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


  missing_feature <- rawdata_df %>%
    dplyr::group_by(feature, condition) %>%
    dplyr::summarise(
      frac_NA = mean(is.na(raw_value)),
      .groups = "drop"
    ) %>%
    dplyr::mutate(valid = frac_NA < frac_NA_threshold) %>%
    dplyr::group_by(feature) %>%
    dplyr::summarise(
      n_valid_groups = sum(valid),
      .groups = "drop"
    ) %>%
    dplyr::filter(n_valid_groups <= valid_group_cutoff)

  rawdata_df <- rawdata_df %>%
    dplyr::filter(!feature %in% missing_feature$feature)

  missing_gene_df <- rawdata %>%
    dplyr::filter(feature %in% missing_feature$feature)


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

  rawdata_impute_df_mtx <- as.matrix(rawdata_impute_df_wide[, -1])
  rownames(rawdata_impute_df_mtx) <- rawdata_impute_df_wide$feature
  rawdata_impute_df_mtx <- rawdata_impute_df_mtx[, obs$sample]


  se <- SummarizedExperiment::SummarizedExperiment(
    assays = list(
      raw_intensity = rawdata_mtx[
        rownames(rawdata_impute_df_mtx),
        colnames(rawdata_impute_df_mtx)
      ],
      intensity = rawdata_impute_df_mtx,
      conc = edgeR::cpm(rawdata_impute_df_mtx),
      zscale = scale_mtx(edgeR::cpm(rawdata_impute_df_mtx))
    ),
    rowData = S4Vectors::DataFrame(var[rownames(rawdata_impute_df_mtx), ]),
    colData = S4Vectors::DataFrame(obs[colnames(rawdata_impute_df_mtx), ])
  )


  cv_df <- calc_feature_CV_by_condition(se)

  un_stable_cv_df <- cv_df %>%
    dplyr::group_by(feature) %>%
    dplyr::summarise(
      n_stable_groups = sum(CV < cv_threshod, na.rm = TRUE),
      total_groups = sum(!is.na(CV)),
      .groups = "drop"
    ) %>%
    dplyr::filter(n_stable_groups <= stable_group_cutoff)

  un_stable_gene_df <- as.data.frame(
    rowData(se)[un_stable_cv_df$feature, ]
  )

  se <- se[!rownames(se) %in% un_stable_cv_df$feature, ]

  return(list(
    se = se,
    un_stable_gene = un_stable_gene_df,
    missing_gene_df = missing_gene_df
  ))
}
