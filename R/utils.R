# ----------------------------------------------
# Utils:  helper functions for EasyProtein
# ----------------------------------------------

#' Scale matrix by row
#'
#' @description
#' Scale each row of a numeric matrix to mean 0 and sd 1.
#'
#' @param mtx A numeric matrix.
#'
#' @return A scaled matrix with the same dimensions.
#' @export
scale_mtx <- function(mtx) {
  new_mtx <- as.matrix(t(apply(mtx, 1, scale)))
  colnames(new_mtx) <- colnames(mtx)
  return(new_mtx)
}

#' Geometric mean
#'
#' @description
#' Compute geometric mean, optionally removing NA values.
#'
#' @param x Numeric vector.
#' @param na.rm Whether to remove NA values.
#'
#' @return A numeric value representing the geometric mean.
#' @export
geom_mean <- function(x, na.rm = TRUE) {
  if (na.rm) x <- x[!is.na(x)]
  exp(mean(log(x)))
}

#' Category table summarizer
#'
#' @description
#' Produce a table summarizing how many values fall within specified cut
#' categories, optionally cumulative.
#'
#' @param data Numeric vector.
#' @param categories Numeric vector of category boundaries.
#' @param cumulative Logical, whether cumulative counts should be returned.
#' @param na.rm Logical, remove NAs before computation.
#' @param digits Number of digits to round results.
#'
#' @return A 2-row matrix summarizing counts and proportions.
#' @export
catable <- function(data,
                    categories = c(
                      quantile(data, c(0.01, 0.1, 0.5, 0.9, 0.99),
                               na.rm = TRUE)
                    ),
                    cumulative = FALSE,
                    na.rm = TRUE,
                    digits = 3) {

  if (!is(data, "numeric"))
    stop("data should be numeric vector")
  if (!is(categories, "numeric"))
    stop("categories should be numeric vector")

  categories <- sort(categories)
  tot <- sum(!is.na(data), na.rm = na.rm)

  outmat <- matrix(0, nrow = 2, ncol = length(categories) + 1)

  # First category: X <= categories[1]
  outmat[1, 1] <- sum(data <= categories[1], na.rm = na.rm)
  outmat[2, 1] <- outmat[1, 1] / tot

  # Intermediate categories
  for (i in 1:(length(categories) - 1)) {
    outmat[1, i + 1] <- sum(data > categories[i] & data <= categories[i + 1], na.rm = na.rm)
    outmat[2, i + 1] <- outmat[1, i + 1] / tot
  }

  # Last category: X > max(categories)
  outmat[1, length(categories) + 1] <-
    sum(data > categories[length(categories)], na.rm = na.rm)
  outmat[2, length(categories) + 1] <-
    outmat[1, length(categories) + 1] / tot

  # Cumulative mode
  if (cumulative) {
    outmat[1, ] <- cumsum(outmat[1, ])
    outmat[2, ] <- cumsum(outmat[2, ])
  }

  # Column names
  cnams <- character(length(categories) + 1)
  cnams[1] <- paste0("X<=", categories[1])

  for (i in 1:(length(categories) - 1)) {
    if (cumulative)
      cnams[i + 1] <- paste0("X<=", categories[i + 1])
    else
      cnams[i + 1] <- paste0(categories[i], "<X<=", categories[i + 1])
  }

  if (cumulative)
    cnams[length(categories) + 1] <- "all X"
  else
    cnams[length(categories) + 1] <- paste0("X>", categories[length(categories)])

  colnames(outmat) <- cnams
  rownames(outmat) <- c("No", "Prop")

  round(outmat, digits = digits)
}


# -----------------------------------------------------------
# Functions for converting SummarizedExperiment to data.frame
# and extracting DEG results
# -----------------------------------------------------------

#' Convert SummarizedExperiment to raw intensity data.frame
#'
#' @param se A SummarizedExperiment object.
#' @return A data.frame containing rowData and raw_intensity assay.
#' @export
se2raw <- function(se) {
  df <- cbind(as.data.frame(rowData(se)),
              as.data.frame(assay(se, "raw_intensity")))
  return(df)
}

#' Convert SummarizedExperiment to intensity data.frame
#'
#' @param se A SummarizedExperiment object.
#' @return A data.frame containing rowData and intensity assay.
#' @export
se2internstiy <- function(se) {
  df <- cbind(as.data.frame(rowData(se)),
              as.data.frame(assay(se, "intensity")))
  return(df)
}

#' Convert SummarizedExperiment to concentration (CPM) data.frame
#'
#' @param se A SummarizedExperiment object.
#' @return A data.frame containing rowData and conc assay.
#' @export
se2conc <- function(se) {
  df <- cbind(as.data.frame(rowData(se)),
              as.data.frame(assay(se, "conc")))
  return(df)
}

#' Convert SummarizedExperiment to z-scaled CPM data.frame
#'
#' @param se A SummarizedExperiment object.
#' @return A data.frame containing rowData and zscale assay.
#' @export
se2scale <- function(se) {
  df <- cbind(as.data.frame(rowData(se)),
              as.data.frame(assay(se, "zscale")))
  return(df)
}

#' Extract DEG results from SummarizedExperiment
#'
#' @description
#' This function performs differential expression analysis using
#' limma-based methods (limma_protein_DE or limma_protein_DE_pair),
#' and returns a data.frame containing:
#' - rowData(se)
#' - expression matrix
#' - DEG statistics
#' - group median expression values
#'
#' @param se A SummarizedExperiment object.
#' @param compare_col Column in colData(se) used to define groups.
#' @param ref Reference group (optional).
#' @param cmp Comparison group (optional).
#' @param pair_col Optional column for paired analysis.
#'
#' @return A data.frame with DEG statistics and annotations.
#' @export
se2DEGs <- function(se,
                    compare_col = "condition",
                    ref = NULL,
                    cmp = NULL,
                    pair_col = NULL) {

  mtx <- assay(se, "conc")
  meta <- as.data.frame(colData(se))

  if (!compare_col %in% colnames(meta)) {
    stop(paste0("Column ", compare_col, " does not exist in colData(se)."))
  }

  # subset to desired ref vs cmp
  if (!is.null(ref) && !is.null(cmp)) {
    keep <- meta[[compare_col]] %in% c(ref, cmp)
    se <- se[, keep]
    mtx <- assay(se, "conc")
    meta <- as.data.frame(colData(se))
    group <- factor(meta[[compare_col]], levels = c(ref, cmp))
  } else {
    group <- factor(meta[[compare_col]])
  }

  # paired or unpaired analysis
  if (is.null(pair_col)) {
    DEGs_res <- limma_protein_DE(mtx, group)
  } else {
    pair_factor <- factor(meta[[pair_col]])
    DEGs_res <- limma_protein_DE_pair(mtx, group, pair_factor)
  }

  # expression matrix as df
  exp_df <- as.data.frame(mtx) %>%
    tibble::rownames_to_column("Protein_id")

  # compute group medians
  med_df <- as.data.frame(mtx) %>%
    tibble::rownames_to_column("Protein_id") %>%
    tidyr::pivot_longer(-Protein_id, names_to = "sample", values_to = "value")

  meta2 <- meta %>% tibble::rownames_to_column(".sample_id")

  med_df <- med_df %>%
    dplyr::left_join(meta2, by = c("sample" = ".sample_id")) %>%
    dplyr::group_by(Protein_id, !!rlang::sym(compare_col)) %>%
    dplyr::summarize(median_expr = median(value, na.rm = TRUE), .groups = "drop") %>%
    tidyr::pivot_wider(
      names_from = !!rlang::sym(compare_col),
      values_from = median_expr,
      names_prefix = "median_"
    )

  # join information
  final_df <- as.data.frame(rowData(se)) %>%
    dplyr::left_join(exp_df, by = c("Protein.Ids" = "Protein_id")) %>%
    dplyr::left_join(DEGs_res[, c(1, 8, 9, 2, 5, 6)], by = c("Protein.Ids" = "feature")) %>%
    dplyr::left_join(med_df, by = c("Protein.Ids" = "Protein_id"))

  # store attributes so downstream functions know group info
  attr(final_df, "ref_group") <- ref
  attr(final_df, "cmp_group") <- cmp

  return(final_df)
}


# -----------------------------------------------------------
# Batch DEG extraction and gene grouping utilities
# -----------------------------------------------------------

#' Run pairwise DEG comparisons for all conditions except a reference
#'
#' @param se A SummarizedExperiment object.
#' @param compare_col Column in colData(se) representing conditions.
#' @param start_col Index specifying which condition to use as reference.
#'
#' @return A named list of DEG data.frames.
#' @export
get_batch_DEGs <- function(se,
                           compare_col = "condition",
                           start_col = 1) {

  groups <- unique(se[[compare_col]])
  ref_group <- groups[start_col]

  deg_list <- purrr::map(
    groups[-start_col],
    function(x) {
      se2DEGs(
        se,
        compare_col = compare_col,
        ref = ref_group,
        cmp = x
      )
    }
  )

  names(deg_list) <- groups[-start_col]
  return(deg_list)
}


#' Assign genes into dynamic groups based on expression, correlation, and CV
#'
#' @description
#' This function identifies gene behavior patterns (Up, Down, Stable,
#' fluctuated, Low expression) based on:
#' - time-course correlation
#' - differential expression
#' - mean expression
#' - coefficient of variation (CV)
#'
#' @param se A SummarizedExperiment object.
#' @param assay Assay name to use (default "conc").
#' @param group_by Column in colData(se) defining sample ordering.
#' @param cor_method Use "All" (raw expression) or "Mean" (expression by group).
#' @param time_threhold Vector of two thresholds for up/down classification.
#' @param min_expresion_threhold Minimum mean expression.
#' @param CV_with_time_threhold CV threshold used to define stable genes.
#' @param padj_threhold Adjusted p-value threshold for DEGs.
#'
#' @return Updated SummarizedExperiment with gene grouping added to rowData.
#' @export
se2gene_group <- function(se,
                          assay = "conc",
                          group_by = "condition",
                          cor_method = "All",
                          time_threhold = c(-0.75, 0.75),
                          min_expresion_threhold = 10,
                          CV_with_time_threhold = 0.2,
                          padj_threhold = 0.05) {

  expr <- assay(se, assay)
  expr_log <- log2(expr)
  expr_mean <- calc_gene_mean_by_condition(se)

  if (!all(rownames(expr_mean) == rownames(expr))) {
    stop("Row order mismatch between expression matrix and mean-expression matrix.")
  }

  time_factor <- factor(se[[group_by]], levels = unique(se[[group_by]]))
  design <- model.matrix(~ 0 + time_factor)
  colnames(design) <- levels(time_factor)

  fit <- limma::lmFit(expr, design)
  fit <- limma::eBayes(fit)
  anova_res <- limma::topTable(fit, number = Inf)

  sig_gene <- anova_res %>%
    tibble::rownames_to_column("Protein.Ids") %>%
    dplyr::filter(adj.P.Val <= 0.01)

  unsig_gene <- anova_res %>%
    tibble::rownames_to_column("Protein.Ids") %>%
    dplyr::filter(!(Protein.Ids %in% sig_gene$Protein.Ids))

  # correlation across time
  if (cor_method == "All") {
    gene_info <- tibble::tibble(
      Protein.Ids = rownames(expr),
      Genes = rowData(se)[["Genes"]],
      Mean_expression = rowMeans(expr),
      Median_exprssion = matrixStats::rowMedians(expr),
      CV_with_time = apply(expr_mean, 1, calculate_cv),
      Spearman_with_time = apply(expr, 1, function(x) {
        cor(x, seq_len(ncol(expr)), method = "spearman")
      })
    )
  } else {
    gene_info <- tibble::tibble(
      Protein.Ids = rownames(expr),
      Genes = rowData(se)[["Genes"]],
      Mean_expression = rowMeans(expr),
      Median_exprssion = matrixStats::rowMedians(expr),
      CV_with_time = apply(expr_mean, 1, calculate_cv),
      Spearman_with_time = apply(expr_mean, 1, function(x) {
        cor(x, seq_len(ncol(expr_mean)), method = "spearman")
      })
    )
  }

  # expression filters
  low_mean_gene <- dplyr::filter(gene_info, Mean_expression < min_expresion_threhold)
  gene_not_low <- dplyr::filter(gene_info, Mean_expression >= min_expresion_threhold)

  up_gene <- gene_not_low %>%
    dplyr::filter(
      Protein.Ids %in% sig_gene$Protein.Ids,
      Spearman_with_time > time_threhold[2]
    )

  down_gene <- gene_not_low %>%
    dplyr::filter(
      Protein.Ids %in% sig_gene$Protein.Ids,
      Spearman_with_time < time_threhold[1]
    )

  adaptive_gene <- gene_not_low %>%
    dplyr::filter(Protein.Ids %in% sig_gene$Protein.Ids) %>%
    dplyr::filter(!(Protein.Ids %in% c(up_gene$Protein.Ids, down_gene$Protein.Ids)))

  stable_gene <- adaptive_gene %>%
    dplyr::filter(CV_with_time <= CV_with_time_threhold)

  fluctuated_gene <- adaptive_gene %>%
    dplyr::filter(CV_with_time > CV_with_time_threhold)

  # FC matrix from all comparisons
  all_deg <- get_batch_DEGs(se, compare_col = "condition", start_col = 1)

  fc_mat <- purrr::map2(all_deg, names(all_deg), function(df, nm) {
    df %>%
      dplyr::filter(adj.P.Val <= padj_threhold) %>%
      dplyr::select(Protein.Ids, logFC) %>%
      dplyr::rename(!!nm := logFC)
  }) %>%
    purrr::reduce(dplyr::full_join, by = "Protein.Ids") %>%
    dplyr::mutate(dplyr::across(everything(), ~ tidyr::replace_na(.x, 0)))

  colnames(fc_mat)[-1] <- paste0("log2FC_", colnames(fc_mat)[-1])

  gene_info_full <- gene_info %>%
    dplyr::left_join(fc_mat, by = "Protein.Ids") %>%
    dplyr::mutate(dplyr::across(everything(), ~ tidyr::replace_na(.x, 0)))

  # assign gene groups
  gene_info_full$gene_group <- dplyr::case_when(
    gene_info_full$Protein.Ids %in% up_gene$Protein.Ids ~ "Up",
    gene_info_full$Protein.Ids %in% down_gene$Protein.Ids ~ "Down",
    gene_info_full$Protein.Ids %in% c(stable_gene$Protein.Ids, unsig_gene$Protein.Ids) ~ "Stable",
    gene_info_full$Protein.Ids %in% fluctuated_gene$Protein.Ids ~ "Fluctuated",
    gene_info_full$Protein.Ids %in% low_mean_gene$Protein.Ids ~ "Low expression"
  )

  # add to rowData
  if (all(rownames(rowData(se)) == gene_info_full$Protein.Ids)) {
    for (col in colnames(gene_info_full)) {
      if (!(col %in% colnames(rowData(se)))) {
        rowData(se)[[col]] <- gene_info_full[[col]]
      }
    }
  }

  return(se)
}


#' Summarize assay values by groups in colData(se)
#'
#' @param se SummarizedExperiment object.
#' @param group_by Column defining groups.
#' @param assay_name Assay to summarize.
#' @param method "mean" or "median".
#' @param na.rm Logical.
#'
#' @return A matrix of summarized expression.
#' @export
summarize_se_by_coldata <- function(se,
                                    group_by,
                                    assay_name = "conc",
                                    method = c("mean", "median"),
                                    na.rm = TRUE) {

  method <- match.arg(method)

  if (!inherits(se, "SummarizedExperiment"))
    stop("`se` must be a SummarizedExperiment object.")

  if (!assay_name %in% SummarizedExperiment::assayNames(se))
    stop(sprintf("Assay '%s' not found.", assay_name))

  cd <- SummarizedExperiment::colData(se)

  if (!group_by %in% colnames(cd))
    stop(sprintf("Column '%s' not found in colData(se).", group_by))

  grp <- as.factor(cd[[group_by]])
  lv <- levels(grp)

  mat <- SummarizedExperiment::assay(se, assay_name)

  summarized <- sapply(lv, function(g) {
    idx <- which(grp == g)
    if (length(idx) == 0) {
      rep(NA_real_, nrow(mat))
    } else if (method == "mean") {
      rowMeans(mat[, idx, drop = FALSE], na.rm = na.rm)
    } else {
      apply(mat[, idx, drop = FALSE], 1, stats::median, na.rm = na.rm)
    }
  })

  rownames(summarized) <- rownames(mat)
  colnames(summarized) <- lv

  return(summarized)
}


#' Calculate coefficient of variation
#'
#' @param x Numeric vector.
#' @export
calculate_cv <- function(x) {
  m <- mean(x)
  s <- sd(x)
  return(s / m)
}

#' Count turning points in vector
#'
#' @param vec Numeric vector.
#' @param fluctuate Allowed fluctuation adjustment.
#'
#' @return Number of turning points.
#' @export
get_turning_point <- function(vec, fluctuate = 0.05) {
  diff_vec <- diff(vec) + vec[-length(vec)] * fluctuate
  direction <- sign(diff_vec)
  turning_point_index <- which(diff(direction) != 0) + 1
  return(length(turning_point_index))
}

#' Calculate coefficient of variation (CV) for each gene per condition
#'
#' @param se SummarizedExperiment object.
#' @param assay_name Assay used for CV calculation (default "conc").
#' @param condition_col Column in colData(se) defining conditions.
#'
#' @return A data.frame containing CV values for each gene per condition.
#' @export

calc_gene_CV_by_condition <- function(se,
                                      assay_name = "conc",
                                      condition_col = "condition") {

  stopifnot(assay_name %in% assayNames(se))
  stopifnot(condition_col %in% colnames(colData(se)))

  if (length(unique(se[[condition_col]])) == ncol(se)) {
    return(NULL)
  }

  mtx <- assay(se, assay_name)
  meta <- as.data.frame(colData(se))

  df_long <- as.data.frame(mtx) %>%
    tibble::rownames_to_column("Protein.Ids") %>%
    tidyr::pivot_longer(cols = -Protein.Ids,
                        names_to = "sample",
                        values_to = "value") %>%
    dplyr::left_join(meta, by = "sample")

  cv_df <- df_long %>%
    dplyr::group_by(Protein.Ids, !!rlang::sym(condition_col)) %>%
    dplyr::summarise(
      mean_val = mean(value, na.rm = TRUE),
      sd_val = sd(value, na.rm = TRUE),
      CV = sd_val / mean_val,
      n = sum(!is.na(value)),
      .groups = "drop"
    )

  return(cv_df)
}

#' Calculate mean expression per gene per condition
#'
#' @param se SummarizedExperiment object.
#' @param assay_name Assay used for mean calculation.
#' @param condition_col Column in colData(se) defining conditions.
#'
#' @return A numeric matrix of gene-by-condition mean expression.
#' @export

calc_gene_mean_by_condition <- function(se,
                                        assay_name = "conc",
                                        condition_col = "condition") {

  stopifnot(assay_name %in% assayNames(se))
  stopifnot(condition_col %in% colnames(colData(se)))

  if (length(unique(se[[condition_col]])) == ncol(se)) {
    return(NULL)
  }

  mtx <- assay(se, assay_name)
  meta <- as.data.frame(colData(se))

  df_long <- as.data.frame(mtx) %>%
    tibble::rownames_to_column("Protein.Ids") %>%
    tidyr::pivot_longer(cols = -Protein.Ids,
                        names_to = "sample",
                        values_to = "value") %>%
    dplyr::left_join(meta, by = "sample")

  mean_df <- df_long %>%
    dplyr::group_by(Protein.Ids, !!rlang::sym(condition_col)) %>%
    dplyr::summarise(
      mean_val = mean(value, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    tidyr::pivot_wider(
      names_from = !!rlang::sym(condition_col),
      values_from = mean_val
    )

  mean_mat <- as.data.frame(mean_df)
  rownames(mean_mat) <- mean_mat$Protein.Ids
  mean_mat$Protein.Ids <- NULL

  mean_mat <- mean_mat[rownames(mtx), , drop = FALSE]
  return(as.matrix(mean_mat))
}

