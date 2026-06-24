# -----------------------------------------------------------
# Useful tools: species detection, imputation, DE analysis,
# clustering utilities, etc.
# -----------------------------------------------------------

#' Detect species (human or mouse) from gene symbols
#'
#' @param gene Character vector of gene symbols.
#' @return "human", "mouse", or "unknown".
#' @export
detect_species_from_symbol <- function(gene) {
  gene <- as.character(gene)
  gene <- trimws(gene)
  gene <- gene[gene != ""]

  if (length(gene) == 0) return(NA)

  human_like <- sum(grepl("^[A-Z0-9]+$", gene))
  mouse_like <- sum(grepl("^[A-Z][a-z0-9]+$", gene))

  frac_human <- human_like / length(gene)
  frac_mouse <- mouse_like / length(gene)

  if (frac_human > 0.7 && frac_human > frac_mouse) {
    return("human")
  } else if (frac_mouse > 0.7 && frac_mouse > frac_human) {
    return("mouse")
  } else {
    return("unknown")
  }
}


#' Calculate gene-wise correlation to target gene profile
#'
#' @description
#' Computes the correlation between every gene in a
#' \code{SummarizedExperiment} object and a target gene profile across all
#' samples. When multiple target genes are provided, their sample-wise median
#' expression is used as the reference profile.
#'
#' The target genes can be matched either against row names of the assay matrix
#' or, when available, against \code{rowData(se)$gene}.
#'
#' @param se A \code{SummarizedExperiment} object.
#' @param gene A character vector of one or more target genes.
#' @param assay_name Character. Assay name used for correlation calculation.
#'   Default is \code{"conc"}.
#' @param method Character. Correlation method passed to \code{stats::cor}.
#'   Default is \code{"pearson"}.
#'
#' @return A data.frame with one row per gene and correlation values.
#' The returned columns include:
#' \describe{
#'   \item{feature_id}{Row names of the assay matrix.}
#'   \item{gene}{Gene label. Uses \code{rowData(se)$gene} when available,
#'   otherwise row names.}
#'   \item{correlation}{Correlation between each gene and the target profile.}
#' }
#'
#'
#' @importFrom SummarizedExperiment assay assayNames rowData
#' @export
correlate_genes_to_target <- function(se,
                                      gene,
                                      assay_name = "conc",
                                      method = "spearman") {
  stopifnot(assay_name %in% SummarizedExperiment::assayNames(se))

  gene <- as.character(gene)
  gene <- unique(stats::na.omit(trimws(gene)))

  if (length(gene) == 0) {
    stop("gene must contain at least one non-empty gene name")
  }

  expr <- SummarizedExperiment::assay(se, assay_name)
  expr <- as.matrix(expr)

  gene_ids <- rownames(expr)
  if (is.null(gene_ids)) {
    stop("rownames of assay matrix are required")
  }

  target_idx <- which(gene_ids %in% gene)
  if (length(target_idx) == 0) {
    stop("None of the input genes were found in rownames(se)")
  }

  missing_genes <- setdiff(gene, gene_ids)
  if (length(missing_genes) > 0) {
    warning(
      "These genes were not found in rownames(se): ",
      paste(missing_genes, collapse = ", ")
    )
  }

  target_expr <- expr[target_idx, , drop = FALSE]
  if (nrow(target_expr) == 1) {
    target_profile <- as.numeric(target_expr[1, ])
  } else {
    target_profile <- apply(target_expr, 2, stats::median, na.rm = TRUE)
  }

  correlation <- apply(expr, 1, function(x) {
    stats::cor(x, target_profile, use = "pairwise.complete.obs", method = method)
  })

  tibble(
    gene = gene_ids,
    correlation = as.numeric(correlation)
  ) %>% arrange(desc(correlation))
}


#' Impute missing values in raw intensity using low-percentile sampling or geometric mean
#'
#' @description
#' Missing values within each condition are imputed either from the low-intensity
#' distribution or via geometric mean when the majority of values are missing.
#'
#' @param df Long-format data.frame containing raw intensities.
#' @param id_col Protein ID column.
#' @param sample_col Sample name column.
#' @param value_col Value column for raw intensity.
#' @param condition_col Condition or group column.
#' @param low_prob Low percentile used for imputation (default 0.01).
#' @param return_log2 Return log2 values instead of raw (default FALSE).
#' @param seed Random seed.
#' @param enable_impute_with_replicate All defects are randomly filled in at the low end (default TRUE).
#'
#' @return Long-format data.frame with imputed values.
#' @export
impute_low1pct_or_median_raw <- function(
    df,
    id_col        = "Protein.Ids",
    sample_col    = "sample",
    value_col     = "raw_value",
    condition_col = "condition",
    low_prob      = 0.001,
    return_log2   = FALSE,
    seed          = 1,
    enable_impute_with_replicate = TRUE
){
  stopifnot(all(c(id_col, sample_col, value_col, condition_col) %in% names(df)))
  set.seed(seed)

  res <- split(df, df[[condition_col]]) %>% lapply(function(dd){

    wide <- dd %>%
      dplyr::select(dplyr::all_of(c(id_col, sample_col, value_col))) %>%
      dplyr::distinct() %>%
      tidyr::pivot_wider(
        names_from  = dplyr::all_of(sample_col),
        values_from = dplyr::all_of(value_col)
      ) %>%
      as.data.frame()

    rn  <- wide[[id_col]]
    mat <- as.matrix(wide[, setdiff(names(wide), id_col), drop = FALSE])
    rownames(mat) <- rn

    min_pos <- suppressWarnings(min(mat, na.rm = TRUE))
    shift   <- if (is.finite(min_pos) && min_pos > 0) 0 else (abs(min_pos) + 1)
    mat_log <- log2(mat + shift)

    col_quant <- apply(mat_log, 2, function(v){
      v2 <- v[is.finite(v)]
      if (length(v2)) stats::quantile(v2, probs = low_prob, na.rm = TRUE) else NA_real_
    })
    col_mins <- apply(mat_log, 2, function(v){
      v2 <- v[is.finite(v)]
      if (length(v2)) min(v2, na.rm = TRUE) else NA_real_
    })

    cond_low_log <- suppressWarnings(
      stats::quantile(as.numeric(mat_log), probs = low_prob, na.rm = TRUE)
    )
    if (!is.finite(cond_low_log)) cond_low_log <- -20

    n_reps    <- ncol(mat_log)
    miss_cnt  <- rowSums(is.na(mat_log))

    for (i in seq_len(nrow(mat_log))) {
      x <- mat_log[i, ]
      na_idx <- is.na(x)
      if (!any(na_idx)) next

      if(enable_impute_with_replicate){

      if (miss_cnt[i] < n_reps / 2) {
        # missing less than half  -> geometric mean
        obs <- x[!na_idx]
        med <- mean(obs, na.rm = TRUE)
        x[na_idx] <- med

      } else {
        # missing more than half  -> low-end sampling
        obs <- x[!na_idx]
        if (length(obs) == 0) {
          x[na_idx] <- cond_low_log
        } else {
        for (j in which(na_idx)) {
          q1 <- col_quant[j]
          m1 <- col_mins[j]
          if (!is.finite(q1) || !is.finite(m1)) {
            x[j] <- cond_low_log
          } else {
            lo <- min(m1, q1)
            hi <- max(m1, q1)
            if (!is.finite(lo) || !is.finite(hi) || lo == hi) {
              x[j] <- q1
            } else {
              x[j] <- stats::runif(1, min = lo, max = hi)
            }
          }
        }}
      }
      } else {
         for (j in which(na_idx)) {
          q1 <- col_quant[j]
          m1 <- col_mins[j]
          if (!is.finite(q1) || !is.finite(m1)) {
            x[j] <- cond_low_log
          } else {
            lo <- min(m1, q1)
            hi <- max(m1, q1)
            if (!is.finite(lo) || !is.finite(hi) || lo == hi) {
              x[j] <- q1
            } else {
              x[j] <- stats::runif(1, min = lo, max = hi)
            }
          }
        }
      }

      mat_log[i, ] <- x
    }

    if (return_log2) {
      mat_out <- mat_log
    } else {
      mat_out <- pmax(2^mat_log - shift, 0)
    }

    wide_imputed <- data.frame(rn, mat_out, check.names = FALSE)
    names(wide_imputed)[1] <- id_col

    long_imputed <- wide_imputed %>%
      tidyr::pivot_longer(
        cols = -dplyr::all_of(id_col),
        names_to  = sample_col,
        values_to = value_col
      ) %>%
      dplyr::mutate(!!condition_col := unique(dd[[condition_col]])[1]) %>%
      dplyr::select(dplyr::all_of(c(id_col, sample_col, value_col, condition_col)))

    long_imputed
  })

  dplyr::bind_rows(res) %>%
    dplyr::arrange(.data[[id_col]], .data[[condition_col]], .data[[sample_col]])
}


#' Perform fuzzy clustering on gene-level time-course matrix using Mfuzz
#'
#' @description
#' Applies fuzzy c-means clustering (via \pkg{Mfuzz}) to a gene-by-sample
#' numeric matrix (e.g., log2FC, expression, or kinetic values).
#' Each gene is assigned to the cluster with the highest membership score.
#'
#' This function is typically used to classify dynamic genes (e.g., DEGs)
#' into temporal response patterns.
#'
#' @param mat A numeric matrix or data.frame with genes in rows and
#'   samples/time points in columns. Row names must contain gene IDs.
#' @param k Integer. Number of clusters (default = 10).
#' @param min_sd Numeric. Minimum standard deviation threshold for filtering
#'   low-variance genes prior to clustering (default = 0).
#'
#' @return A tibble with three columns:
#' \describe{
#'   \item{gene}{Gene identifier (rownames of input matrix).}
#'   \item{gene_group}{Cluster assignment label (e.g., "C1", "C2", ...).}
#'   \item{type}{Character label, default set to "DEGs".}
#' }
#'
#' @details
#' The workflow includes:
#' \enumerate{
#'   \item Filtering genes by row-wise standard deviation.
#'   \item Z-score standardization using \code{Mfuzz::standardise()}.
#'   \item Automatic estimation of fuzzifier parameter \code{m}.
#'   \item Fuzzy c-means clustering.
#'   \item Hard assignment based on maximum membership.
#' }
#'
#' @seealso \code{\link[Mfuzz]{mfuzz}}, \code{\link[Mfuzz]{mestimate}},
#'   \code{\link[Mfuzz]{standardise}}
#'
#' @importFrom Biobase ExpressionSet
#' @importFrom Mfuzz standardise mestimate mfuzz
#' @importFrom tibble tibble
#'
#' @examples
#' \dontrun{
#' # Example: clustering a log2FC matrix
#' set.seed(1)
#' mat <- matrix(rnorm(1000), nrow = 100)
#' rownames(mat) <- paste0("Gene", 1:100)
#' colnames(mat) <- paste0("T", 1:10)
#'
#' res <- run_mfuzz_clustering(mat, k = 5)
#' head(res)
#' }
#'
#' @export
run_mfuzz_clustering <- function(mat, k = 10, min_sd = 0) {
  if (!requireNamespace("Mfuzz", quietly = TRUE)) {
    stop("Package 'Mfuzz' is required but not installed.")
  }
  if (!requireNamespace("Biobase", quietly = TRUE)) {
    stop("Package 'Biobase' is required but not installed.")
  }
  stopifnot(is.matrix(mat) || is.data.frame(mat))
  mat <- as.matrix(mat)

  if (is.null(rownames(mat)))
    stop("mat must have rownames as gene IDs")

  sd_all <- apply(mat, 1, sd, na.rm = TRUE)
  mat <- mat[sd_all >= min_sd, , drop = FALSE]
  eset <- Biobase::ExpressionSet(mat)

  eset <- Mfuzz::standardise(eset)
  m <- Mfuzz::mestimate(eset)
  cl <- Mfuzz::mfuzz(eset, c = k, m = m)
  membership <- cl$membership
  cluster_id <- apply(membership, 1, which.max)

  tibble::tibble(
    gene = rownames(membership),
    gene_group = paste0("C", cluster_id),
    type = 'DEGs'
  )
}

#' Classify genes by multi-round monotonic time-course patterns
#'
#' This function classifies genes based on monotonic temporal trends
#' using an iterative Spearman correlation strategy across progressively
#' trimmed time windows. At each round, genes exhibiting significant
#' positive or negative monotonicity are assigned to Up or
#' Down groups corresponding to the round at which the trend
#' first becomes detectable.
#'
#' The time window is shortened by removing one boundary sample
#' (head, tail, or automatically selected) after each round, until
#' fewer than min_len samples remain.
#'
#' @param mat A numeric matrix with genes in rows and time-ordered samples
#'   in columns. Row names must correspond to gene identifiers.
#' @param rho_cut Numeric scalar specifying the absolute Spearman correlation
#'   threshold for calling a monotonic trend. Default is 0.75.
#' @param min_len Integer specifying the minimum number of samples required
#'   to perform trend classification. Default is 4.
#' @param trim_side Character string specifying how to trim the time window
#'   at each iteration. One of:

#' @return A data.frame with one row per gene and the following columns:gene,gene_group,type

classify_timecourse_by_round <- function(
    mat,
    rho_cut = 0.75,
    min_len = 4,
    trim_side = c("best", "head", "tail")
) {
  trim_side <- match.arg(trim_side)

  # ---- basic checks ----
  if (!is.matrix(mat)) mat <- as.matrix(mat)
  if (is.null(rownames(mat))) stop("The mat must have rownames as the gene name")
  if (ncol(mat) < min_len) stop("The number of columns is less than min_len, so classification cannot start")

  genes_all <- rownames(mat)
  group <- rep(NA_character_, length(genes_all))
  names(group) <- genes_all


  remaining <- genes_all


  cols <- seq_len(ncol(mat))

  round_id <- 1L

  while (length(cols) >= min_len && length(remaining) > 0) {

    sub <- mat[remaining, cols, drop = FALSE]
    tvec <- seq_along(cols)


    rho <- apply(sub, 1, function(x) suppressWarnings(cor(x, tvec, method = "spearman")))
    rho[is.na(rho)] <- 0

    up_genes   <- names(rho)[rho >=  rho_cut]
    down_genes <- names(rho)[rho <= -rho_cut]

    if (length(up_genes) > 0)   group[up_genes]   <- paste0("Up", round_id)
    if (length(down_genes) > 0) group[down_genes] <- paste0("Down", round_id)


    newly_assigned <- union(up_genes, down_genes)
    remaining <- setdiff(remaining, newly_assigned)


    if (length(cols) == min_len) break

    # ---- trim one sample (head/tail/best) ----
    if (trim_side == "head") {
      cols <- cols[-1]
    } else if (trim_side == "tail") {
      cols <- cols[-length(cols)]
    } else {

      if (length(remaining) == 0) {
        cols <- cols[-1]
      } else {
        cols_head <- cols[-1]
        cols_tail <- cols[-length(cols)]

        score_side <- function(use_cols) {
          sub2 <- mat[remaining, use_cols, drop = FALSE]
          t2 <- seq_along(use_cols)
          rho2 <- apply(sub2, 1, function(x) suppressWarnings(cor(x, t2, method = "spearman")))
          rho2 <- rho2[is.finite(rho2)]
          if (length(rho2) == 0) return(0)
          median(abs(rho2), na.rm = TRUE)
        }

        s_head <- score_side(cols_head)
        s_tail <- score_side(cols_tail)

        cols <- if (s_head >= s_tail) cols_head else cols_tail
      }
    }

    round_id <- round_id + 1L
  }

  group[is.na(group)] <- "No pattern"

  out <- data.frame(
    gene = genes_all,
    gene_group = unname(group[genes_all]),
    stringsAsFactors = FALSE
  )
  out$type <- case_when(
    str_detect(out$gene_group,'Up') ~ 'Up',
    str_detect(out$gene_group,'Down') ~ 'Down',
    str_detect(out$gene_group,'No') ~ 'No pattern'
  )
  return(out)
}

#' Limma-based differential expression analysis for proteomics (unpaired)
#'
#' @param expr Expression matrix.
#' @param group Factor defining two groups to compare.
#' @param offset Offset added before log2 transformation.
#' @param contrast_str Contrast string for limma (e.g., "B-A").
#' @param trend Whether to apply trend eBayes.
#' @param robust Whether to apply robust eBayes.
#'
#' @return A data.frame containing logFC, P-values, adjusted P-values,
#' mean log2 expression per group, and delta mean.
#' @export

limma_protein_DE <- function(expr,
                             group,
                             offset = NULL,
                             contrast_str = NULL,
                             trend = TRUE,
                             robust = TRUE) {

  stopifnot(is.matrix(expr) || is.data.frame(expr))
  expr <- as.matrix(expr)

  if (is.null(colnames(expr))) colnames(expr) <- paste0("S", seq_len(ncol(expr)))

  col_sums <- colSums(expr, na.rm = TRUE)
  if (!all(is.finite(col_sums))) stop("Non-finite values detected.")

  if (!all(abs(col_sums - 1e6) <= 0.1 * 1e6)) {
    warning("Column sums deviate from 1e6. Check normalization.")
  }

  if (is.null(offset)) {
    pos_min <- suppressWarnings(min(expr[expr > 0], na.rm = TRUE))
    if (!is.finite(pos_min)) pos_min <- 1e-6
    offset <- max(pos_min / 2, 1e-6)
  }

  expr_log <- log2(expr + offset)

  if (is.character(group)) group <- factor(group)
  stopifnot(nlevels(group) == 2, length(group) == ncol(expr_log))
  group <- droplevels(group)

  design <- model.matrix(~0 + group)
  lv <- levels(group)
  lv_safe <- make.names(lv)
  colnames(design) <- lv_safe
  level_map <- stats::setNames(lv_safe, lv)

  if (is.null(contrast_str)) {
    contrast_str <- paste0(lv_safe[2], "-", lv_safe[1])
  } else {
    contrast_str <- as.character(contrast_str)
    for (orig in names(level_map)) {
      contrast_str <- gsub(
        paste0("(?<![A-Za-z0-9_.])", orig, "(?![A-Za-z0-9_.])"),
        level_map[[orig]],
        contrast_str,
        perl = TRUE
      )
    }
  }

  contrast.mat <- limma::makeContrasts(contrasts = contrast_str, levels = design)

  fit <- limma::lmFit(expr_log, design)
  fit2 <- limma::contrasts.fit(fit, contrast.mat)
  fit2 <- limma::eBayes(fit2, trend = trend, robust = robust)
  tt <- limma::topTable(fit2, number = Inf, sort.by = "P")

  grp_means <- sapply(lv, function(g) rowMeans(expr_log[, group == g, drop = FALSE], na.rm = TRUE))

  if (is.vector(grp_means)) grp_means <- cbind(grp_means)
  colnames(grp_means) <- paste0("mean_log2_", lv)

  res <- cbind(
    data.frame(feature = rownames(tt), tt, check.names = FALSE),
    grp_means[rownames(tt), , drop = FALSE]
  )

  parts <- strsplit(contrast_str, "-")[[1]]
  if (length(parts) == 2) {
    inv_level_map <- stats::setNames(names(level_map), level_map)
    parts_orig <- unname(inv_level_map[parts])
  } else {
    parts_orig <- character(0)
  }

  if (length(parts_orig) == 2 && all(parts_orig %in% lv)) {
    res$delta_mean_log2 <- res[[paste0("mean_log2_", parts_orig[1])]] -
      res[[paste0("mean_log2_", parts_orig[2])]]
  } else {
    res$delta_mean_log2 <- NA_real_
  }

  res$negLog10FDR <- -log10(res$adj.P.Val)
  rownames(res) <- NULL
  return(res)
}


#' Paired differential expression analysis using limma
#'
#' @param expr Expression matrix.
#' @param group Factor defining two comparison groups.
#' @param donor Pairing variable (e.g., donor ID).
#' @param offset Offset added before log2 transformation.
#' @param trend Apply trend eBayes.
#' @param robust Apply robust eBayes.
#' @param contrast_str Contrast specification for limma.
#'
#' @return A data.frame containing limma statistics for paired tests.
#' @export

limma_protein_DE_pair <- function(expr,
                                  group,
                                  donor = NULL,
                                  offset = NULL,
                                  trend = TRUE,
                                  robust = TRUE,
                                  contrast_str = NULL) {

  stopifnot(is.matrix(expr) || is.data.frame(expr))
  expr <- as.matrix(expr)

  if (is.null(colnames(expr))) colnames(expr) <- paste0("S", seq_len(ncol(expr)))

  if (is.character(group)) group <- factor(group)
  group <- droplevels(group)
  stopifnot(length(group) == ncol(expr), nlevels(group) == 2)
  lv <- levels(group)
  lv_safe <- make.names(lv)
  level_map <- stats::setNames(lv_safe, lv)

  if (is.null(offset)) {
    pos_min <- suppressWarnings(min(expr[expr > 0], na.rm = TRUE))
    if (!is.finite(pos_min)) pos_min <- 1e-6
    offset <- max(pos_min / 2, 1e-6)
  }

  expr_log <- log2(expr + offset)

  if (!is.null(donor)) {
    if (is.character(donor)) donor <- factor(donor)
    stopifnot(length(donor) == ncol(expr))
    design <- model.matrix(~ donor + group)
  } else {
    design <- model.matrix(~0 + group)
    colnames(design) <- lv_safe
  }

  if (is.null(contrast_str)) {
    if (!is.null(donor)) {
      group_col <- grep("^group", colnames(design), value = TRUE)
      if (length(group_col) != 1) stop("Cannot determine unique group column.")
      contrast_str <- group_col
    } else {
      contrast_str <- paste0(lv_safe[2], "-", lv_safe[1])
    }
  } else {
    contrast_str <- as.character(contrast_str)
    if (is.null(donor)) {
      for (orig in names(level_map)) {
        contrast_str <- gsub(
          paste0("(?<![A-Za-z0-9_.])", orig, "(?![A-Za-z0-9_.])"),
          level_map[[orig]],
          contrast_str,
          perl = TRUE
        )
      }
    }
  }

  contrast.mat <- limma::makeContrasts(contrasts = contrast_str, levels = design)

  fit <- limma::lmFit(expr_log, design)
  fit2 <- limma::contrasts.fit(fit, contrast.mat)
  fit2 <- limma::eBayes(fit2, trend = trend, robust = robust)
  tt <- limma::topTable(fit2, number = Inf, sort.by = "P")

  grp_means <- sapply(lv, function(g) {
    rowMeans(expr_log[, group == g, drop = FALSE], na.rm = TRUE)
  })
  colnames(grp_means) <- paste0("mean_log2_", lv)

  parts <- strsplit(contrast_str, "-")[[1]]

  if (length(parts) == 2) {
    inv_level_map <- stats::setNames(names(level_map), level_map)
    parts_orig <- unname(inv_level_map[parts])
  } else {
    parts_orig <- character(0)
  }

  if (length(parts_orig) == 2 && all(parts_orig %in% lv)) {
    delta <- grp_means[, paste0("mean_log2_", parts_orig[2])] -
      grp_means[, paste0("mean_log2_", parts_orig[1])]
  } else if (length(parts) == 1 && grepl("^group", parts)) {
    delta <- grp_means[, paste0("mean_log2_", lv[2])] -
      grp_means[, paste0("mean_log2_", lv[1])]
  } else {
    delta <- NA_real_
  }

  res <- cbind(
    data.frame(feature = rownames(tt), tt, check.names = FALSE),
    grp_means[rownames(tt), , drop = FALSE]
  )

  res$delta_mean_log2 <- delta[rownames(tt)]
  res$negLog10FDR <- -log10(pmax(res$adj.P.Val, 1e-300))

  rownames(res) <- NULL
  return(res)
}


#' PCA-based clustering using DynamicTreeCut
#'
#' @description
#' Perform PCA-based feature or sample clustering using DynamicTreeCut.
#'
#' @param mtx Numeric matrix. Rows or columns will be clustered.
#' @param mode "row" or "col". Determines clustering direction.
#' @param n_pcs Number of principal components to retain for distance calculation.
#' @param method_hclust Agglomeration method for hierarchical clustering.
#' @param deepSplit DynamicTreeCut parameter controlling cluster granularity.
#' @param minClusterSize Minimum cluster size in DynamicTreeCut.
#' @param pamStage Whether to apply PAM stage refinement in DynamicTreeCut.
#'
#' @return A data.frame containing `protein_group` and assigned cluster label.
#' @export
auto_cluster_matrix_pca_one <- function(
    mtx,
    mode = c("row", "col"),
    n_pcs = 20,
    method_hclust = "ward.D2",
    deepSplit = 0,
    minClusterSize = 10,
    pamStage = TRUE
) {
  # check namespace
  if (!requireNamespace("dynamicTreeCut", quietly = TRUE)) {
    stop("Package 'dynamicTreeCut' is required but not installed.")
  }

  mode <- match.arg(mode)

  if (mode == "row") {
    pca_res <- stats::prcomp(mtx, center = FALSE, scale. = FALSE, rank. = n_pcs)
    d <- stats::dist(pca_res$x)
    hc <- stats::hclust(d, method = method_hclust)
    cl <- dynamicTreeCut::cutreeDynamic(
      dendro = hc,
      distM  = as.matrix(d),
      method = "hybrid",
      deepSplit = deepSplit,
      minClusterSize = minClusterSize,
      pamStage = pamStage
    )
    df <- data.frame(gene = rownames(mtx), km_cluster = paste0("km", cl))
  } else {
    d <- stats::as.dist(1 - stats::cor(mtx, method = "spearman"))
    hc <- stats::hclust(d, method = method_hclust)
    cl <- dynamicTreeCut::cutreeDynamic(
      dendro = hc,
      distM  = as.matrix(d),
      method = "hybrid",
      deepSplit = deepSplit,
      minClusterSize = minClusterSize,
      pamStage = pamStage
    )
    df <- data.frame(gene = colnames(mtx), km_cluster = paste0("km", cl))
  }

  return(df)
}

#' Functional enrichment analysis (GO / KEGG)
#'
#' This function performs GO or KEGG enrichment analysis using
#' \pkg{clusterProfiler}. Dependencies are loaded dynamically via
#' \code{requireNamespace()}, so the function will only require
#' clusterProfiler and annotation packages when executed.
#'
#' @param gene A character vector of gene symbols.
#' @param db Database to use: \code{"GO"} or \code{"KEGG"}.
#' @param species Species: \code{"human"} or \code{"mouse"}.
#' @param keyType Gene ID type used in \code{gene}, e.g. \code{"SYMBOL"}.
#' @param qvalue_cutoff Q-value threshold for filtering.
#' @param use_internal_data whether using internal KEGG data
#' @param return_data Logical. If \code{TRUE}, return \code{enriched@result};
#'   otherwise return the full enrichment result object.
#' @return A data frame or enrichment result object.
#'
#' @details
#' KEGG analysis automatically converts gene IDs to ENTREZID using
#' \code{clusterProfiler::bitr()}. Human and mouse annotation packages
#' (\code{org.Hs.eg.db} and \code{org.Mm.eg.db}) are used depending on
#' species.
#'
#' @examples
#' \dontrun{
#' enrichment_analysis(c("TP53","MDM2"), db="GO", species="human")
#' }
#'
#' @export
enrichment_analysis <- function(
    gene,
    db            = c("GO", "KEGG"),
    species       = c("human", "mouse"),
    keyType       = "SYMBOL",
    qvalue_cutoff = 0.05,
  use_internal_data = TRUE,
  return_data = TRUE
){
  ## -------------------------------
  ## Argument processing
  ## -------------------------------
  db      <- match.arg(db)
  species <- match.arg(species)

  gene <- unique(trimws(as.character(gene)))
  gene <- gsub("[^[:alnum:]_]", "", gene)
  gene <- gene[gene != ""]
  if (length(gene) == 0) stop("No valid gene symbols provided.")

  ## -------------------------------
  ## Check clusterProfiler
  ## -------------------------------
  if (!requireNamespace("clusterProfiler", quietly = TRUE)) {
    stop("Package 'clusterProfiler' must be installed for enrichment_analysis().")
  }

  ## -------------------------------
  ## Select species OrgDb
  ## -------------------------------
  if (species == "human") {
    if (!requireNamespace("org.Hs.eg.db", quietly = TRUE)) {
      stop("Package 'org.Hs.eg.db' must be installed for human enrichment analysis.")
    }
    OrgDb <- org.Hs.eg.db::org.Hs.eg.db
    kegg_org <- "hsa"
  } else {
    if (!requireNamespace("org.Mm.eg.db", quietly = TRUE)) {
      stop("Package 'org.Mm.eg.db' must be installed for mouse enrichment analysis.")
    }
    OrgDb <- org.Mm.eg.db::org.Mm.eg.db
    kegg_org <- "mmu"
  }

  ## -------------------------------
  ## Perform Enrichment
  ## -------------------------------
  if (db == "GO") {
    enriched <- clusterProfiler::enrichGO(
      gene          = gene,
      OrgDb         = OrgDb,
      keyType       = keyType,
      ont           = "ALL",
      pAdjustMethod = "BH",
      pvalueCutoff  = 0.05,
      qvalueCutoff  = qvalue_cutoff
    )

  } else {  # KEGG
    kegg_db_package <- paste0("KEGG", ".", "db")
    if (isTRUE(use_internal_data) && !requireNamespace(kegg_db_package, quietly = TRUE)) {
      kegg_pkg <- system.file(
        "shiny", "EasyProtein", "www", "KEGG.db_1.0.tar.gz",
        package = "EasyProtein"
      )
      if (identical(kegg_pkg, "")) {
        kegg_pkg <- file.path("inst", "shiny", "EasyProtein", "www", "KEGG.db_1.0.tar.gz")
      }

      if (!file.exists(kegg_pkg)) {
        stop(
          "use_internal_data=TRUE requires KEGG.db, but package file not found: ",
          kegg_pkg
        )
      }

      utils::install.packages(kegg_pkg, repos = NULL, type = "source")

      if (!requireNamespace(kegg_db_package, quietly = TRUE)) {
        stop("Failed to install/load package 'KEGG.db'.")
      }
    }

    gene_df <- clusterProfiler::bitr(
      gene,
      fromType = keyType,
      toType   = "ENTREZID",
      OrgDb    = OrgDb
    )

    enriched <- clusterProfiler::enrichKEGG(
      gene          = gene_df$ENTREZID,
      organism      = kegg_org,
      pAdjustMethod = "BH",
      pvalueCutoff  = 0.05,
      qvalueCutoff  = qvalue_cutoff,
      use_internal_data = use_internal_data
    )

    enriched <- clusterProfiler::setReadable(
      enriched,
      OrgDb   = OrgDb,
      keyType = "ENTREZID"
    )
  }

  if (isTRUE(return_data)) {
    return(enriched@result)
  }
  return(enriched)
}
#' Run GSEA (Gene Set Enrichment Analysis)
#'
#' Performs GO or KEGG GSEA using ranked gene statistics. This function
#' dynamically loads clusterProfiler and annotation packages only when
#' needed, making them optional dependencies.
#'
#' @param df A data frame containing ranking statistics and gene IDs.
#' @param gene_col Column storing gene IDs.
#' @param stat_col Column storing ranking values (e.g. log2FC).
#' @param db Database for GSEA: \code{"GO"} or \code{"KEGG"}.
#' @param ont GO ontology ("BP","CC","MF","ALL").
#' @param species Species: \code{"human"} or \code{"mouse"}.
#' @param keyType Gene ID type (default SYMBOL).
#' @param qvalue_cutoff Q-value filter.
#' @param minGSSize Minimum gene set size.
#' @param maxGSSize Maximum gene set size.
#' @param pAdjustMethod Multiple testing correction.
#' @param eps GSEA numeric convergence threshold.
#' @param use_internal_data whether using internal KEGG data
#' @param seed Random seed.
#'
#' @return A \code{gseaResult} object.
#'
#' @export
run_gsea <- function(
    df,
    gene_col      = "gene",
    stat_col      = "log2FC",
    db            = c("GO", "KEGG"),
    ont           = "ALL",
    species       = c("human", "mouse"),
    keyType       = "SYMBOL",
    qvalue_cutoff = 0.05,
    minGSSize     = 10,
    maxGSSize     = 5000,
    pAdjustMethod = "BH",
    eps           = 1e-10,
    use_internal_data = FALSE,
    seed          = 123
){
  ## ---------------------
  ## Check arguments
  ## ---------------------
  stopifnot(all(c(gene_col, stat_col) %in% colnames(df)))
  db      <- match.arg(db)
  species <- match.arg(species)

  ## ---------------------
  ## Check dependency
  ## ---------------------
  if (!requireNamespace("clusterProfiler", quietly = TRUE)) {
    stop("Package 'clusterProfiler' must be installed for run_gsea().")
  }

  ## ---------------------
  ## Select OrgDb
  ## ---------------------
  if (species == "human") {
    if (!requireNamespace("org.Hs.eg.db", quietly = TRUE))
      stop("Package 'org.Hs.eg.db' must be installed.")
    OrgDb <- org.Hs.eg.db::org.Hs.eg.db
    kegg_org <- "hsa"
  } else {
    if (!requireNamespace("org.Mm.eg.db", quietly = TRUE))
      stop("Package 'org.Mm.eg.db' must be installed.")
    OrgDb <- org.Mm.eg.db::org.Mm.eg.db
    kegg_org <- "mmu"
  }

  ## ---------------------
  ## Clean + rank gene list
  ## ---------------------
  df[[gene_col]] <- gsub("[^[:alnum:]_]", "", df[[gene_col]])

  x <- df |>
    dplyr::transmute(g = as.character(.data[[gene_col]]),
                     s = as.numeric(.data[[stat_col]])) |>
    dplyr::filter(!is.na(g), g != "", !is.na(s)) |>
    dplyr::group_by(g) |>
    dplyr::slice_max(order_by = abs(s), n = 1, with_ties = FALSE) |>
    dplyr::ungroup()

  geneList <- x$s
  names(geneList) <- x$g
  geneList <- sort(geneList, decreasing = TRUE)

  ## ---------------------
  ## Run GO GSEA
  ## ---------------------
  if (db == "GO") {
    res <- clusterProfiler::gseGO(
      geneList      = geneList,
      OrgDb         = OrgDb,
      keyType       = keyType,
      ont           = ont,
      minGSSize     = minGSSize,
      maxGSSize     = maxGSSize,
      pAdjustMethod = pAdjustMethod,
      pvalueCutoff  = 1,  # filter later
      eps           = eps,
      verbose       = FALSE
    )
    return(res)
  }

  ## ---------------------
  ## Run KEGG GSEA
  ## ---------------------
  if (keyType != "ENTREZID") {
    conv <- tryCatch(
      clusterProfiler::bitr(names(geneList),
                            fromType = keyType,
                            toType   = "ENTREZID",
                            OrgDb    = OrgDb),
      error = function(e) NULL
    )
    if (is.null(conv) || !nrow(conv))
      stop("ID conversion to ENTREZID failed.")

    gl_entrez <- tibble::tibble(ID = names(geneList), s = geneList) |>
      dplyr::inner_join(conv, by = c("ID" = keyType)) |>
      dplyr::group_by(ENTREZID) |>
      dplyr::summarize(s = s[which.max(abs(s))], .groups = "drop") |>
      (\(df) { stats::setNames(df$s, df$ENTREZID) })() |>
      sort(decreasing = TRUE)

  } else {
    gl_entrez <- geneList
  }

  res <- clusterProfiler::gseKEGG(
    geneList      = gl_entrez,
    organism      = kegg_org,
    minGSSize     = minGSSize,
    maxGSSize     = maxGSSize,
    pAdjustMethod = pAdjustMethod,
    pvalueCutoff  = 1,
    eps           = eps,
    use_internal_data = use_internal_data,
    verbose       = FALSE
  )

  return(res)
}

#' Extract positive and negative contributors for a given PCA component
#'
#' This function extracts the top contributing features (e.g. proteins/genes)
#' for a specified principal component (PC) from a \code{FactoMineR::PCA} result.
#' It supports both signed loadings (coordinates) for direction-aware interpretation
#' and contribution values for importance ranking, and can optionally merge
#' feature annotations from a \code{SummarizedExperiment} object.
#'
#' @param pca.res A \code{PCA} object returned by \code{FactoMineR::PCA}.
#'
#' @param se_obj A \code{SummarizedExperiment} object. If provided, row annotations
#'   (from \code{rowData(se_obj)}) will be joined to PCA variables using
#'   \code{Protein.Ids}. Default is \code{NULL}.
#'
#' @param pc Integer. The principal component to extract (e.g. \code{1} for PC1).
#'   Default is \code{1}.
#'
#' @param n Integer. Number of top features to return for each direction
#'   (positive and negative). Default is \code{20}.
#'
#' @param use Character. Which PCA metric to use:
#'   \describe{
#'     \item{\code{"coord"}}{Use signed coordinates (loadings). This preserves
#'       directionality and is suitable for identifying positive vs negative
#'       contributors along the PC.}
#'     \item{\code{"contrib"}}{Use contribution values. This ranks features by
#'       importance to the PC but does not encode direction.}
#'   }
#'   Default is \code{"coord"}.
#'
#' @return A list with the following components:
#'   \describe{
#'     \item{PC}{Character string indicating the selected PC (e.g. \code{"Dim.1"}).}
#'     \item{metric}{Which metric was used: \code{"coord"} or \code{"contrib"}.}
#'     \item{positive}{A data frame of the top \code{n} positively contributing
#'       features (only for \code{"coord"}).}
#'     \item{negative}{A data frame of the top \code{n} negatively contributing
#'       features (only for \code{"coord"}).}
#'   }
#'
#' @details
#' The function operates on \code{pca.res$var[[use]]}, where:
#' \itemize{
#'   \item \code{coord} represents the signed loadings (projection of each feature
#'     onto the PC axis), allowing interpretation of positive vs negative directions.
#'   \item \code{contrib} represents the percentage contribution of each feature to
#'     the variance of the PC, without directional information.
#' }
#'
#' When \code{se_obj} is provided, feature-level annotations are merged from
#' \code{rowData(se_obj)} by \code{Protein.Ids}. Gene symbols are extracted from
#' the \code{Genes} column (taking the first symbol before a semicolon).
#'
#' If multiple protein entries map to the same gene, consider collapsing results
#' at the gene level downstream.
#'
#' @examples
#' \dontrun{
#' library(FactoMineR)
#'
#' # Run PCA
#' pca.res <- PCA(t(expr_matrix), scale.unit = TRUE, graph = FALSE)
#'
#' # Extract top contributors on PC1 using signed coordinates
#' res_pc1 <- get_pc_contributors(pca.res, pc = 1, n = 30, use = "coord")
#'
#' res_pc1$positive
#' res_pc1$negative
#'
#' # Using contribution values instead of signed loadings
#' res_pc1_contrib <- get_pc_contributors(pca.res, pc = 1, n = 30, use = "contrib")
#' }
#'
#' @importFrom dplyr %>% left_join mutate filter arrange slice_head group_by
#' @importFrom stringr str_extract
#' @export


get_pc_contributors <- function(pca.res,se_obj= NULL, pc = 1, n = 20, use = c("coord", "contrib")) {

  use <- match.arg(use)

  mat <- pca.res$var[[use]]

  pc_name <- paste0("Dim.", pc)
  if (!pc_name %in% colnames(mat)) {
    stop("PC no exist:", pc_name)
  }

  df <- data.frame(
    gene = rownames(mat),
    value = mat[, pc_name]
  )
  if (!is.null(se_obj)) {
    df <- df %>% left_join(as.data.frame(rowData(se_obj)), by = 'gene')
  }


  df <- df[!is.na(df$value), ]


  pos <- df %>%
    dplyr::arrange(dplyr::desc(value)) %>%
    dplyr::filter(value >0) %>%
    dplyr::slice_head(n = n) %>%
    mutate(type = 'Positive')


  neg <- df %>%
    dplyr::arrange(value) %>%
    dplyr::filter(value <0) %>%
    dplyr::slice_head(n = n) %>%
    mutate(type = 'Negative')

  return(list(
    PC = pc_name,
    metric = use,
    positive = pos,
    negative = neg
  ))
}



#' Hybrid temporal signature clustering for multi-series time-course proteomics
#'
#' @description
#' Cluster proteins by coordinated temporal response patterns across one or
#' multiple biological time series. The method first estimates log2 fold change
#' against the baseline time point within each series, gates non-significant
#' comparisons to zero, and clusters the remaining dynamic proteins with a
#' hybrid distance that combines response shape, amplitude, temporal trend and
#' significant-response timing.
#'
#' Proteins with no statistically supported post-baseline change are not forced
#' into dynamic clusters. They are returned in the background group
#' \code{Flat_or_not_significant}.
#'
#' @param x A \code{SummarizedExperiment} object, or a named list of
#'   gene-by-time matrices. For a \code{SummarizedExperiment}, samples are in
#'   columns and features/proteins are in rows.
#' @param assay_name Character. Assay used when \code{x} is a
#'   \code{SummarizedExperiment}. Default is \code{"conc"}.
#' @param time_col Character. Column in \code{colData(x)} containing ordered
#'   time points. Default is \code{"time"}.
#' @param series_col Character or \code{NULL}. Column in \code{colData(x)}
#'   containing the biological series, such as cell line. If \code{NULL}, all
#'   samples are treated as one series.
#' @param baseline Baseline time point. If \code{NULL}, the first ordered time
#'   point in each series is used.
#' @param k Integer number of clusters or \code{"auto"}. When \code{"auto"},
#'   the cluster number is selected by average silhouette width.
#' @param k_range Integer vector of candidate cluster numbers used when
#'   \code{k = "auto"}.
#' @param min_mean Numeric. Minimum mean raw abundance for feature filtering.
#' @param min_sd Numeric. Minimum standard deviation of the baseline-referenced
#'   trajectory for feature filtering.
#' @param log2_offset Numeric or \code{NULL}. Offset added before log2
#'   transformation. If \code{NULL}, half of the minimum positive abundance is
#'   used.
#' @param p_adj_cutoff Numeric adjusted P-value cutoff used to define
#'   significant time points.
#' @param logFC_cutoff Numeric absolute log2FC cutoff used together with
#'   \code{p_adj_cutoff}.
#' @param min_cluster_prop Numeric. Cluster-level fraction threshold used to
#'   call a cluster response time.
#' @param min_significant_timepoints Integer. Minimum number of significant
#'   post-baseline time points required for a protein to enter dynamic
#'   clustering. Default is \code{1}.
#' @param distance_weights Named numeric vector with entries \code{shape},
#'   \code{amplitude}, \code{trend} and \code{timing}. Values are normalized to
#'   sum to one.
#' @param max_k_features Integer. Maximum number of proteins used to estimate
#'   the silhouette score when selecting \code{k}. The final clustering still
#'   uses all retained dynamic proteins.
#' @param seed Integer random seed.
#' @param scale_for_clustering Logical. If TRUE, scale the significance-gated matrix before clustering. Default is FALSE.
#' @param distance Character. Distance used by the optional kmeans workflow; one of \code{"correlation"} or \code{"euclidean"}.
#' @param common_features Logical. If TRUE, retain only features observed in all input series.
#' @param clustering_method Character. Clustering backend. Default \code{"hybrid_hclust"} uses the hybrid temporal distance; \code{"kmeans"} is retained for compatibility.
#'
#' @return A list with class \code{EasyProteinTimecoursePattern}. Important
#'   elements include:
#' \describe{
#'   \item{\code{gene_cluster}}{Dynamic proteins and their assigned pattern
#'   clusters.}
#'   \item{\code{gene_cluster_all}}{All proteins, including background proteins
#'   labeled \code{Flat_or_not_significant}.}
#'   \item{\code{background_genes}}{Proteins without enough significant
#'   time-point changes for dynamic clustering.}
#'   \item{\code{gene_change}}{Gene-level first significant and strongest
#'   response times.}
#'   \item{\code{cluster_change}}{Cluster-level significant response times.}
#'   \item{\code{cluster_summary}}{Cluster sizes, centroid pattern labels and
#'   activity summaries.}
#'   \item{\code{cluster_input}}{The significance-gated log2FC matrix used for
#'   clustering.}
#'   \item{\code{timepoint_tests}}{limma time-point test results.}
#' }
#'
#' @details
#' For each protein \eqn{g}, series \eqn{s}, and time point \eqn{t}, the method
#' estimates a baseline-referenced log2 fold change \eqn{L_{g,s,t}}. The
#' clustering matrix is
#'
#' \deqn{
#' Z_{g,s,t} =
#' \begin{cases}
#' L_{g,s,t}, & q_{g,s,t}\leq \alpha \text{ and } |L_{g,s,t}|\geq \delta,\\
#' 0, & \text{otherwise}.
#' \end{cases}
#' }
#'
#' Dynamic proteins are clustered using a hybrid distance:
#'
#' \deqn{
#' D = w_{shape}D_{shape} + w_{amp}D_{amp} +
#'     w_{trend}D_{trend} + w_{time}D_{time}.
#' }
#'
#' This makes the clustering sensitive to both response magnitude and temporal
#' trajectory, while treating non-significant comparisons as explicit
#' no-change observations.
#'
#' @examples
#' \dontrun{
#' res <- temporalSignatureClustering(
#'   se,
#'   assay_name = "conc",
#'   time_col = "time",
#'   series_col = "cell_line",
#'   baseline = 0,
#'   k = "auto",
#'   k_range = 4:10
#' )
#'
#' head(res$gene_cluster)
#' res$cluster_summary
#' }
#'
#' @export

temporalSignatureClustering <- function(
    x,
    assay_name = "conc",
    time_col = "time",
    series_col = NULL,
    baseline = NULL,
    k = "auto",
    k_range = 2:10,
    min_mean = 0,
    min_sd = 0.15,
    log2_offset = NULL,
    scale_for_clustering = FALSE,
    distance = c("correlation", "euclidean"),
    p_adj_cutoff = 0.05,
    logFC_cutoff = 0.5,
    min_cluster_prop = 0.2,
    common_features = TRUE,
    max_k_features = 2000,
    min_significant_timepoints = 1,
    clustering_method = c("hybrid_hclust", "kmeans"),
    distance_weights = c(shape = 0.35, amplitude = 0.25, trend = 0.25, timing = 0.15),
    seed = 1
) {
  distance <- match.arg(distance)
  clustering_method <- match.arg(clustering_method)
  set.seed(seed)

  prepared <- prepare_timecourse_matrix(
    x = x,
    assay_name = assay_name,
    time_col = time_col,
    series_col = series_col,
    baseline = baseline,
    log2_offset = log2_offset,
    common_features = common_features
  )

  trajectory <- prepared$trajectory
  raw_mean <- prepared$raw_mean
  feature_mean <- rowMeans(raw_mean, na.rm = TRUE)
  feature_sd <- apply(trajectory, 1, stats::sd, na.rm = TRUE)
  keep <- is.finite(feature_mean) & is.finite(feature_sd) &
    feature_mean >= min_mean & feature_sd >= min_sd

  if (sum(keep) < 3) {
    stop("Fewer than three features passed filtering; lower min_mean/min_sd.")
  }

  trajectory <- trajectory[keep, , drop = FALSE]

  test_res <- test_timecourse_changes(
    x = x,
    assay_name = assay_name,
    time_col = time_col,
    series_col = series_col,
    baseline = baseline,
    log2_offset = prepared$log2_offset,
    p_adj_cutoff = p_adj_cutoff,
    logFC_cutoff = logFC_cutoff
  )

  significant_logfc <- build_significant_logfc_matrix(
    test_res = test_res,
    genes = rownames(trajectory),
    metadata = prepared$metadata,
    p_adj_cutoff = p_adj_cutoff,
    logFC_cutoff = logFC_cutoff
  )

  cluster_input <- significant_logfc$matrix

  dynamic_keep <- rowSums(cluster_input != 0, na.rm = TRUE) >= min_significant_timepoints
  background_genes <- tibble::tibble(
    gene = rownames(cluster_input)[!dynamic_keep],
    cluster = "Flat_or_not_significant",
    pattern = "Flat_or_not_significant",
    pattern_signature = "No significant log2FC across all series"
  )

  if (sum(dynamic_keep) < 3) {
    stop("Fewer than three genes have enough significant time points for clustering.")
  }

  cluster_input <- cluster_input[dynamic_keep, , drop = FALSE]
  trajectory <- trajectory[rownames(cluster_input), , drop = FALSE]

  if (isTRUE(scale_for_clustering)) {
    cluster_input <- scale(cluster_input)
    cluster_input[!is.finite(cluster_input)] <- 0
  }

  if (clustering_method == "hybrid_hclust") {
    cluster_fit <- run_hybrid_temporal_clustering(
      mat = cluster_input,
      metadata = significant_logfc$metadata,
      k = k,
      k_range = k_range,
      weights = distance_weights,
      max_features = max_k_features,
      seed = seed
    )
    selected_k <- cluster_fit$selected_k
    gene_cluster <- tibble::tibble(
      gene = names(cluster_fit$cluster),
      cluster = paste0("P", as.integer(cluster_fit$cluster))
    )
  } else {
    selected_k <- select_timecourse_k(
      cluster_input,
      k = k,
      k_range = k_range,
      distance = distance,
      max_features = max_k_features,
      seed = seed
    )

    km <- stats::kmeans(cluster_input, centers = selected_k$k, nstart = 50, iter.max = 100)
    gene_cluster <- tibble::tibble(
      gene = rownames(cluster_input),
      cluster = paste0("P", km$cluster)
    )
  }

  centroids <- summarize_timecourse_clusters(
    trajectory = cluster_input,
    gene_cluster = gene_cluster,
    feature_info = significant_logfc$metadata
  )

  cluster_activity <- tibble::tibble(
    gene = rownames(cluster_input),
    n_significant_timepoints = rowSums(cluster_input != 0, na.rm = TRUE),
    mean_abs_sig_logFC = rowMeans(abs(cluster_input), na.rm = TRUE)
  ) %>%
    dplyr::left_join(gene_cluster, by = "gene") %>%
    dplyr::group_by(.data$cluster) %>%
    dplyr::summarise(
      median_significant_timepoints = stats::median(.data$n_significant_timepoints),
      mean_abs_sig_logFC = mean(.data$mean_abs_sig_logFC, na.rm = TRUE),
      .groups = "drop"
    )

  centroids$cluster_summary <- centroids$cluster_summary %>%
    dplyr::left_join(cluster_activity, by = "cluster")

  gene_cluster <- gene_cluster %>%
    dplyr::left_join(
      centroids$cluster_summary[, c("cluster", "pattern", "pattern_signature")],
      by = "cluster"
    )

  gene_cluster_all <- dplyr::bind_rows(gene_cluster, background_genes)

  if (!is.null(test_res) && nrow(test_res) > 0) {
    gene_change <- test_res %>%
      dplyr::filter(.data$gene %in% gene_cluster$gene) %>%
      dplyr::group_by(.data$gene, .data$series) %>%
      dplyr::arrange(.data$time, .by_group = TRUE) %>%
      dplyr::summarise(
        first_significant_time = {
          idx <- which(.data$significant)
          if (length(idx) == 0) NA_character_ else as.character(.data$time[idx[1]])
        },
        strongest_time = as.character(.data$time[which.max(abs(.data$logFC))]),
        max_abs_logFC = max(abs(.data$logFC), na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::left_join(gene_cluster, by = "gene")

    cluster_change <- test_res %>%
      dplyr::inner_join(gene_cluster, by = "gene") %>%
      dplyr::group_by(.data$cluster, .data$pattern, .data$series, .data$time) %>%
      dplyr::summarise(
        n_genes = dplyr::n(),
        n_significant = sum(.data$significant, na.rm = TRUE),
        significant_fraction = .data$n_significant / .data$n_genes,
        median_logFC = stats::median(.data$logFC, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::group_by(.data$cluster, .data$pattern, .data$series) %>%
      dplyr::arrange(.data$time, .by_group = TRUE) %>%
      dplyr::mutate(
        cluster_significant_time = dplyr::if_else(
          .data$significant_fraction >= min_cluster_prop,
          as.character(.data$time),
          NA_character_
        )
      ) %>%
      dplyr::summarise(
        first_cluster_significant_time = {
          tt <- stats::na.omit(.data$cluster_significant_time)
          if (length(tt) == 0) NA_character_ else tt[1]
        },
        max_significant_fraction = max(.data$significant_fraction, na.rm = TRUE),
        strongest_cluster_time = as.character(.data$time[which.max(abs(.data$median_logFC))]),
        .groups = "drop"
      )
  } else {
    gene_change <- NULL
    cluster_change <- NULL
  }

  out <- list(
    gene_cluster = gene_cluster,
    gene_cluster_all = gene_cluster_all,
    background_genes = background_genes,
    gene_change = gene_change,
    cluster_change = cluster_change,
    timepoint_tests = test_res,
    cluster_summary = centroids$cluster_summary,
    centroids = centroids$centroids,
    trajectory = trajectory,
    cluster_input = cluster_input,
    selected_k = selected_k,
    metadata = significant_logfc$metadata,
    expression_metadata = prepared$metadata,
    parameters = list(
      assay_name = assay_name,
      time_col = time_col,
      series_col = series_col,
      baseline = baseline,
      min_mean = min_mean,
      min_sd = min_sd,
      clustering_matrix = "significant_log2FC_vs_baseline",
      clustering_method = clustering_method,
      distance_weights = distance_weights,
      nonsignificant_value = 0,
      p_adj_cutoff = p_adj_cutoff,
      logFC_cutoff = logFC_cutoff
      ,
      min_significant_timepoints = min_significant_timepoints
    )
  )
  class(out) <- c("EasyProteinTimecoursePattern", class(out))
  out
}

prepare_timecourse_matrix <- function(
    x,
    assay_name = "conc",
    time_col = "time",
    series_col = NULL,
    baseline = NULL,
    log2_offset = NULL,
    common_features = TRUE
) {
  if (inherits(x, "SummarizedExperiment")) {
    stopifnot(assay_name %in% SummarizedExperiment::assayNames(x))
    stopifnot(time_col %in% colnames(SummarizedExperiment::colData(x)))

    mat_raw <- as.matrix(SummarizedExperiment::assay(x, assay_name))
    if (is.null(log2_offset)) log2_offset <- infer_log2_offset(mat_raw)
    mat_log <- log2(mat_raw + log2_offset)

    meta <- as.data.frame(SummarizedExperiment::colData(x))
    meta$.__time__ <- meta[[time_col]]
    meta$.__series__ <- if (is.null(series_col)) "Series1" else meta[[series_col]]
    series_levels <- unique(as.character(meta$.__series__))

    series_mats <- lapply(series_levels, function(ss) {
      idx_series <- which(as.character(meta$.__series__) == ss)
      time_levels <- sort_unique_time(meta$.__time__[idx_series])
      base <- choose_baseline(time_levels, baseline)
      mean_mat <- sapply(time_levels, function(tt) {
        idx <- idx_series[as.character(meta$.__time__[idx_series]) == as.character(tt)]
        rowMeans(mat_log[, idx, drop = FALSE], na.rm = TRUE)
      })
      mean_mat <- as.matrix(mean_mat)
      rownames(mean_mat) <- rownames(mat_log)
      colnames(mean_mat) <- paste0(ss, "|", time_levels)
      sweep(mean_mat, 1, mean_mat[, paste0(ss, "|", base)], "-")
    })
    names(series_mats) <- series_levels

    raw_mean <- lapply(series_levels, function(ss) {
      idx_series <- which(as.character(meta$.__series__) == ss)
      time_levels <- sort_unique_time(meta$.__time__[idx_series])
      mean_mat <- sapply(time_levels, function(tt) {
        idx <- idx_series[as.character(meta$.__time__[idx_series]) == as.character(tt)]
        rowMeans(mat_raw[, idx, drop = FALSE], na.rm = TRUE)
      })
      colnames(mean_mat) <- paste0(ss, "|", time_levels)
      mean_mat
    })
    names(raw_mean) <- series_levels
  } else {
    as_mat <- function(z) {
      z <- as.matrix(z)
      if (is.null(rownames(z))) stop("Matrix-like input must have rownames.")
      z
    }
    series_mats_input <- if (is.list(x) && !is.data.frame(x)) x else list(Series1 = x)
    if (is.null(names(series_mats_input))) {
      names(series_mats_input) <- paste0("Series", seq_along(series_mats_input))
    }

    raw_mean <- lapply(series_mats_input, as_mat)
    if (is.null(log2_offset)) log2_offset <- infer_log2_offset(unlist(raw_mean, use.names = FALSE))

    series_mats <- lapply(names(raw_mean), function(ss) {
      mat_raw <- raw_mean[[ss]]
      time_levels <- colnames(mat_raw)
      if (is.null(time_levels)) time_levels <- seq_len(ncol(mat_raw))
      base <- choose_baseline(time_levels, baseline)
      mat_log <- log2(mat_raw + log2_offset)
      colnames(mat_log) <- paste0(ss, "|", time_levels)
      sweep(mat_log, 1, mat_log[, paste0(ss, "|", base)], "-")
    })
    names(series_mats) <- names(raw_mean)
    raw_mean <- lapply(names(raw_mean), function(ss) {
      mat <- raw_mean[[ss]]
      colnames(mat) <- paste0(ss, "|", colnames(mat))
      mat
    })
    names(raw_mean) <- names(series_mats)
  }

  feature_sets <- lapply(series_mats, rownames)
  features <- if (isTRUE(common_features)) Reduce(intersect, feature_sets) else Reduce(union, feature_sets)

  align_rows <- function(mat, features) {
    out <- matrix(NA_real_, nrow = length(features), ncol = ncol(mat))
    rownames(out) <- features
    colnames(out) <- colnames(mat)
    hit <- intersect(features, rownames(mat))
    out[hit, ] <- mat[hit, , drop = FALSE]
    out
  }

  trajectory <- do.call(cbind, lapply(series_mats, align_rows, features = features))
  raw_mean_mtx <- do.call(cbind, lapply(raw_mean, align_rows, features = features))
  trajectory[!is.finite(trajectory)] <- NA_real_

  feature_info <- tibble::tibble(
    feature = colnames(trajectory),
    series = sub("\\|.*$", "", colnames(trajectory)),
    time = sub("^.*\\|", "", colnames(trajectory))
  )

  list(
    trajectory = trajectory,
    raw_mean = raw_mean_mtx,
    feature_info = feature_info,
    metadata = feature_info,
    log2_offset = log2_offset
  )
}

build_significant_logfc_matrix <- function(
    test_res,
    genes,
    metadata,
    p_adj_cutoff = 0.05,
    logFC_cutoff = 0.5
) {
  if (is.null(test_res) || nrow(test_res) == 0) {
    stop("timepoint test results are required to build the significant log2FC matrix.")
  }

  metadata <- metadata %>%
    dplyr::mutate(
      time_num = suppressWarnings(as.numeric(.data$time)),
      time_order = ifelse(is.na(time_num), match(.data$time, unique(.data$time)), time_num)
    ) %>%
    dplyr::arrange(.data$series, .data$time_order)

  mat <- matrix(
    0,
    nrow = length(genes),
    ncol = nrow(metadata),
    dimnames = list(genes, metadata$feature)
  )

  test_res <- test_res %>%
    dplyr::filter(.data$gene %in% genes) %>%
    dplyr::mutate(
      feature = paste0(.data$series, "|", .data$time),
      significant_for_clustering =
        .data$adj.P.Val <= p_adj_cutoff & abs(.data$logFC) >= logFC_cutoff,
      cluster_value = dplyr::if_else(.data$significant_for_clustering, .data$logFC, 0)
    )

  row_idx <- match(test_res$gene, rownames(mat))
  col_idx <- match(test_res$feature, colnames(mat))
  keep <- !is.na(row_idx) & !is.na(col_idx)

  mat[cbind(row_idx[keep], col_idx[keep])] <- test_res$cluster_value[keep]
  mat[!is.finite(mat)] <- 0

  list(matrix = mat, metadata = metadata)
}

select_timecourse_k <- function(
    mat,
    k = "auto",
    k_range = 2:10,
    distance = c("correlation", "euclidean"),
    max_features = 2000,
    seed = 1
) {
  distance <- match.arg(distance)
  mat <- as.matrix(mat)
  mat[!is.finite(mat)] <- 0

  if (!identical(k, "auto")) {
    return(list(k = as.integer(k), scores = NULL, method = "user"))
  }

  if (nrow(mat) > max_features) {
    set.seed(seed)
    pick <- sample(seq_len(nrow(mat)), max_features)
    mat_for_k <- mat[pick, , drop = FALSE]
  } else {
    mat_for_k <- mat
  }

  max_possible <- max(2, min(nrow(mat_for_k) - 1, max(k_range)))
  k_range <- k_range[k_range >= 2 & k_range <= max_possible]
  if (length(k_range) == 0) stop("No valid k values in k_range.")

  if (distance == "correlation") {
    cor_m <- suppressWarnings(stats::cor(t(mat_for_k), method = "spearman", use = "pairwise.complete.obs"))
    cor_m[!is.finite(cor_m)] <- 0
    d <- stats::as.dist(1 - cor_m)
  } else {
    d <- stats::dist(mat_for_k)
  }

  scores <- lapply(k_range, function(kk) {
    set.seed(seed)
    km <- stats::kmeans(mat_for_k, centers = kk, nstart = 25, iter.max = 100)
    sil <- if (requireNamespace("cluster", quietly = TRUE)) {
      mean(cluster::silhouette(km$cluster, d)[, "sil_width"], na.rm = TRUE)
    } else {
      stats::cor(
        as.numeric(d),
        as.numeric(stats::dist(km$centers[km$cluster, , drop = FALSE])),
        use = "pairwise.complete.obs"
      )
    }
    data.frame(k = kk, score = sil, stringsAsFactors = FALSE)
  })
  scores <- do.call(rbind, scores)
  scores <- scores[order(scores$score, decreasing = TRUE), , drop = FALSE]
  list(k = scores$k[1], scores = scores, method = "average_silhouette")
}

run_hybrid_temporal_clustering <- function(
    mat,
    metadata,
    k = "auto",
    k_range = 2:10,
    weights = c(shape = 0.35, amplitude = 0.25, trend = 0.25, timing = 0.15),
    max_features = 2000,
    seed = 1
) {
  mat <- as.matrix(mat)
  mat[!is.finite(mat)] <- 0
  metadata <- metadata[match(colnames(mat), metadata$feature), , drop = FALSE]

  if (!all(c("shape", "amplitude", "trend", "timing") %in% names(weights))) {
    stop("weights must include shape, amplitude, trend and timing.")
  }
  weights <- weights[c("shape", "amplitude", "trend", "timing")]
  weights <- weights / sum(weights)

  dist_full <- hybrid_temporal_distance(mat, metadata, weights)
  hc <- stats::hclust(dist_full, method = "ward.D2")

  selected_k <- select_hclust_k(
    dist_obj = dist_full,
    hc = hc,
    k = k,
    k_range = k_range,
    max_features = max_features,
    seed = seed
  )

  cluster_id <- stats::cutree(hc, k = selected_k$k)
  names(cluster_id) <- rownames(mat)

  list(
    cluster = cluster_id,
    selected_k = selected_k,
    hclust = hc,
    distance = dist_full
  )
}

hybrid_temporal_distance <- function(
    mat,
    metadata,
    weights = c(shape = 0.35, amplitude = 0.25, trend = 0.25, timing = 0.15),
    logfc_cap = 4
) {
  mat <- as.matrix(mat)
  mat[!is.finite(mat)] <- 0
  mat <- pmax(pmin(mat, logfc_cap), -logfc_cap)

  shape_mat <- t(scale(t(mat)))
  shape_mat[!is.finite(shape_mat)] <- 0
  shape_cor <- suppressWarnings(stats::cor(t(shape_mat), method = "pearson"))
  shape_cor[!is.finite(shape_cor)] <- 0
  d_shape <- stats::as.dist((1 - shape_cor) / 2)

  d_amplitude <- normalize_dist(stats::dist(mat, method = "euclidean"))

  trend_mat <- build_trend_matrix(mat, metadata)
  d_trend <- normalize_dist(stats::dist(trend_mat, method = "euclidean"))

  timing_mat <- (mat != 0) * 1
  d_timing <- normalize_dist(stats::dist(timing_mat, method = "binary"))

  d <- weights["shape"] * normalize_dist(d_shape) +
    weights["amplitude"] * d_amplitude +
    weights["trend"] * d_trend +
    weights["timing"] * d_timing

  attr(d, "method") <- "hybrid_temporal_distance"
  d
}

build_trend_matrix <- function(mat, metadata) {
  pieces <- lapply(unique(metadata$series), function(ss) {
    cols <- metadata$feature[metadata$series == ss]
    cols <- cols[cols %in% colnames(mat)]
    if (length(cols) < 2) {
      return(NULL)
    }
    t(diff(t(mat[, cols, drop = FALSE])))
  })
  pieces <- pieces[!vapply(pieces, is.null, logical(1))]
  if (length(pieces) == 0) {
    return(matrix(0, nrow = nrow(mat), ncol = 1, dimnames = list(rownames(mat), "trend0")))
  }
  trend <- do.call(cbind, pieces)
  rownames(trend) <- rownames(mat)
  trend[!is.finite(trend)] <- 0
  trend
}

normalize_dist <- function(d) {
  x <- as.numeric(d)
  finite <- is.finite(x)
  if (!any(finite) || max(x[finite], na.rm = TRUE) == 0) {
    x[] <- 0
  } else {
    x[finite] <- x[finite] / stats::quantile(x[finite], probs = 0.95, na.rm = TRUE, names = FALSE)
    x[!finite] <- 0
    x <- pmin(x, 1)
  }
  out <- d
  out[] <- x
  out
}

select_hclust_k <- function(
    dist_obj,
    hc,
    k = "auto",
    k_range = 2:10,
    max_features = 2000,
    seed = 1
) {
  if (!identical(k, "auto")) {
    return(list(k = as.integer(k), scores = NULL, method = "user"))
  }

  n <- attr(dist_obj, "Size")
  k_range <- k_range[k_range >= 2 & k_range <= max(2, n - 1)]
  if (length(k_range) == 0) stop("No valid k values in k_range.")

  use_idx <- seq_len(n)
  if (n > max_features) {
    set.seed(seed)
    use_idx <- sort(sample(seq_len(n), max_features))
  }

  dist_m <- as.matrix(dist_obj)
  dist_sub <- stats::as.dist(dist_m[use_idx, use_idx, drop = FALSE])

  scores <- lapply(k_range, function(kk) {
    cluster_id <- stats::cutree(hc, k = kk)[use_idx]
    sil <- if (requireNamespace("cluster", quietly = TRUE) && length(unique(cluster_id)) > 1) {
      mean(cluster::silhouette(cluster_id, dist_sub)[, "sil_width"], na.rm = TRUE)
    } else {
      NA_real_
    }
    data.frame(k = kk, score = sil, stringsAsFactors = FALSE)
  })

  scores <- do.call(rbind, scores)
  scores <- scores[order(scores$score, decreasing = TRUE), , drop = FALSE]
  list(k = scores$k[1], scores = scores, method = "hybrid_silhouette")
}

summarize_timecourse_clusters <- function(trajectory, gene_cluster, feature_info) {
  centroid_mtx <- rowsum(trajectory[gene_cluster$gene, , drop = FALSE], gene_cluster$cluster, na.rm = TRUE)
  cluster_n <- table(gene_cluster$cluster)
  centroid_mtx <- centroid_mtx / as.numeric(cluster_n[rownames(centroid_mtx)])

  centroid_long <- as.data.frame(centroid_mtx) %>%
    tibble::rownames_to_column("cluster") %>%
    tidyr::pivot_longer(-"cluster", names_to = "feature", values_to = "value") %>%
    dplyr::left_join(feature_info, by = "feature")

  cluster_summary <- centroid_long %>%
    dplyr::group_by(.data$cluster) %>%
    dplyr::summarise(
      n_genes = as.integer(cluster_n[.data$cluster[1]]),
      max_abs_change = max(abs(.data$value), na.rm = TRUE),
      pattern = .data$cluster[1],
      pattern_signature = summarize_series_signature(.data$value, .data$time, .data$series),
      centroid_change_time = infer_centroid_change_time(.data$value, .data$time, .data$series),
      .groups = "drop"
    ) %>%
    dplyr::arrange(.data$cluster)

  list(cluster_summary = cluster_summary, centroids = centroid_long)
}

summarize_series_signature <- function(value, time, series) {
  df <- data.frame(value = value, time = time, series = series, stringsAsFactors = FALSE)
  sig <- vapply(split(df, df$series), function(dd) {
    paste0(unique(dd$series), ":", label_single_centroid(dd$value))
  }, character(1))
  paste(sig, collapse = " | ")
}

label_single_centroid <- function(y) {
  y <- as.numeric(y)
  if (length(y) < 3 || all(!is.finite(y))) return("Mixed")
  y[!is.finite(y)] <- 0
  t <- seq_along(y)
  rho <- suppressWarnings(stats::cor(y, t, method = "spearman", use = "complete.obs"))
  if (!is.finite(rho)) rho <- 0
  end_value <- y[length(y)]
  peak_value <- y[which.max(abs(y))]
  max_abs <- abs(peak_value)
  if (max_abs < 0.1) return("Flat")
  return_to_baseline <- abs(end_value) < 0.4 * max_abs
  if (return_to_baseline && peak_value > 0) return("Transient_up")
  if (return_to_baseline && peak_value < 0) return("Transient_down")
  if (rho >= 0.5 && end_value > 0) return("Sustained_up")
  if (rho <= -0.5 && end_value < 0) return("Sustained_down")
  if (end_value > 0) return("Late_up")
  if (end_value < 0) return("Late_down")
  "Mixed"
}

test_timecourse_changes <- function(
    x,
    assay_name = "conc",
    time_col = "time",
    series_col = NULL,
    baseline = NULL,
    log2_offset = NULL,
    p_adj_cutoff = 0.05,
    logFC_cutoff = 0.5
) {
  if (!inherits(x, "SummarizedExperiment")) return(NULL)
  stopifnot(assay_name %in% SummarizedExperiment::assayNames(x))
  stopifnot(time_col %in% colnames(SummarizedExperiment::colData(x)))

  mat_raw <- as.matrix(SummarizedExperiment::assay(x, assay_name))
  if (is.null(log2_offset)) log2_offset <- infer_log2_offset(mat_raw)
  mat_log <- log2(mat_raw + log2_offset)
  meta <- as.data.frame(SummarizedExperiment::colData(x))
  meta$.__series__ <- if (is.null(series_col)) "Series1" else meta[[series_col]]
  meta$.__time__ <- meta[[time_col]]

  out <- lapply(unique(as.character(meta$.__series__)), function(ss) {
    idx_series <- which(as.character(meta$.__series__) == ss)
    time_levels <- sort_unique_time(meta$.__time__[idx_series])
    base <- choose_baseline(time_levels, baseline)
    cmp_times <- setdiff(time_levels, base)

    do.call(rbind, lapply(cmp_times, function(tt) {
      idx <- idx_series[as.character(meta$.__time__[idx_series]) %in% as.character(c(base, tt))]
      group <- factor(as.character(meta$.__time__[idx]), levels = as.character(c(base, tt)))
      if (length(unique(group)) < 2) return(NULL)

      design <- stats::model.matrix(~0 + group)
      colnames(design) <- make.names(levels(group))
      contrast <- paste0(make.names(as.character(tt)), "-", make.names(as.character(base)))

      fit <- limma::lmFit(mat_log[, idx, drop = FALSE], design)
      fit <- limma::contrasts.fit(fit, limma::makeContrasts(contrasts = contrast, levels = design))
      fit <- limma::eBayes(fit, trend = TRUE, robust = TRUE)
      tab <- limma::topTable(fit, number = Inf, sort.by = "none")
      tab$gene <- rownames(tab)
      tab$series <- ss
      tab$time <- tt
      tab$baseline <- base
      tab$significant <- tab$adj.P.Val <= p_adj_cutoff & abs(tab$logFC) >= logFC_cutoff
      tab[, c("gene", "series", "baseline", "time", "logFC", "P.Value", "adj.P.Val", "significant")]
    }))
  })

  out <- do.call(rbind, out)
  rownames(out) <- NULL
  out
}

infer_log2_offset <- function(x) {
  x <- as.numeric(x)
  pos_min <- suppressWarnings(min(x[x > 0], na.rm = TRUE))
  if (!is.finite(pos_min)) return(1e-6)
  max(pos_min / 2, 1e-6)
}

sort_unique_time <- function(x) {
  ux <- unique(x)
  xn <- suppressWarnings(as.numeric(as.character(ux)))
  if (all(is.finite(xn))) ux[order(xn)] else ux
}

choose_baseline <- function(time_levels, baseline = NULL) {
  if (is.null(baseline)) return(time_levels[1])
  hit <- which(as.character(time_levels) == as.character(baseline))
  if (length(hit) == 0) stop("baseline is not present in time points.")
  time_levels[hit[1]]
}

label_timecourse_pattern <- function(value, time, series) {
  df <- data.frame(value = value, time = time, series = series, stringsAsFactors = FALSE)
  series_scores <- lapply(split(df, df$series), function(dd) {
    y <- dd$value
    t <- seq_along(y)
    rho <- suppressWarnings(stats::cor(y, t, method = "spearman", use = "complete.obs"))
    end_delta <- y[length(y)] - y[1]
    peak <- y[which.max(abs(y))]
    returns <- abs(y[length(y)]) < 0.35 * max(abs(y), na.rm = TRUE)
    c(
      rho = ifelse(is.finite(rho), rho, 0),
      end_delta = end_delta,
      peak = peak,
      returns = as.numeric(returns)
    )
  })
  scores <- do.call(rbind, series_scores)
  mean_rho <- mean(scores[, "rho"], na.rm = TRUE)
  mean_end <- mean(scores[, "end_delta"], na.rm = TRUE)
  peak_sign <- sign(mean(scores[, "peak"], na.rm = TRUE))
  adaptive <- mean(scores[, "returns"], na.rm = TRUE) >= 0.5

  if (adaptive && peak_sign > 0) return("Adaptive_up")
  if (adaptive && peak_sign < 0) return("Adaptive_down")
  if (mean_rho >= 0.6 && mean_end > 0) return("Sustained_up")
  if (mean_rho <= -0.6 && mean_end < 0) return("Sustained_down")
  if (mean_end > 0.5) return("Late_up")
  if (mean_end < -0.5) return("Late_down")
  if (peak_sign > 0) return("Transient_up")
  if (peak_sign < 0) return("Transient_down")
  "Mixed"
}

infer_centroid_change_time <- function(value, time, series) {
  df <- data.frame(value = value, time = time, series = series, stringsAsFactors = FALSE)
  tt <- lapply(split(df, df$series), function(dd) {
    if (nrow(dd) < 2) return(NA_character_)
    jump <- abs(diff(dd$value))
    as.character(dd$time[which.max(jump) + 1])
  })
  names(sort(table(unlist(tt)), decreasing = TRUE))[1]
}

auto_consensus_timecourse_pattern_clustering <- function(
    x,
    assay_name = "conc",
    time_col = "time",
    series_col = "cell_line",
    baseline = NULL,
    min_mean = 10,
    min_sd = 0.15,
    log2_offset = NULL,
    p_adj_cutoff = 0.05,
    logFC_cutoff = 0.5,
    min_series = 3,
    keep_no_change = FALSE,
    seed = 1
) {
  set.seed(seed)

  prepared <- prepare_timecourse_matrix(
    x = x,
    assay_name = assay_name,
    time_col = time_col,
    series_col = series_col,
    baseline = baseline,
    log2_offset = log2_offset,
    common_features = TRUE
  )

  test_res <- test_timecourse_changes(
    x = x,
    assay_name = assay_name,
    time_col = time_col,
    series_col = series_col,
    baseline = baseline,
    log2_offset = prepared$log2_offset,
    p_adj_cutoff = p_adj_cutoff,
    logFC_cutoff = logFC_cutoff
  )

  trajectory <- prepared$trajectory
  raw_mean <- prepared$raw_mean
  feature_mean <- rowMeans(raw_mean, na.rm = TRUE)
  feature_sd <- apply(trajectory, 1, stats::sd, na.rm = TRUE)
  keep_feature <- is.finite(feature_mean) & is.finite(feature_sd) &
    feature_mean >= min_mean & feature_sd >= min_sd
  trajectory <- trajectory[keep_feature, , drop = FALSE]

  series_levels <- unique(prepared$metadata$series)
  if (length(series_levels) < min_series) {
    stop("Detected fewer series than min_series.")
  }

  pattern_by_series <- classify_gene_pattern_by_series(
    trajectory = trajectory,
    metadata = prepared$metadata,
    timepoint_tests = test_res,
    p_adj_cutoff = p_adj_cutoff,
    logFC_cutoff = logFC_cutoff
  )

  pattern_wide <- pattern_by_series %>%
    dplyr::select("gene", "series", "series_pattern") %>%
    tidyr::pivot_wider(
      names_from = "series",
      values_from = "series_pattern",
      names_prefix = "pattern_"
    )

  time_wide <- pattern_by_series %>%
    dplyr::select("gene", "series", "first_significant_time") %>%
    tidyr::pivot_wider(
      names_from = "series",
      values_from = "first_significant_time",
      names_prefix = "first_time_"
    )

  pattern_cols <- grep("^pattern_", colnames(pattern_wide), value = TRUE)
  consensus_df <- pattern_wide %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      n_series_called = sum(!is.na(c_across(dplyr::all_of(pattern_cols)))),
      n_unique_pattern = length(unique(stats::na.omit(c_across(dplyr::all_of(pattern_cols))))),
      consensus_pattern = {
        vals <- stats::na.omit(c_across(dplyr::all_of(pattern_cols)))
        if (length(vals) == 0 || length(unique(vals)) != 1) NA_character_ else unique(vals)
      },
      is_consensus = .data$n_series_called >= min_series &&
        .data$n_unique_pattern == 1 &&
        (keep_no_change || !.data$consensus_pattern %in% c("No_significant_change", "Mixed"))
    ) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(time_wide, by = "gene")

  consensus_genes <- consensus_df %>%
    dplyr::filter(.data$is_consensus) %>%
    dplyr::pull(.data$gene)

  if (length(consensus_genes) == 0) {
    stop("No genes have the same non-flat pattern across all series under current thresholds.")
  }

  gene_cluster <- consensus_df %>%
    dplyr::filter(.data$is_consensus) %>%
    dplyr::transmute(
      gene = .data$gene,
      cluster = .data$consensus_pattern,
      pattern = .data$consensus_pattern
    )

  cluster_summary <- gene_cluster %>%
    dplyr::count(.data$cluster, .data$pattern, name = "n_genes") %>%
    dplyr::arrange(dplyr::desc(.data$n_genes))

  cluster_change <- pattern_by_series %>%
    dplyr::inner_join(gene_cluster, by = "gene") %>%
    dplyr::group_by(.data$cluster, .data$pattern, .data$series) %>%
    dplyr::summarise(
      n_genes = dplyr::n(),
      first_cluster_significant_time = {
        tt <- stats::na.omit(.data$first_significant_time)
        if (length(tt) == 0) NA_character_ else names(sort(table(tt), decreasing = TRUE))[1]
      },
      median_max_abs_logFC = stats::median(.data$max_abs_logFC, na.rm = TRUE),
      .groups = "drop"
    )

  cluster_input <- t(scale(t(trajectory[consensus_genes, , drop = FALSE])))
  cluster_input[!is.finite(cluster_input)] <- 0

  consistency_audit <- consensus_df %>%
    dplyr::count(.data$is_consensus, .data$consensus_pattern, name = "n_genes") %>%
    dplyr::arrange(.data$is_consensus, .data$consensus_pattern)

  list(
    gene_cluster = gene_cluster,
    gene_change = pattern_by_series,
    gene_pattern_by_series = pattern_by_series,
    gene_consensus = consensus_df,
    cluster_change = cluster_change,
    cluster_summary = cluster_summary,
    consistency_audit = consistency_audit,
    timepoint_tests = test_res,
    trajectory = trajectory[consensus_genes, , drop = FALSE],
    cluster_input = cluster_input,
    metadata = prepared$metadata,
    parameters = list(
      assay_name = assay_name,
      time_col = time_col,
      series_col = series_col,
      baseline = baseline,
      min_mean = min_mean,
      min_sd = min_sd,
      p_adj_cutoff = p_adj_cutoff,
      logFC_cutoff = logFC_cutoff,
      min_series = min_series,
      keep_no_change = keep_no_change
    )
  )
}

classify_gene_pattern_by_series <- function(
    trajectory,
    metadata,
    timepoint_tests,
    p_adj_cutoff = 0.05,
    logFC_cutoff = 0.5
) {
  genes <- rownames(trajectory)
  series_levels <- unique(metadata$series)

  out <- lapply(series_levels, function(ss) {
    cols <- metadata$feature[metadata$series == ss]
    cols <- cols[cols %in% colnames(trajectory)]
    time_values <- metadata$time[match(cols, metadata$feature)]

    do.call(rbind, lapply(genes, function(gg) {
      y <- as.numeric(trajectory[gg, cols])
      tests <- timepoint_tests %>%
        dplyr::filter(.data$gene == gg, .data$series == ss)

      series_pattern <- classify_one_series_pattern(
        y = y,
        time = time_values,
        tests = tests,
        p_adj_cutoff = p_adj_cutoff,
        logFC_cutoff = logFC_cutoff
      )

      sig_tests <- tests %>%
        dplyr::filter(.data$adj.P.Val <= p_adj_cutoff, abs(.data$logFC) >= logFC_cutoff) %>%
        dplyr::arrange(.data$time)

      data.frame(
        gene = gg,
        series = ss,
        series_pattern = series_pattern,
        first_significant_time = if (nrow(sig_tests) == 0) NA_character_ else as.character(sig_tests$time[1]),
        strongest_time = if (nrow(tests) == 0) NA_character_ else as.character(tests$time[which.max(abs(tests$logFC))]),
        max_abs_logFC = if (nrow(tests) == 0) NA_real_ else max(abs(tests$logFC), na.rm = TRUE),
        stringsAsFactors = FALSE
      )
    }))
  })

  dplyr::bind_rows(out)
}

classify_one_series_pattern <- function(
    y,
    time,
    tests,
    p_adj_cutoff = 0.05,
    logFC_cutoff = 0.5
) {
  if (length(y) < 3 || all(!is.finite(y))) return("Mixed")
  y[!is.finite(y)] <- NA_real_

  sig_tests <- tests %>%
    dplyr::filter(.data$adj.P.Val <= p_adj_cutoff, abs(.data$logFC) >= logFC_cutoff)

  if (nrow(sig_tests) == 0) return("No_significant_change")

  y0 <- y
  y0[is.na(y0)] <- 0
  t <- seq_along(y0)
  rho <- suppressWarnings(stats::cor(y0, t, method = "spearman", use = "complete.obs"))
  if (!is.finite(rho)) rho <- 0

  end_value <- y0[length(y0)]
  peak_idx <- which.max(abs(y0))
  peak_value <- y0[peak_idx]
  max_abs <- abs(peak_value)
  return_to_baseline <- abs(end_value) < 0.4 * max_abs

  sig_sign <- sign(stats::median(sig_tests$logFC, na.rm = TRUE))
  if (!is.finite(sig_sign) || sig_sign == 0) sig_sign <- sign(peak_value)

  if (return_to_baseline && sig_sign > 0) return("Transient_up")
  if (return_to_baseline && sig_sign < 0) return("Transient_down")
  if (rho >= 0.5 && end_value >= logFC_cutoff) return("Sustained_up")
  if (rho <= -0.5 && end_value <= -logFC_cutoff) return("Sustained_down")
  if (end_value >= logFC_cutoff && sig_sign > 0) return("Late_up")
  if (end_value <= -logFC_cutoff && sig_sign < 0) return("Late_down")
  if (sig_sign > 0) return("Transient_up")
  if (sig_sign < 0) return("Transient_down")
  "Mixed"
}

