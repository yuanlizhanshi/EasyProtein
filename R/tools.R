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
#'
#' @return Long-format data.frame with imputed values.
#' @export
impute_low1pct_or_median_raw <- function(
    df,
    id_col        = "Protein.Ids",
    sample_col    = "sample",
    value_col     = "raw_value",
    condition_col = "condition",
    low_prob      = 0.01,
    return_log2   = FALSE,
    seed          = 1
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

      if (miss_cnt[i] > n_reps / 2) {
        # more than half missing -> geometric mean
        obs <- x[!na_idx]
        if (length(obs) == 0) {
          x[na_idx] <- cond_low_log
        } else {
          med <- geom_mean(obs, na.rm = TRUE)
          x[na_idx] <- med
        }
      } else {
        # less than half missing -> low-end sampling
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
  colnames(design) <- levels(group)

  if (is.null(contrast_str)) {
    lv <- levels(group)
    contrast_str <- paste0(lv[2], "-", lv[1])
  }

  contrast.mat <- limma::makeContrasts(contrasts = contrast_str, levels = design)

  fit <- limma::lmFit(expr_log, design)
  fit2 <- limma::contrasts.fit(fit, contrast.mat)
  fit2 <- limma::eBayes(fit2, trend = trend, robust = robust)
  tt <- limma::topTable(fit2, number = Inf, sort.by = "P")

  lv <- levels(group)
  grp_means <- sapply(lv, function(g) rowMeans(expr_log[, group == g, drop = FALSE], na.rm = TRUE))

  if (is.vector(grp_means)) grp_means <- cbind(grp_means)
  colnames(grp_means) <- paste0("mean_log2_", lv)

  res <- cbind(
    data.frame(feature = rownames(tt), tt, check.names = FALSE),
    grp_means[rownames(tt), , drop = FALSE]
  )

  parts <- strsplit(contrast_str, "-")[[1]]
  if (length(parts) == 2 && all(parts %in% lv)) {
    res$delta_mean_log2 <- res[[paste0("mean_log2_", parts[1])]] -
      res[[paste0("mean_log2_", parts[2])]]
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
  }

  if (is.null(contrast_str)) {
    if (!is.null(donor)) {
      group_col <- grep("^group", colnames(design), value = TRUE)
      if (length(group_col) != 1) stop("Cannot determine unique group column.")
      contrast_str <- group_col
    } else {
      contrast_str <- paste0(lv[2], "-", lv[1])
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

  if (length(parts) == 2 && all(parts %in% lv)) {
    delta <- grp_means[, paste0("mean_log2_", parts[2])] -
      grp_means[, paste0("mean_log2_", parts[1])]
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
    df <- data.frame(protein_group = rownames(mtx), km_cluster = paste0("km", cl))
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
    df <- data.frame(protein_group = colnames(mtx), km_cluster = paste0("km", cl))
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
#' @return A data frame containing enriched GO/KEGG terms.
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
    use_internal_data = FALSE
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

  return(enriched@result)
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
#' @param filter Logical. If \code{TRUE}, features are collapsed at the gene level:
#'   the first entry per gene is kept after filtering non-empty gene names.
#'   This is useful when multiple proteins/isoforms map to the same gene symbol.
#'   Default is \code{TRUE}.
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
#' If \code{filter = TRUE}, the results are collapsed at the gene level by keeping
#' only one representative entry per gene, which is recommended when multiple
#' protein entries map to the same gene.
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


get_pc_contributors <- function(pca.res,se_obj= NULL, pc = 1, n = 20,filter =T, use = c("coord", "contrib")) {

  use <- match.arg(use)

  mat <- pca.res$var[[use]]

  pc_name <- paste0("Dim.", pc)
  if (!pc_name %in% colnames(mat)) {
    stop("PC no exist:", pc_name)
  }

  df <- data.frame(
    Protein.Ids = rownames(mat),
    value = mat[, pc_name]
  ) %>%
    left_join(as.data.frame(rowData(se_obj)),by = 'Protein.Ids') %>%
    mutate(gene = str_extract(Genes, '^[^;]+'))

  if(filter){
    df <- df %>%
      dplyr::filter(gene != '') %>%
      group_by(gene) %>%
      slice_head(n = 1)
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

