library(bslib)
library(ComplexHeatmap)
library(dplyr)
library(DT)
library(ggiraph)
library(ggplot2)
library(ggpubr)
library(htmltools)
library(httr)
library(limma)
library(RColorBrewer)
library(reactable)
library(readr)
library(readxl)
library(rlang)
library(shiny)
library(shinyjqui)
library(shinyjs)
library(shinydashboard)
library(shinythemes)
library(shinyWidgets)
library(stringr)
library(SummarizedExperiment)
library(tidyverse)
library(cowplot)


# 控制用户文件上传最大为2G
options(shiny.maxRequestSize=1024*1024^2)

col<- rev(colorRampPalette(c("#cc0000", "#FFff00",'#66ccff','#000066'))(50))

# 定义自定义颜色custom_colors
custom_colors <- c(
  "#BDBDBD", "#8DB5CE", "#542E8B", "#D7BD90", "#C82129", "#F296C0", "#647950", "#114B9B", "#FBBD90", "#193E51", "#BE93BF")

getPalette = colorRampPalette(custom_colors)


cols <- c("OrangeRed","SlateBlue3","DarkOrange","GreenYellow","Purple",
          "DarkSlateGray","Gold","DarkGreen","DeepPink2","Red4","#4682B4",
          "#FFDAB9","#708090","#836FFF","#CDC673","#CD9B1D","#FF6EB4","#CDB5CD"
          ,"#008B8B","#43CD80","#483D8B","#66CD00","#CDC673","#CDAD00","#CD9B9B"
          ,"#FF8247","#8B7355","#8B3A62","#68228B","#CDB7B5","#CD853F","#6B8E23"
          ,"#696969","#7B68EE","#9F79EE","#B0C4DE","#7A378B","#66CDAA","#EEE8AA"
          ,"#00FF00","#EEA2AD","#A0522D","#000080","#E9967A","#00CDCD","#8B4500"
          ,"#DDA0DD","#EE9572","#EEE9E9","#8B1A1A","#8B8378","#EE9A49","#EECFA1"
          ,"#8B4726","#8B8878","#EEB4B4","#C1CDCD","#8B7500","#0000FF","#EEEED1"
          ,"#4F94CD","#6E8B3D","#B0E2FF","#76EE00","#A2B5CD","#548B54","#BBFFFF"
          ,"#B4EEB4","#00C5CD","#008B8B","#7FFFD4","#8EE5EE","#43CD80","#68838B"
          ,"#00FF00","#B9D3EE","#9ACD32","#00688B","#FFEC8B","#1C86EE","#CDCD00"
          ,"#473C8B","#FFB90F","#EED5D2","#CD5555","#CDC9A5","#FFE7BA","#FFDAB9"
          ,"#CD661D","#CDC5BF","#FF8C69","#8A2BE2","#CD8500","#B03060","#FF6347"
          ,"#FF7F50","#CD0000","#F4A460","#FFB5C5","#DAA52")


STRING_BASE <- "https://string-db.org/api"
CALLER_ID   <- "EasyProtein"  

`%||%` <- function(a, b) if (is.null(a)) b else a
js_escape  <- function(x) gsub("'", "\\\\'", x, fixed = TRUE)
#Data transformation functions----

scale_mtx <- function(mtx){
  new_mtx <- as.matrix(t(apply(mtx, 1, scale)))
  colnames(new_mtx) <- colnames(mtx)
  return(new_mtx)
}
geom_mean <- function(x, na.rm = TRUE) {
  
  if (na.rm) x <- x[!is.na(x)]
  
  exp(mean(log(x)))
}

catable <- function (data, categories = c(quantile(data, c(0.01, 0.1, 0.5, 0.9, 0.99), na.rm = TRUE)), cumulative = FALSE, na.rm = TRUE, digits = 3){
  if (!is(data, "numeric")) 
    stop("data should be numeric vector")
  if (!is(categories, "numeric")) 
    stop("categories should be numeric vector")
  ouv <- rep(NA, length(data))
  categories <- sort(categories)
  outmat <- matrix(rep(0, 2 * (length(categories) + 1)), nrow = 2)
  tot <- sum(!is.na(data), na.rm = na.rm)
  outmat[1, 1] <- sum(data <= categories[1], na.rm = na.rm)
  outmat[2, 1] <- (outmat[1, 1]/tot)
  for (i in 1:(length(categories) - 1)) {
    outmat[1, i + 1] <- sum(data > categories[i] & data <= 
                              categories[i + 1], na.rm = na.rm)
    outmat[2, i + 1] <- (outmat[1, i + 1]/tot)
  }
  outmat[1, length(categories) + 1] <- sum(data > categories[length(categories)], 
                                           na.rm = na.rm)
  outmat[2, length(categories) + 1] <- (outmat[1, length(categories) + 
                                                 1]/tot)
  if (cumulative) {
    for (i in 2:(length(categories) + 1)) {
      outmat[1, i] <- (outmat[1, i] + outmat[1, i - 1])
      outmat[2, i] <- (outmat[2, i] + outmat[2, i - 1])
    }
  }
  cnams <- rep("", length(categories) + 1)
  cnams[1] <- paste("X<=", categories[1], sep = "")
  for (i in 1:(length(categories) - 1)) {
    if (cumulative) 
      cnams[i + 1] <- paste("X<=", categories[i + 1], sep = "")
    else cnams[i + 1] <- paste(categories[i], "<X<=", categories[i + 
                                                                   1], sep = "")
  }
  if (cumulative) 
    cnams[length(categories) + 1] <- paste("all X", sep = "")
  else cnams[length(categories) + 1] <- paste("X>", categories[length(categories)], 
                                              sep = "")
  colnames(outmat) <- cnams
  rownames(outmat) <- c("No", "Prop")
  outmat <- round(outmat, digits = digits)
  outmat
}


make_download_pdf <- function(plot_expr, input, suffix = NULL,
                              width = 7, height = 5,
                              input_field = "se_file") {
  downloadHandler(
    filename = function() {
      base <- tools::file_path_sans_ext(basename(input[[input_field]]$name))
      paste0(base, if (!is.null(suffix)) paste0("_", suffix) else "", ".pdf")
    },
    content = function(file) {
      w <- if (is.function(width))  width()  else width
      h <- if (is.function(height)) height() else height
      cairo_pdf(file, width = w, height = h, fallback_resolution = 300)
      print(plot_expr())  # plot_expr() 必须返回 ggplot
      dev.off()
    }
  )
}

#old
# rawdata2se <- function(exp_file, obs_col = "Protein.Ids") {
#   rawdata <- data.table::fread(exp_file) %>% as.data.frame()
#   colnames(rawdata) <- gsub("\\\\", "/", colnames(rawdata))
#   rawdata <- fix_duplicate_protein_ids(rawdata,id_col = obs_col)
#   
#   
#   var <- rawdata %>% dplyr::select(Protein.Ids, Genes) %>% as.data.frame()
#   rownames(var) <- rawdata[[obs_col]]
#   
#   
#   rawdata_mtx <- rawdata[, stringr::str_which(colnames(rawdata), "raw")]
#   rownames(rawdata_mtx) <- rawdata[[obs_col]]
#   colnames(rawdata_mtx) <- tools::file_path_sans_ext(basename(colnames(rawdata_mtx)))
#   
#   
#   obs <- tibble::tibble(
#     sample = colnames(rawdata_mtx),
#     condition = stringr::str_extract(colnames(rawdata_mtx), "\\w+(?=_[^_]*$)"),
#     rep = stringr::str_extract(colnames(rawdata_mtx), "\\d+$"),
#     group = paste0(condition, "#", rep)
#   ) %>% as.data.frame()
#   
#   if (all(is.na(obs$condition))) {
#     obs$condition <- obs$sample
#   }
#   rownames(obs) <- colnames(rawdata_mtx)
#   
#   
#   rawdata_df <- rawdata_mtx %>%
#     tibble::rownames_to_column(obs_col) %>%
#     tidyr::pivot_longer(cols = -all_of(obs_col), names_to = "sample", values_to = "raw_value") %>%
#     dplyr::left_join(obs, by = c("sample"))
#   
#   rawdata_impute_df <- impute_low1pct_or_median_raw(rawdata_df,id_col = obs_col)
#   
#   rawdata_impute_df_wide <- rawdata_impute_df %>%
#     tidyr::pivot_wider(id_cols = all_of(obs_col), names_from = "sample", values_from = "raw_value")
#   
#   rawdata_impute_df_mtx <- rawdata_impute_df_wide[, -1] %>% as.matrix()
#   rownames(rawdata_impute_df_mtx) <- rawdata_impute_df_wide[[obs_col]]
#   rawdata_impute_df_mtx <- rawdata_impute_df_mtx[, obs$sample]
#   
#   se <- SummarizedExperiment::SummarizedExperiment(
#     assays = list(
#       raw_intensity = as.matrix(rawdata_mtx[rownames(rawdata_impute_df_mtx), colnames(rawdata_impute_df_mtx)]),
#       intensity = as.matrix(rawdata_impute_df_mtx),
#       conc = edgeR::cpm(as.matrix(rawdata_impute_df_mtx)),
#       zscale = scale_mtx(edgeR::cpm(as.matrix(rawdata_impute_df_mtx)))
#     ),
#     rowData = S4Vectors::DataFrame(var[rownames(rawdata_impute_df_mtx), ]),
#     colData = S4Vectors::DataFrame(obs[colnames(rawdata_impute_df_mtx), ])
#   )
#   
#   return(se)
# }
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

rawdata2se <- function(
    exp_file,
    obs_col = "Protein.Ids",
    enable_detect_outlier_gene  = F,
    fc_threshold = 5,
    valid_group_cutoff = -1,
    frac_NA_threshold = 0.5,
    stable_group_cutoff = -1,
    cv_threshod = 0.5
) {
  
  #---- Step 1. 读取数据 
  rawdata <- data.table::fread(exp_file) %>% as.data.frame()
  colnames(rawdata) <- gsub("\\\\", "/", colnames(rawdata))
  rawdata <- fix_duplicate_protein_ids(rawdata, id_col = obs_col)
  
  var <- rawdata %>% dplyr::select(.data[[obs_col]], Genes) %>% as.data.frame()
  rownames(var) <- rawdata[[obs_col]]
  
  rawdata_mtx <- rawdata[, stringr::str_which(colnames(rawdata), "raw")]
  rownames(rawdata_mtx) <- rawdata[[obs_col]]
  colnames(rawdata_mtx) <- tools::file_path_sans_ext(basename(colnames(rawdata_mtx)))
  
  # ---- Step 2. 样本注释信息 
  obs <- tibble::tibble(
    sample = colnames(rawdata_mtx),
    condition = stringr::str_extract(colnames(rawdata_mtx), "\\w+(?=_[^_]*$)"),
    rep = stringr::str_extract(colnames(rawdata_mtx), "\\d+$"),
    group = paste0(condition, "#", rep)
  ) %>% as.data.frame()
  if (all(is.na(obs$condition))) obs$condition <- obs$sample
  rownames(obs) <- colnames(rawdata_mtx)
  
  # 判断每个condition的rep数量
  rep_counts <- table(obs$condition)
  single_rep <- all(rep_counts <= 1)
  
  # message("检测到每个 condition 的重复数：")
  # print(rep_counts)
  message(if (single_rep) "⚠️ 仅有单重复，跳过FC、缺失与CV筛选步骤！"
          else "✅ 多重复存在，将执行完整筛选流程。")
  
  # ---- Step 3. 转长表并计算 fold change 
  rawdata_df <- rawdata_mtx %>%
    tibble::rownames_to_column(obs_col) %>%
    tidyr::pivot_longer(cols = -all_of(obs_col),
                        names_to = "sample", values_to = "raw_value") %>%
    dplyr::left_join(obs, by = c("sample"))
  
  # ---- 如果只有一个rep，直接跳过过滤 
  if (single_rep) {
    rawdata_impute_df <- impute_low1pct_or_median_raw(rawdata_df, id_col = obs_col)
    rawdata_impute_df_wide <- rawdata_impute_df %>%
      tidyr::pivot_wider(id_cols = all_of(obs_col),
                         names_from = "sample", values_from = "raw_value")
    rawdata_impute_df_mtx <- rawdata_impute_df_wide[, -1] %>% as.matrix()
    rownames(rawdata_impute_df_mtx) <- rawdata_impute_df_wide[[obs_col]]
    rawdata_impute_df_mtx <- rawdata_impute_df_mtx[, obs$sample]
    
    se <- SummarizedExperiment::SummarizedExperiment(
      assays = list(
        raw_intensity = as.matrix(rawdata_mtx[rownames(rawdata_impute_df_mtx),
                                              colnames(rawdata_impute_df_mtx)]),
        intensity = as.matrix(rawdata_impute_df_mtx),
        conc = edgeR::cpm(as.matrix(rawdata_impute_df_mtx)),
        zscale = scale_mtx(edgeR::cpm(as.matrix(rawdata_impute_df_mtx)))
      ),
      rowData = S4Vectors::DataFrame(var[rownames(rawdata_impute_df_mtx), ]),
      colData = S4Vectors::DataFrame(obs[colnames(rawdata_impute_df_mtx), ])
    )
    
    return(list(se = se,
                un_stable_gene = NULL,
                missing_gene_df = NULL))
  }
  
  # ---- Step 4. 多重复：执行完整筛选 
  rawdata_df <- rawdata_df %>%
    group_by(.data[[obs_col]], condition) %>%
    mutate(
      med_value = median(raw_value, na.rm = TRUE),
      fc = if_else(raw_value / med_value < 1,
                   med_value / raw_value,
                   raw_value / med_value)
    ) %>%
    ungroup()
  if (enable_detect_outlier_gene) {
    rawdata_df$raw_value[which(rawdata_df$fc > fc_threshold)] <- NA
  }
  
  
  # ---- Step 5. 缺失筛选 
  missing_gene <- rawdata_df %>%
    group_by(.data[[obs_col]], condition) %>%
    summarize(
      n_NA = sum(is.na(raw_value)),
      n_total = n(),
      frac_NA = n_NA / n_total,
      .groups = "drop"
    ) %>%
    mutate(valid = frac_NA < frac_NA_threshold) %>%
    group_by(.data[[obs_col]]) %>%
    summarize(
      n_valid_groups = sum(valid),
      total_groups = n()
    ) %>%
    dplyr::filter(n_valid_groups <= valid_group_cutoff)
  
  rawdata_df <- rawdata_df %>%
    dplyr::filter(!(.data[[obs_col]] %in% unique(missing_gene[[obs_col]])))
  
  missing_gene_df <- rawdata %>%
    dplyr::filter((.data[[obs_col]] %in% unique(missing_gene[[obs_col]])))
  
  # ---- Step 6. 缺失填充 
  rawdata_impute_df <- impute_low1pct_or_median_raw(rawdata_df, id_col = obs_col)
  rawdata_impute_df_wide <- rawdata_impute_df %>%
    tidyr::pivot_wider(id_cols = all_of(obs_col),
                       names_from = "sample", values_from = "raw_value")
  rawdata_impute_df_mtx <- rawdata_impute_df_wide[, -1] %>% as.matrix()
  rownames(rawdata_impute_df_mtx) <- rawdata_impute_df_wide[[obs_col]]
  rawdata_impute_df_mtx <- rawdata_impute_df_mtx[, obs$sample]
  
  # ---- Step 7. 构建 SE 
  se <- SummarizedExperiment::SummarizedExperiment(
    assays = list(
      raw_intensity = as.matrix(rawdata_mtx[rownames(rawdata_impute_df_mtx),
                                            colnames(rawdata_impute_df_mtx)]),
      intensity = as.matrix(rawdata_impute_df_mtx),
      conc = edgeR::cpm(as.matrix(rawdata_impute_df_mtx)),
      zscale = scale_mtx(edgeR::cpm(as.matrix(rawdata_impute_df_mtx)))
    ),
    rowData = S4Vectors::DataFrame(var[rownames(rawdata_impute_df_mtx), ]),
    colData = S4Vectors::DataFrame(obs[colnames(rawdata_impute_df_mtx), ])
  )
  
  # ---- Step 8. CV筛选 
  cv_df <- calc_gene_CV_by_condition(se)
  
  un_stable_cv_df <- cv_df %>%
    group_by(.data[[obs_col]]) %>%
    summarize(
      n_stable_groups = sum(CV < cv_threshod, na.rm = TRUE),
      total_groups = sum(!is.na(CV)),
      frac_stable = n_stable_groups / total_groups
    ) %>%
    dplyr::filter(n_stable_groups <= stable_group_cutoff)
  
  un_stable_gene_df <- rawdata %>%
    dplyr::filter((.data[[obs_col]] %in% unique(un_stable_cv_df[[obs_col]])))
  
  se <- se[setdiff(rownames(se), un_stable_gene_df[[obs_col]]), ]
  
  # ---- 返回 
  return(list(
    se = se,
    un_stable_gene = un_stable_gene_df,
    missing_gene_df = missing_gene_df
  ))
}


se2raw <- function(se){
  return(cbind(as.data.frame(rowData(se)), as.data.frame(assay(se, "raw_intensity"))))
}

se2internstiy <- function(se){
  return(cbind(as.data.frame(rowData(se)), as.data.frame(assay(se, "intensity"))))
}
se2conc <- function(se, group_by = 'condition') {
  
  expr_mean <- calc_gene_mean_by_condition(
    se = se,
    assay_name = 'conc',
    condition_col = group_by
  )
  colnames(expr_mean) <- paste0('Mean_', colnames(expr_mean))
  
  expr_median <- calc_gene_mean_by_condition(
    se = se,
    assay_name = 'conc',
    method = 'Median',
    condition_col = group_by
  )
  colnames(expr_median) <- paste0('Median_', colnames(expr_median))
  
  out <- cbind(
    as.data.frame(rowData(se)),
    as.data.frame(assay(se, "conc")),
    as.data.frame(expr_mean),
    as.data.frame(expr_median)
  )
  
  return(out)
}

se2scale<- function(se){
  return(cbind(as.data.frame(rowData(se)), as.data.frame(assay(se, "zscale"))))
}
se2DEGs <- function(se, compare_col = 'condition', ref = NULL, cmp = NULL,pair_col = NULL) {
  mtx <- assay(se, 'conc')
  meta <- as.data.frame(colData(se))
  
  if (!compare_col %in% colnames(meta)) {
    stop(paste0("列 ", compare_col, " 不存在于 colData(se) 中！"))
  }
  
  if (!is.null(ref) && !is.null(cmp)) {
    keep <- meta[[compare_col]] %in% c(ref, cmp)
    se <- se[, keep]
    mtx <- assay(se, 'conc')
    meta <- as.data.frame(colData(se))
    group <- factor(meta[[compare_col]], levels = c(ref, cmp))
  } else {
    group <- factor(meta[[compare_col]])
  }
  if (is.null(pair_col)){
    DEGs_res <- limma_protein_DE(mtx, group)
  }else{
    pair_col <- factor(meta[[pair_col]])
    DEGs_res <- limma_protein_DE_pair(mtx, group,pair_col)
  }
  
  
  exp_df <- as.data.frame(mtx) %>%
    tibble::rownames_to_column('Protein_id')
  
  
  med_df <- as.data.frame(mtx) %>% 
    tibble::rownames_to_column('Protein_id') %>%
    tidyr::pivot_longer(-Protein_id, names_to = "sample", values_to = "value")
  meta2 <- meta %>% tibble::rownames_to_column('.sample_id')
  med_df <- med_df %>%
    dplyr::left_join(meta2, by = c("sample" = ".sample_id")) %>%
    dplyr::group_by(Protein_id, !!rlang::sym(compare_col)) %>%
    dplyr::summarize(median_expr = median(value, na.rm = TRUE), .groups = "drop") %>%
    tidyr::pivot_wider(
      names_from = !!rlang::sym(compare_col),
      values_from = median_expr,
      names_prefix = "median_"
    )
  
  
  final_df <- as.data.frame(rowData(se)) %>%
    dplyr::left_join(exp_df, by = c('Protein.Ids' = 'Protein_id')) %>%
    dplyr::left_join(DEGs_res[, c(1, 8, 9, 2, 5, 6)], by = c('Protein.Ids' = 'feature')) %>%
    dplyr::left_join(med_df, by = c('Protein.Ids' = 'Protein_id'))
  
  attr(final_df, "ref_group") <- ref
  attr(final_df, "cmp_group") <- cmp
  
  return(final_df)
}
get_batch_DEGs <- function(se,compare_col = 'condition',start_col = 1){
  start_col_ref <- unique(se[[compare_col]])[start_col]
  DEGs_list <- map(unique(se[[compare_col]])[-start_col],function(x,se,ref,compare_col){
    DEGs_df <- se2DEGs(
      se,
      compare_col = compare_col,
      ref = start_col_ref,
      cmp = x
    )
  },se = se,ref = start_col_ref, compare_col = compare_col,.progress = T)
  names(DEGs_list) <- unique(se[[compare_col]])[-start_col]
  return(DEGs_list)
}


se2gene_group <- function(se,
                          assay = 'conc',
                          group_by = 'condition',
                          time_threhold = 0.75,
                          min_expresion_threhold = 1,
                          CV_with_time_threhold =  0.1,
                          padj_threhold = 0.05
){
  expr <- assay(se, assay)
  expr_log <- log2(expr)
  
  expr_mean <- calc_gene_mean_by_condition(se = se, assay_name= assay,condition_col = group_by)
  
  if (!all(rownames(expr_mean) == rownames(expr))) {
    stop('Error')
  }
  
  
  gene_info <- tibble(
    Protein.Ids = rownames(expr),
    Genes = rowData(se)[['Genes']],
    Mean_expression = rowMeans(expr),
    Median_exprssion = rowMedians(expr),
    CV_with_time = apply(expr_mean,1,calculate_cv),
    Spearman_with_time = apply(expr_mean,1,function(x,N = NULL){
      cor(x,1:N,method = 'spearman')
    },N = ncol(expr_mean))
  )
  
  
  
  
  gene_with_low_mean <- gene_info %>% 
    dplyr::filter(Mean_expression < min_expresion_threhold)
  
  gene_with_low_CV <- gene_info %>% 
    dplyr::filter(Mean_expression >= min_expresion_threhold)%>% 
    dplyr::filter(CV_with_time <= CV_with_time_threhold)
  
  gene_with_low_mean_and_low_cv <- union(gene_with_low_mean$Protein.Ids,gene_with_low_CV$Protein.Ids)
  gene_with_low_mean_and_low_cv_df <- tibble(
    gene = gene_with_low_mean_and_low_cv,
    gene_group = c(rep('Low expression',length(gene_with_low_mean$Protein.Ids)),
                   rep('Low CV',length(gene_with_low_CV$Protein.Ids))
    ),
    type = c(rep('Low expression',length(gene_with_low_mean$Protein.Ids)),
             rep('Low CV',length(gene_with_low_CV$Protein.Ids))
    )
  )
  
  
  
  gene_left <- gene_info %>% dplyr::filter(!(Protein.Ids %in% gene_with_low_mean_and_low_cv))
  
  
  gene_group_left <- classify_timecourse_by_round(expr_mean[gene_left$Protein.Ids,],rho_cut = time_threhold)
  
  gene_group_final <- bind_rows(gene_group_left,gene_with_low_mean_and_low_cv_df
  )
  gene_info_full <- left_join(gene_info,gene_group_final,by = c('Protein.Ids' = 'gene'))
  
  #Get log2FC
  all_DEGs <- get_batch_DEGs(se,compare_col = group_by,start_col = 1)
  gene_FC_matrix <- map2(all_DEGs,names(all_DEGs),function(df,name,padj_threhold = padj_threhold){
    df <- df %>% dplyr::filter(adj.P.Val <= padj_threhold) %>% 
      dplyr::select(Protein.Ids,logFC) 
    colnames(df)[2] <- name
    return(df)
  },padj_threhold = padj_threhold) %>% purrr::reduce(full_join,by = 'Protein.Ids') %>%
    mutate(
      across(everything(), ~replace_na(.x, 0))
    ) 
  colnames(gene_FC_matrix)[-1] <- paste0('log2FC_',colnames(gene_FC_matrix)[-1])
  
  
  gene_info_full <- gene_info_full %>% left_join(gene_FC_matrix,by = 'Protein.Ids') %>%
    mutate(
      across(everything(), ~replace_na(.x, 0))
    ) 
  
  
  if (all(rownames(rowData(se)) == gene_info_full$Protein.Ids)) {
    for (col in colnames(gene_info_full)) {
      if (!(col %in% colnames(rowData(se)))) {
        rowData(se)[col] = gene_info_full[col]
      }
    }
  }
  return(se)
}


summarize_se_by_coldata <- function(
    se,
    group_by,                 # 字符串：colData(se) 里的列名
    assay_name = "conc",
    method = c("mean", "median"),
    na.rm = TRUE
){
  method <- match.arg(method)
  
  if (!inherits(se, "SummarizedExperiment"))
    stop("`se` 必须是 SummarizedExperiment 对象。")
  
  if (!assay_name %in% SummarizedExperiment::assayNames(se))
    stop(sprintf("assay '%s' 不存在。可用：%s",
                 assay_name, paste(SummarizedExperiment::assayNames(se), collapse=", ")))
  
  cd <- SummarizedExperiment::colData(se)
  if (!group_by %in% colnames(cd))
    stop(sprintf("colData 中找不到分组列 '%s'。可用：%s",
                 group_by, paste(colnames(cd), collapse=", ")))
  
  grp <- as.vector(cd[[group_by]])
  if (all(is.na(grp)))
    stop("all  NA。")
  
  grp <- as.factor(grp)
  lv  <- levels(grp)
  
  mat <- SummarizedExperiment::assay(se, assay_name)
  if (!is.numeric(mat))
    stop("assay 矩阵不是数值型。")
  
  summarized_mat <- sapply(lv, function(g){
    cols <- which(grp == g)
    if (length(cols) == 0) {
      rep(NA_real_, nrow(mat))
    } else if (method == "mean") {
      rowMeans(mat[, cols, drop = FALSE], na.rm = na.rm)
    } else {
      apply(mat[, cols, drop = FALSE], 1, stats::median, na.rm = na.rm)
    }
  })
  
  # 保持维度名
  if (is.null(dim(summarized_mat))) {
    summarized_mat <- matrix(summarized_mat, ncol = 1)
    colnames(summarized_mat) <- lv[1]
  }
  rownames(summarized_mat) <- rownames(mat)
  colnames(summarized_mat) <- lv
  
  return(summarized_mat)
}


calculate_cv <- function(x) {
  mean_x <- mean(x)
  sd_x <- sd(x)
  cv <- sd_x / mean_x
  return(cv)
}
get_turning_point <- function(vec,fluctuate = 0.05){
  diff_vec <- diff(vec) + vec[-length(vec)]  * fluctuate
  direction <- sign(diff_vec)
  turning_point_index <- which(diff(direction) != 0) + 1
  return(length(turning_point_index))
}
calc_gene_CV_by_condition <- function(se, 
                                      assay_name = "conc", 
                                      condition_col = "condition") {
  stopifnot(assay_name %in% assayNames(se))
  stopifnot(condition_col %in% colnames(colData(se)))
  if (length(unique(se$condition)) == dim(se)[2]) {
    return(NULL) 
  }else{
    mtx  <- assay(se, assay_name)
    meta <- as.data.frame(colData(se))
    
    # 转为长格式
    df_long <- as.data.frame(mtx) %>%
      tibble::rownames_to_column("Protein.Ids") %>%
      tidyr::pivot_longer(
        cols = -Protein.Ids,
        names_to = "sample",
        values_to = "value"
      ) %>%
      left_join(meta, by = c("sample" ))
    
    # 计算每个gene在每个condition下的CV
    cv_df <- df_long %>%
      group_by(Protein.Ids, !!sym(condition_col)) %>%
      summarise(
        mean_val = mean(value, na.rm = TRUE),
        sd_val   = sd(value, na.rm = TRUE),
        CV       = sd_val / mean_val,
        n        = sum(!is.na(value)),
        .groups  = "drop"
      )
    
    # cv_wide <- cv_df %>%
    #   select(Protein.Ids, .data[[condition_col]], CV) %>%
    #   pivot_wider(
    #     names_from  = .data[[condition_col]],
    #     values_from = CV,
    #     names_prefix = "CV_"
    #   )
    
    return(cv_df)
  }
}
calc_gene_mean_by_condition <- function(se, 
                                        assay_name = "conc", 
                                        method = 'mean',
                                        condition_col = "condition") {
  
  stopifnot(assay_name %in% assayNames(se))
  stopifnot(condition_col %in% colnames(colData(se)))
  
  if (length(unique(se[[condition_col]])) == ncol(se)) {
    return(NULL)
  } else {
    
    mtx  <- assay(se, assay_name)
    meta <- as.data.frame(colData(se))
    
    gene_order <- rownames(mtx)
    
    df_long <- as.data.frame(mtx) %>%
      tibble::rownames_to_column("Protein.Ids") %>%
      tidyr::pivot_longer(
        cols = -Protein.Ids,
        names_to = "sample",
        values_to = "value"
      ) %>%
      dplyr::left_join(meta, by = c("sample"))
    if (method == 'mean') {
      mean_df <- df_long %>%
        dplyr::group_by(Protein.Ids, !!sym(condition_col)) %>%
        dplyr::summarise(
          mean_val = mean(value, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        tidyr::pivot_wider(
          names_from  = !!sym(condition_col),
          values_from = mean_val
        )
    }else{
      mean_df <- df_long %>%
        dplyr::group_by(Protein.Ids, !!sym(condition_col)) %>%
        dplyr::summarise(
          mean_val = median(value, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        tidyr::pivot_wider(
          names_from  = !!sym(condition_col),
          values_from = mean_val
        )
    }

    

    mean_mat <- as.data.frame(mean_df)
    rownames(mean_mat) <- mean_mat$Protein.Ids
    mean_mat$Protein.Ids <- NULL
    
    mean_mat <- mean_mat[gene_order, , drop = FALSE]
    
    return(as.matrix(mean_mat))
  }
}
get_group_means <- function(se,assay = 'conc',group = 'condition'){
  #this function is deprecated
  expr <- assay(se, "intensity")
  group <- colData(se)[[group]]
  
  group_mean <- t(apply(expr, 1, function(x) tapply(x, group, mean, na.rm = TRUE)))
  return(group_mean)
}


##Algorithm-----
detect_species_from_symbol <- function(gene) {
  gene <- as.character(gene)
  gene <- trimws(gene)
  gene <- gene[gene != ""]
  
  if (length(gene) == 0) return(NA)
  
  # 统计模式
  human_like <- sum(grepl("^[A-Z0-9]+$", gene))                
  mouse_like <- sum(grepl("^[A-Z][a-z0-9]+$", gene))          
  
  frac_human <- human_like / length(gene)
  frac_mouse <- mouse_like / length(gene)
  
  # 设定阈值
  if (frac_human > 0.7 && frac_human > frac_mouse) {
    return("human")
  } else if (frac_mouse > 0.7 && frac_mouse > frac_human) {
    return("mouse")
  } else {
    return("unknown")
  }
}
impute_low1pct_or_median_raw <- function(
    df,
    id_col        = "Protein.Ids",
    sample_col    = "sample",
    value_col     = "raw_value",
    condition_col = "condition",
    low_prob      = 0.01,   # 低端分位（1%）
    return_log2   = FALSE,  # 若 TRUE，返回 log2 标度
    seed          = 1
){
  stopifnot(all(c(id_col, sample_col, value_col, condition_col) %in% names(df)))
  set.seed(seed)
  
  res <- split(df, df[[condition_col]]) %>% lapply(function(dd){
    
    wide <- dd %>%
      select(all_of(c(id_col, sample_col, value_col))) %>%
      distinct() %>%
      tidyr::pivot_wider(
        names_from  = all_of(sample_col),
        values_from = all_of(value_col)
      ) %>%
      as.data.frame()
    
    rn  <- wide[[id_col]]
    mat <- as.matrix(wide[, setdiff(names(wide), id_col), drop = FALSE])
    rownames(mat) <- rn
    
    
    min_pos <- suppressWarnings(min(mat, na.rm = TRUE))
    shift   <- if (is.finite(min_pos) && min_pos > 0) 0 else (abs(min_pos) + 1)
    mat_log <- log2(mat + shift)
    
    # 每列低端分布
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
    miss_prop <- miss_cnt / pmax(n_reps, 1)
    
    for (i in seq_len(nrow(mat_log))) {
      x <- mat_log[i, ]
      na_idx <- is.na(x)
      
      if (!any(na_idx)) next
      
      if (miss_cnt[i] > n_reps / 2) {
        # 缺失 < 一半 → 每列低端抽样
        for (j in which(na_idx)) {
          q1 <- col_quant[j]; m1 <- col_mins[j]
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
      } else {
        # 缺失 ≥ 一半 → 几何平均数
        obs <- x[!na_idx]
        if (length(obs) == 0) {
          x[na_idx] <- cond_low_log
        } else {
          med <- geom_mean(obs, na.rm = TRUE)
          x[na_idx] <- med
        }
      }
      
      mat_log[i, ] <- x
    }
    
    # 反变换
    if (return_log2) {
      mat_out <- mat_log
    } else {
      mat_out <- pmax(2^mat_log - shift, 0)
    }
    
    
    wide_imputed <- data.frame(rn, mat_out, check.names = FALSE)
    names(wide_imputed)[1] <- id_col
    
    long_imputed <- wide_imputed %>%
      tidyr::pivot_longer(
        cols = -all_of(id_col),
        names_to  = sample_col,
        values_to = value_col
      ) %>%
      mutate(!!condition_col := unique(dd[[condition_col]])[1]) %>%
      select(all_of(c(id_col, sample_col, value_col, condition_col)))
    
    long_imputed
  })
  
  bind_rows(res) %>%
    arrange(.data[[id_col]], .data[[condition_col]], .data[[sample_col]])
}


limma_protein_DE <- function(expr,
                             group,
                             offset = NULL,
                             contrast_str = NULL,
                             trend = TRUE,
                             robust = TRUE) {
  stopifnot(is.matrix(expr) || is.data.frame(expr))
  expr <- as.matrix(expr)
  
  if (is.null(colnames(expr))) colnames(expr) <- paste0("S", seq_len(ncol(expr)))
  
  # 检查列和是否≈1e6（±10%容忍）
  col_sums <- colSums(expr, na.rm = TRUE)
  if (!all(is.finite(col_sums))) stop("发现非有限值，请先清洗数据。")
  if (!all(abs(col_sums - 1e6) <= 0.1 * 1e6)) {
    warning("有列的总和偏离 1e6（>10%），请确认是否已标准化到1e6。仍继续运行。")
  }
  
  # 自动选择 offset，保证 log2 安全
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
  contrast.mat <- makeContrasts(contrasts = contrast_str, levels = design)
  
  # 拟合 + eBayes
  fit <- lmFit(expr_log, design)
  fit2 <- contrasts.fit(fit, contrast.mat)
  fit2 <- eBayes(fit2, trend = trend, robust = robust)
  
  tt <- topTable(fit2, number = Inf, sort.by = "P")
  
  # 每组均值与 delta_mean（基于 log2 表达的组均值）
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
limma_protein_DE_pair <- function(expr,
                                  group,
                                  donor = NULL,
                                  offset = NULL,
                                  trend = TRUE,
                                  robust = TRUE,
                                  contrast_str = NULL) {
  

  stopifnot(is.matrix(expr) || is.data.frame(expr))
  expr <- as.matrix(expr)
  
  if (is.null(colnames(expr))) {
    colnames(expr) <- paste0("S", seq_len(ncol(expr)))
  }
  
  if (is.character(group)) group <- factor(group)
  group <- droplevels(group)
  stopifnot(length(group) == ncol(expr))
  stopifnot(nlevels(group) == 2)
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
    
    # 配对设计
    design <- model.matrix(~ donor + group)
    # (Intercept) donorX donorY ... groupB
  } else {
    # 不配对
    design <- model.matrix(~0 + group)
    # groupA groupB
  }
  
 
  if (is.null(contrast_str)) {
    
    if (!is.null(donor)) {
      # paired model, only one group column will appear:
      # e.g., "groupB"
      group_col <- grep("^group", colnames(design), value = TRUE)
      
      if (length(group_col) != 1) {
        stop("无法唯一确定 group 列，请检查 group 因子或 design 矩阵。")
      }
      
      contrast_str <- group_col   # e.g., "groupB"
      
    } else {
      # ~0 + group 模型有两列
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
    # unpaired case: A-B
    delta <- grp_means[, paste0("mean_log2_", parts[2])] -
      grp_means[, paste0("mean_log2_", parts[1])]
  } else if (length(parts) == 1 && grepl("^group", parts)) {
    # paired case: groupB
    # groupB = mean(B) - mean(A)
    ref <- lv[1]
    alt <- lv[2]
    delta <- grp_means[, paste0("mean_log2_", alt)] -
      grp_means[, paste0("mean_log2_", ref)]
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

auto_cluster_matrix_pca_one <- function(
    mtx,
    mode = c("row", "col"),  # 聚类方向
    n_pcs = 20,
    method_hclust = "ward.D2",
    deepSplit = 0,
    minClusterSize = 10,
    pamStage = TRUE
) {
  require(dynamicTreeCut)
  mode <- match.arg(mode)
  
  if (mode == "row") {
    message("🔹 Performing PCA-based clustering on rows (features)...")
    pca_res <- prcomp(mtx, center = FALSE, scale. = FALSE, rank. = n_pcs)
    d <- dist(pca_res$x)
    hc <- hclust(d, method = method_hclust)
    cl <- cutreeDynamic(
      dendro = hc,
      distM  = as.matrix(d),
      method = "hybrid",
      deepSplit = deepSplit,
      minClusterSize = minClusterSize,
      pamStage = pamStage
    )
  } else {
    
    d <- as.dist(1 - cor(mtx,method = 'spearman'))
    hc <- hclust(d, method = method_hclust)
    cl <- cutreeDynamic(
      dendro = hc,
      distM  = as.matrix(d),
      method = "hybrid",
      deepSplit = deepSplit,
      minClusterSize = 1,
      pamStage = pamStage
    )
  }
  
  
  
  if (mode == "row") {
    res_df <- data.frame(protein_group = rownames(mtx), km_cluster = paste0('km',cl))
  } else {
    res_df <- data.frame(protein_group = colnames(mtx), km_cluster = paste0('km',cl))
  }
  
  return(res_df)
}




enrichment_analysis <- function(
    gene,
    db            = c("GO", "KEGG"),
    species       = c("human", "mouse"),
    keyType       = "SYMBOL",
    qvalue_cutoff = 0.05
) {
  db <- match.arg(db)
  species <- match.arg(species)
  
  gene <- as.character(gene)
  gene <- trimws(gene)
  gene <- gsub("[^[:alnum:]_]", "", gene)
  gene <- gene[gene != ""]
  gene <- unique(gene)
  
  if (length(gene) == 0) {
    stop("No valid gene symbols found after cleaning input.")
  }
  
  withProgress(message = paste0("Running ", db, " enrichment for ", species, "..."), {
    library(clusterProfiler)
    
    # 选择物种数据库
    if (species == "human") {
      library(org.Hs.eg.db)
      OrgDb <- org.Hs.eg.db
      kegg_org <- "hsa"
    } else if (species == "mouse") {
      library(org.Mm.eg.db)
      OrgDb <- org.Mm.eg.db
      kegg_org <- "mmu"
    }
    
    # 运行 GO 或 KEGG
    if (db == "GO") {
      enriched <- enrichGO(
        gene          = gene,
        OrgDb         = OrgDb,
        keyType       = keyType,
        ont           = "ALL",
        pAdjustMethod = "BH",
        pvalueCutoff  = 0.05,
        qvalueCutoff  = qvalue_cutoff
      )
      
    } else if (db == "KEGG") {
      suppressMessages({
        gene_df <- bitr(gene, fromType = keyType, toType = "ENTREZID", OrgDb = OrgDb)
      })
      enriched <- enrichKEGG(
        gene          = gene_df$ENTREZID,
        organism      = kegg_org,
        pAdjustMethod = "BH",
        pvalueCutoff  = 0.05,
        qvalueCutoff  = qvalue_cutoff,
        use_internal_data = T
      ) %>% setReadable(OrgDb = org.Hs.eg.db, keyType="ENTREZID")
      
      
    }
  })
  
  return(enriched@result)
}
classify_timecourse_by_round <- function(
    mat,
    rho_cut = 0.75,
    min_len = 4,
    trim_side = c("best", "head", "tail") 
) {
  trim_side <- match.arg(trim_side)
  
  # ---- basic checks ----
  if (!is.matrix(mat)) mat <- as.matrix(mat)
  if (is.null(rownames(mat))) stop("mat 必须有 rownames 作为 gene 名称")
  if (ncol(mat) < min_len) stop("列数不足 min_len，无法开始分类")
  
  genes_all <- rownames(mat)
  group <- rep(NA_character_, length(genes_all))
  names(group) <- genes_all
  
  # 当前仍未被分组的 gene
  remaining <- genes_all
  
  # 当前使用的列索引（时间窗口）
  cols <- seq_len(ncol(mat))
  
  round_id <- 1L
  
  while (length(cols) >= min_len && length(remaining) > 0) {
    
    sub <- mat[remaining, cols, drop = FALSE]
    tvec <- seq_along(cols)
    
    # 对每个 gene(行)算 Spearman
    rho <- apply(sub, 1, function(x) suppressWarnings(cor(x, tvec, method = "spearman")))
    rho[is.na(rho)] <- 0  # NA 当作不显著，留到后续或最终 Uncertain
    
    up_genes   <- names(rho)[rho >=  rho_cut]
    down_genes <- names(rho)[rho <= -rho_cut]
    
    # 本轮打标签（只给尚未打过标签的）
    if (length(up_genes) > 0)   group[up_genes]   <- paste0("Up", round_id)
    if (length(down_genes) > 0) group[down_genes] <- paste0("Down", round_id)
    
    # 从后续轮次移除已分组的 gene
    newly_assigned <- union(up_genes, down_genes)
    remaining <- setdiff(remaining, newly_assigned)
    
    # 若列数已到 min_len，停止（不能再裁剪）
    if (length(cols) == min_len) break
    
    # ---- trim one sample (head/tail/best) ----
    if (trim_side == "head") {
      cols <- cols[-1]
    } else if (trim_side == "tail") {
      cols <- cols[-length(cols)]
    } else {
      # best: 计算去头/去尾后，“剩余未分组 gene”的整体单调性强度，选更强的一侧
      # 若 remaining 为空，随便去掉一个即可
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
          median(abs(rho2), na.rm = TRUE)  # 用中位数 abs(rho) 作全局评分
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



run_gsea <- function(
    df,
    gene_col      = "gene",
    stat_col      = "log2FC",      # 排名用数值，越大越靠前（如 log2FC / t 值 / signed -log10 p）
    db            = c("GO", "KEGG"),
    ont           = "ALL",         # 仅对 GO 生效：BP/CC/MF/ALL
    species       = c("human", "mouse"),
    keyType       = "SYMBOL",      # 你的 gene 名称类型
    qvalue_cutoff = 0.05,          # 按 qvalue 过滤
    minGSSize     = 10,
    maxGSSize     = 5000,
    pAdjustMethod = "BH",
    eps           = 1e-10,
    seed          = 123
){
  stopifnot(all(c(gene_col, stat_col) %in% colnames(df)))
  db      <- match.arg(db)
  species <- match.arg(species)
  
  suppressPackageStartupMessages({
    library(clusterProfiler)
    if (species == "human") {
      library(org.Hs.eg.db); OrgDb <- org.Hs.eg.db; kegg_org <- "hsa"
    } else {
      library(org.Mm.eg.db); OrgDb <- org.Mm.eg.db; kegg_org <- "mmu"
    }
  })
  
  set.seed(seed)
  df[[gene_col]] <-  gsub("[^[:alnum:]_]", "", df[[gene_col]])
  # 清洗并准备 rank 向量
  x <- df %>% 
    transmute(g = as.character(.data[[gene_col]]),
              s = as.numeric(.data[[stat_col]])) %>%
    filter(!is.na(g), g != "", !is.na(s)) %>%
    group_by(g) %>% slice_max(order_by = abs(s), n = 1, with_ties = FALSE) %>% ungroup()
  geneList <- x$s; names(geneList) <- x$g
  geneList <- sort(geneList, decreasing = TRUE)
  
  res <- NULL
  
  if (db == "GO") {
    # 直接按你提供的 keyType（常用 SYMBOL），qvalue 过滤在后
    res <- gseGO(
      geneList      = geneList,
      OrgDb         = OrgDb,
      keyType       = keyType,
      ont           = ont,
      minGSSize     = minGSSize,
      maxGSSize     = maxGSSize,
      pAdjustMethod = pAdjustMethod,
      pvalueCutoff  = 1,          # 放宽，后面按 qvalue 过滤
      eps           = eps,
      verbose       = FALSE
    )
  } else if (db == "KEGG") {
    # KEGG 需要 ENTREZID：必要时把 names(geneList) 转成 ENTREZID
    gl_entrez <- geneList
    if (keyType != "ENTREZID") {
      conv <- tryCatch(
        bitr(names(geneList), fromType = keyType, toType = "ENTREZID", OrgDb = OrgDb),
        error = function(e) NULL
      )
      if (is.null(conv) || !nrow(conv)) stop("ID 转换失败：无法获得 ENTREZID。")
      gl_entrez <- tibble(ID = names(geneList), s = as.numeric(geneList)) %>%
        inner_join(conv, by = setNames(keyType, "ID")) %>%
        group_by(ENTREZID) %>%
        summarize(s = s[which.max(abs(s))], .groups = "drop") %>%
        { setNames(.$s, .$ENTREZID) } %>%
        sort(decreasing = TRUE)
    } else {
      names(gl_entrez) <- as.character(names(gl_entrez))
    }
    
    res <- gseKEGG(
      geneList      = gl_entrez,
      organism      = kegg_org,
      minGSSize     = minGSSize,
      maxGSSize     = maxGSSize,
      pAdjustMethod = pAdjustMethod,
      pvalueCutoff  = 1,          # 放宽，后面按 qvalue 过滤
      eps           = eps,
      use_internal_data = T,
      verbose       = FALSE
    )
  }
  
  
  return(res)
}


##plot functions------
plot_gene_expression <- function(se, gene, by = "condition") {

  conc_mean_df <- assay(se, "conc") %>% 
    as.data.frame() %>%
    rownames_to_column('Protein.Ids') %>%
    tidyr::pivot_longer(
      cols = -Protein.Ids,
      names_to = "sample",
      values_to = "conc"
    ) %>%
    dplyr::left_join(as.data.frame(se@colData), by = "sample") %>%
    dplyr::left_join(as.data.frame(rowData(se)), by = "Protein.Ids")
  gene_name <- paste0("^", gene, "$")
  target_gene <- conc_mean_df %>%
    dplyr::filter(stringr::str_detect(Genes, gene_name))

  if (!by %in% colnames(se@colData)) {
    stop(paste0("Column '", by, "' not found in colData(se)"))
  }

  target_gene[[by]] <- factor(
    target_gene[[by]],
    levels = unique(se@colData[[by]])
  )
  group_unique <- unique(se@colData[[by]])
  if (length(group_unique) < ncol(se)) {

    p1 <- ggplot(target_gene, aes(x = .data[[by]], y = conc)) +
      geom_boxplot(outlier.size = 1, width = 0.3) +
      labs(x = NULL, y = "Expression") +
      ggtitle(gene) +
      theme_test() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12, face = "bold", color = "black"),
        axis.text.y = element_text(size = 12, face = "bold", color = "black"),
        axis.title.x = element_text(size = 14, face = "bold", color = "black"),
        axis.title.y = element_text(size = 14, face = "bold", color = "black")
      )
  } else {

    p1 <- ggplot(target_gene, aes(x = sample, y = conc)) +
      geom_col() +
      labs(x = NULL, y = "Expression") +
      ggtitle(gene) +
      theme_test() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12, face = "bold", color = "black"),
        axis.text.y = element_text(size = 12, face = "bold", color = "black"),
        axis.title.x = element_text(size = 14, face = "bold", color = "black"),
        axis.title.y = element_text(size = 14, face = "bold", color = "black")
      )
  }
  
  return(p1)
}

plot_pca <- function(pca.df, pca.res, colorby, label) {
  
  if (label == 'NULL') {
    ggplot(pca.df, aes(x = Dim.1, y = Dim.2)) +
      geom_point(aes_string(color = colorby), size = rel(2.5)) +
      labs(
        x = paste0("PC1 ", round(pca.res$eig[1, 2], 2), "%"),
        y = paste0("PC2 ", round(pca.res$eig[2, 2], 2), "%"),
        color = NULL, shape = NULL
      ) +
      theme_test() +
      #ggrepel::geom_text_repel(aes_string(label = label)) +
      theme(
        panel.grid = element_blank(),
        legend.position = "right",
        axis.text.x  = element_text(size = 12, face = "bold", color = "black"),
        axis.text.y  = element_text(size = 12, face = "bold", color = "black"),
        axis.title.x = element_text(size = 14, face = "bold", color = "black"),
        axis.title.y = element_text(size = 14, face = "bold", color = "black")
      )
  }else{
    ggplot(pca.df, aes(x = Dim.1, y = Dim.2)) +
      geom_point(aes_string(color = colorby), size = rel(2.5)) +
      labs(
        x = paste0("PC1 ", round(pca.res$eig[1, 2], 2), "%"),
        y = paste0("PC2 ", round(pca.res$eig[2, 2], 2), "%"),
        color = NULL, shape = NULL
      ) +
      theme_test() +
      ggrepel::geom_text_repel(aes_string(label = label)) +
      theme(
        panel.grid = element_blank(),
        legend.position = "right",
        axis.text.x  = element_text(size = 12, face = "bold", color = "black"),
        axis.text.y  = element_text(size = 12, face = "bold", color = "black"),
        axis.title.x = element_text(size = 14, face = "bold", color = "black"),
        axis.title.y = element_text(size = 14, face = "bold", color = "black")
      )
  }
  
}

plotSE_density <- function(se){
  rawdata <- se2raw(se)
  rawdata_df <- rawdata %>% 
    pivot_longer(cols = 3:length(rawdata),names_to = 'Sample',values_to = 'intersity')  %>%
    dplyr::filter(!is.na(intersity)) %>%
    mutate(log2intersity = log2(intersity) )
  rawdata_df$Sample <- factor(rawdata_df$Sample,levels = unique(colnames(se)))
  p <- ggplot(rawdata_df,aes(x = log2intersity,y = Sample,fill = Sample)) +
    geom_violin() + 
    geom_boxplot(width = 0.2,outlier.size = 0.5)  + 
    scale_color_manual(values = cols) +
    labs(x = 'log2intensity',y = NULL) + 
    guides(fill = 'none') +
    scale_y_discrete(limits = rev) +  
    theme_test() +
    theme(
      axis.text.x  = element_text(size = 12, face = "bold", color = "black"),
      axis.text.y  = element_text(size = 12, face = "bold", color = "black"),
      axis.title.x = element_text(size = 14, face = "bold", color = "black"),
      axis.title.y = element_text(size = 14, face = "bold", color = "black")
    )
  return(p)
}
#plotSE_density(se)
plotSE_missing_value <- function(se){
  rawdata <- se2raw(se)
  missing_number_df <- rawdata %>% 
    pivot_longer(cols = 3:length(rawdata),names_to = 'Sample',values_to = 'intersity')  %>%
    dplyr::filter(is.na(intersity)) %>%
    group_by(Sample) %>%
    mutate(missing_number = n()) %>%
    dplyr::select(Sample,missing_number) %>%
    distinct_all()
  missing_number_df$Sample <- factor(missing_number_df$Sample,levels = unique(colnames(se)))
  
  p <- ggplot(missing_number_df,aes(x = missing_number,y = Sample,fill = Sample)) +
    geom_segment(aes(x = 0,xend = missing_number),color = 'grey60') + 
    geom_point(aes(color = Sample), size=4 ) +
    labs(x = 'Number of missing values',y = NULL) + 
    geom_text(aes(label = missing_number,hjust = -0.5)) + 
    scale_x_continuous(expand = expansion(mult = c(0.05, 0.2))) + 
    guides(fill = 'none',color = 'none') +
    scale_y_discrete(limits = rev) +  
    theme_test() +
    theme(
      axis.text.x  = element_text(size = 12, face = "bold", color = "black"),
      axis.text.y  = element_text(size = 12, face = "bold", color = "black"),
      axis.title.x = element_text(size = 14, face = "bold", color = "black"),
      axis.title.y = element_text(size = 14, face = "bold", color = "black")
    )
  return(p)
}
#plotSE_missing_value(se)

plotSE_protein_number <- function(se){
  rawdata <- se2raw(se)
  protein_numberr_df <- rawdata %>% 
    pivot_longer(cols = 3:length(rawdata),names_to = 'Sample',values_to = 'intersity')  %>%
    dplyr::filter(!is.na(intersity)) %>%
    group_by(Sample) %>%
    mutate(protein_number = n()) %>%
    dplyr::select(Sample,protein_number) %>%
    distinct_all()
  protein_numberr_df$Sample <- factor(protein_numberr_df$Sample,levels = unique(colnames(se)))
  p <- ggplot(protein_numberr_df,aes(x = protein_number,y = Sample,fill = Sample)) +
    geom_segment(aes(x = 0,xend = protein_number),color = 'grey60') + 
    geom_point(aes(color = Sample), size=4 ) +
    labs(x = 'Number of detected proteins',y = NULL) + 
    geom_text(aes(label = protein_number,hjust = -0.5)) + 
    scale_x_continuous(expand = expansion(mult = c(0.05, 0.2))) + 
    guides(fill = 'none',color = 'none') +
    theme_test()  +
    scale_y_discrete(limits = rev) +  
    theme(
      axis.text.x  = element_text(size = 12, face = "bold", color = "black"),
      axis.text.y  = element_text(size = 12, face = "bold", color = "black"),
      axis.title.x = element_text(size = 14, face = "bold", color = "black"),
      axis.title.y = element_text(size = 14, face = "bold", color = "black")
    )
  return(p)
}
plotCV_density <- function(se){
  cv_by_condition <- calc_gene_CV_by_condition(se)
  if (is.null(cv_by_condition)) {
    showNotification('No replicates', type = "message")
    return(NULL)
  }else{
    cv_by_condition$condition <- factor(cv_by_condition$condition,levels = unique(cv_by_condition$condition))
    p <- ggplot(cv_by_condition,aes(CV,condition,,fill = condition)) +
      geom_violin() + 
      geom_boxplot(width = 0.2,outlier.size = 0.5)  + 
      scale_x_log10() + 
      scale_color_manual(values = cols) +
      labs(x = 'CV',y = NULL) + 
      guides(fill = 'none') +
      theme_test() +
      scale_y_discrete(limits = rev) +  
      theme(
        axis.text.x  = element_text(size = 12, face = "bold", color = "black"),
        axis.text.y  = element_text(size = 12, face = "bold", color = "black"),
        axis.title.x = element_text(size = 14, face = "bold", color = "black"),
        axis.title.y = element_text(size = 14, face = "bold", color = "black")
      )
    return(p)
  }
}

ivolcano <- function(
    data,
    logFC_col = "logFC",
    pval_col = "adj.P.Val",
    gene_col = "gene",
    title = "",
    interactive = TRUE,
    onclick_fun = NULL,
    pval_cutoff = 0.05,
    logFC_cutoff = 1,
    top_n = 10,
    label_mode = "separate",
    fontface = "italic",
    label_sig_only = TRUE,
    threshold_line = list(color = "black", linetype = "dashed", linewidth = 0.5),
    sig_colors = c(Up = "red", Down = "blue", Not_Significant = "grey70"),
    size_by = "none"
) {
  stopifnot(all(c(logFC_col, pval_col, gene_col) %in% colnames(data)))
  label_mode <- match.arg(label_mode, c("all", "separate"))
  size_by <- match.arg(size_by, c("none", "negLogP", "absLogFC"))
  
  # prepare data
  df <- data[!is.na(data[[logFC_col]]) & !is.na(data[[pval_col]]), ]
  
  df <- df |>
    dplyr::mutate(
      negLogP = -log10(!!sym(pval_col)),
      sig = dplyr::case_when(
        !!sym(pval_col) < pval_cutoff & !!sym(logFC_col) > logFC_cutoff ~ "Up",
        !!sym(pval_col) < pval_cutoff & !!sym(logFC_col) < -logFC_cutoff ~
          "Down",
        TRUE ~ "Not_Significant"
      )
    )
  df$sig <- factor(df$sig, levels = c("Up", "Down", "Not_Significant"))
  df$tooltip <- sprintf(
    "Gene: %s\nlogFC: %s\nadj.P.val: %s",
    df[[gene_col]],
    signif(df[[logFC_col]], 3),
    signif(df[[pval_col]], 3)
  )
  
  # onclick
  if (!is.null(onclick_fun)) {
    if (is.function(onclick_fun)) {
      df$onclick <- vapply(
        df[[gene_col]],
        function(g) {
          x <- onclick_fun(g)
          if (is.na(x) || !nzchar(x)) "" else as.character(x)
        },
        character(1)
      )
    } else {
      stop("onclick_fun must be a function or NULL")
    }
  } else {
    df$onclick <- ""
  }
  aes_args <- aes(
    x = !!sym(logFC_col),
    y = !!sym("negLogP"),
    color = !!sym("sig"),
    tooltip = paste0(
      gene_col,
      ": ",
      !!sym(gene_col),
      "\nlogFC: ",
      round(!!sym(logFC_col), 2),
      "\nFDR: ",
      signif(!!sym(pval_col), 3)
    ),
    data_id = !!sym(gene_col),
    onclick = !!sym("onclick")
  )
  if (!is.null(size_by) && size_by %in% names(df)) {
    aes_args$size <- df[[size_by]]
  }
  
  
  sig_counts <- table(df$sig)
  
  for (nm in names(sig_colors)) {
    if (!nm %in% names(sig_counts)) sig_counts[nm] <- 0
  }
  sig_counts <- sig_counts[names(sig_colors)]  # 保证顺序一致
  sig_labels <- sprintf("%s (n=%d)", names(sig_colors), sig_counts)
  
  p <- ggplot(df, aes_args) +
    {
      if (interactive) {
        ggiraph::geom_point_interactive(alpha = 0.7)
      } else {
        geom_point(alpha = 0.7)
      }
    } +
    scale_color_manual(
      values = sig_colors,
      labels = sig_labels,     # ✅ 图例标签带数量
      name = "Significance"    # 图例标题
    ) +
    labs(title = title, x = "log2 Fold Change", y = "-log10(FDR)") +
    theme_minimal()
  
  # threshold line
  p <- p +
    geom_hline(
      yintercept = -log10(pval_cutoff),
      color = threshold_line$color,
      linetype = threshold_line$linetype,
      linewidth = threshold_line$linewidth
    ) +
    geom_vline(
      xintercept = c(-logFC_cutoff, logFC_cutoff),
      color = threshold_line$color,
      linetype = threshold_line$linetype,
      linewidth = threshold_line$linewidth
    )
  
  # label topN genes
  if (top_n > 0) {
    df_label <- df
    if (label_sig_only) {
      df_label <- dplyr::filter(df, .data$sig != "Not_Significant")
    }
    if (label_mode == "separate") {
      df_label <- dplyr::group_by(df_label, sign(!!sym(logFC_col)))
    }
    df_label <- dplyr::slice_min(
      df_label,
      order_by = !!sym(pval_col),
      n = top_n
    )
    
    df_label$label <- df_label[[gene_col]]
    
    p <- p +
      ggrepel::geom_text_repel(
        data = df_label,
        aes(label = !!sym("label"), fontface = fontface),
        box.padding = 0.3,
        max.overlaps = 20
      )
  }
  return(
    list(
      ggiraph_obj =   ggiraph::girafe(ggobj = p, options = list(ggiraph::opts_hover(css = "fill:black;r:6"))),
      ggobj =  p
    )
    
  )
  
}

plot_GO_dot1 <- function(GO_df, topn = 10, label_format = 30){
  go_res <- GO_df %>%
    arrange(p.adjust) %>%                  # 显著性更强的在前（p.adjust 小）
    slice_head(n = topn) %>%
    mutate(neg10_p = -log10(p.adjust))
  p_GO <- ggdotchart(go_res, x = "Description", y = "neg10_p",
                     color = "Description",
                     sorting = "descending",
                     palette = if (exists("cols")) cols else NULL,
                     add = "segments",
                     rotate = TRUE,
                     dot.size = 3,
                     ggtheme = theme_test()) +
    labs(x = NULL, y = "-log10 adjusted p value") +
    guides(color = "none") +
    scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = label_format)) +
    theme(
      axis.text.x  = element_text(size = 12, face = "bold", color = "black"),
      axis.text.y  = element_text(size = 12, face = "bold", color = "black"),
      axis.title.x = element_text(size = 14, face = "bold", color = "black"),
      axis.title.y = element_text(size = 14, face = "bold", color = "black")
    )
  p_GO
}

plot_GO_dot2 <- function(GO_df, topn = 10, label_format = 30){
  go_res2 <- GO_df %>%
    arrange(p.adjust) %>%
    slice_head(n = topn) %>%
    mutate(
      neg10_p = -log10(p.adjust),
      gene_ratio = as.numeric(stringr::str_extract(GeneRatio, "\\d+")) /
        as.numeric(stringr::str_extract(GeneRatio, "\\d+$"))
    )
  ggplot(go_res2,
         aes(x = gene_ratio,
             y = reorder(Description, gene_ratio),
             color = p.adjust, size = Count)) +
    geom_point() +
    labs(x = "GeneRatio", y = NULL) +
    scale_color_gradient(low = "blue", high = "red", name = "p.adjust") +
    theme_test() +
    guides(size  = guide_legend(order = 2),
           color = guide_colorbar(order = 1)) +
    scale_y_discrete(labels = function(x) stringr::str_wrap(x, width = label_format)) +
    theme(
      axis.text.x  = element_text(size = 12, face = "bold", color = "black"),
      axis.text.y  = element_text(size = 12, face = "bold", color = "black"),
      axis.title.x = element_text(size = 14, face = "bold", color = "black"),
      axis.title.y = element_text(size = 14, face = "bold", color = "black")
    )
}

plot_GO_dot3 <- function(GO_df, topn = 10, label_format = 30){
  go_res2 <- GO_df %>%
    mutate(FDR = -log10(`false discovery rate`)) %>%
    arrange(desc(FDR)) %>%
    slice_head(n = topn) %>%
    mutate(
      Count = `genes mapped` 
    )
  p <- ggplot(go_res2,
              aes(x = `enrichment score`,
                  y = reorder(`term description`, `enrichment score`),
                  color = FDR, size = Count)) +
    geom_point() +
    labs(x = "GeneRatio", y = NULL) +
    scale_color_gradient(low = "blue", high = "red", name = "-log10(FDR)") +
    theme_test() +
    guides(size  = guide_legend(order = 2),
           color = guide_colorbar(order = 1)) +
    scale_y_discrete(labels = function(x) stringr::str_wrap(x, width = label_format)) +
    theme(
      axis.text.x  = element_text(size = 12, face = "bold", color = "black"),
      axis.text.y  = element_text(size = 12, face = "bold", color = "black"),
      axis.title.x = element_text(size = 14, face = "bold", color = "black"),
      axis.title.y = element_text(size = 14, face = "bold", color = "black")
    )
  return(p)
}
plot_heatmap_withline <- function(mat = NULL,
                                  row_split = NULL,
                                  #top_annotation = top_annotation,
                                  column_split =NULL,
                                  top_annotation = NULL,
                                  show_column_names = FALSE,
                                  ht.col.list = list(col_range = c(-2, 0, 2),
                                                     col_color = c("#08519C", "white", "#A50F15")),
                                  border = TRUE,
                                  line.size = 0.1,
                                  line.col = "grey90",
                                  mline.size = 2,
                                  mline.col = "#CC3333",
                                  set.md = "mean",
                                  textbox.pos = c(0.5, 0.8),
                                  textbox.size = 8,
                                  panel.arg = c(2, 0.25, 4, "grey90", NA),
                                  ...) {
  
  ComplexHeatmap::ht_opt(message = FALSE)
  
  
  
  subgroup <- factor(row_split)
  cluster.num <- nlevels(subgroup)
  
  
  clustcol <-c("OrangeRed","SlateBlue3","DarkOrange","GreenYellow","Purple","DarkSlateGray","Gold","DarkGreen","DeepPink2","Red4","#4682B4","#FFDAB9","#708090","#836FFF","#CDC673","#CD9B1D","#FF6EB4","#CDB5CD","#008B8B","#43CD80","#483D8B","#66CD00","#CDC673","#CDAD00","#CD9B9B","#FF8247","#8B7355","#8B3A62","#68228B","#CDB7B5","#CD853F","#6B8E23","#696969","#7B68EE","#9F79EE","#B0C4DE","#7A378B","#66CDAA","#EEE8AA","#00FF00","#EEA2AD","#A0522D","#000080","#E9967A","#00CDCD","#8B4500","#DDA0DD","#EE9572","#EEE9E9","#8B1A1A","#8B8378","#EE9A49","#EECFA1","#8B4726","#8B8878","#EEB4B4","#C1CDCD","#8B7500","#0000FF","#EEEED1","#4F94CD","#6E8B3D","#B0E2FF","#76EE00","#A2B5CD","#548B54","#BBFFFF","#B4EEB4","#00C5CD","#008B8B","#7FFFD4","#8EE5EE","#43CD80","#68838B","#00FF00","#B9D3EE","#9ACD32","#00688B","#FFEC8B","#1C86EE","#CDCD00","#473C8B","#FFB90F","#EED5D2","#CD5555","#CDC9A5","#FFE7BA","#FFDAB9","#CD661D","#CDC5BF","#FF8C69","#8A2BE2","#CD8500","#B03060","#FF6347","#FF7F50","#CD0000","#F4A460","#FFB5C5","#DAA520","#CD6889","#32CD32","#FF00FF","#2E8B57","#CD96CD","#48D1CC","#9B30FF","#1E90FF","#CDB5CD","#191970","#E8E8E8","#FFDAB9")
  colanno <- sample(clustcol,cluster.num)
  names(colanno) <- levels(subgroup)
  
  anno.block <- ComplexHeatmap::anno_block(align_to = split(1:nrow(mat), subgroup),
                                           panel_fun = function(index, nm) {
                                             npos = as.numeric(nm)
                                             grid::grid.rect(gp = grid::gpar(fill = colanno[npos], col = NA))
                                             grid::grid.text(label = paste("n:", length(index), sep = ''),
                                                             rot = 90,
                                                             gp = grid::gpar(col = "white", fontsize = 8))
                                           },
                                           which = "row")
  panel_fun = function(index, nm) {
    grid::pushViewport(grid::viewport(xscale = c(0, 1), yscale = c(0, 1), clip = "on"))
    grid::grid.rect()
    
    tmpmat <- mat[index, , drop = FALSE]
    
    if (!is.null(column_split)) {
      col_groups <- factor(column_split)
      group_levels <- levels(col_groups)
      
      # 计算每个组的均值
      means <- sapply(group_levels, function(g) {
        cols <- which(col_groups == g)
        vals <- as.numeric(tmpmat[, cols])
        if (set.md == "mean") {
          mean(vals, na.rm = TRUE)
        } else {
          median(vals, na.rm = TRUE)
        }
      })
      
      x_coords <- seq_along(means)
      from_range <- range(means, na.rm = TRUE)
      
      # ✅ 缩放到 0.05–0.95，并强制不越界
      clip_range <- function(x) pmin(pmax(x, 0.05), 0.95)
      y_mean <- clip_range(scales::rescale(means, to = c(0.05, 0.95), from = from_range))
      x_scaled <- clip_range(scales::rescale(x_coords, to = c(0.05, 0.95)))
      
      # ✅ 绘制均值线（用 NPC 单位确保不超 panel）
      grid::grid.lines(
        x = unit(x_scaled, "npc"),
        y = unit(y_mean, "npc"),
        gp = grid::gpar(lwd = mline.size, col = mline.col)
      )
      
    } else {
      mdia <- if (set.md == "mean") colMeans(tmpmat, na.rm = TRUE)
      else apply(tmpmat, 2, median, na.rm = TRUE)
      
      y <- scales::rescale(mdia, to = c(0.05, 0.95), from = range(mdia, na.rm = TRUE))
      grid::grid.lines(
        x = unit(scales::rescale(seq_along(mdia), to = c(0.05, 0.95)), "npc"),
        y = unit(y, "npc"),
        gp = grid::gpar(lwd = mline.size, col = mline.col)
      )
    }
    
    # ✅ 添加文字标签
    grid.textbox <- utils::getFromNamespace("grid.textbox", "ComplexHeatmap")
    text <- paste("Gene number:", length(index))
    grid.textbox(
      text,
      x = textbox.pos[1], y = textbox.pos[2],
      background_gp = gpar(col = NA, fill = 'transparent'),
      gp = grid::gpar(fontsize = textbox.size, fontface = "italic")
    )
    
    grid::popViewport()
  }
  
  
  
  
  
  right_annotation <- ComplexHeatmap::rowAnnotation(cluster = anno.block,
                                                    line = ComplexHeatmap::anno_link(align_to = subgroup,
                                                                                     which = "row",
                                                                                     panel_fun = panel_fun,
                                                                                     size = grid::unit(as.numeric(panel.arg[1]), "cm"),
                                                                                     gap = grid::unit(as.numeric(panel.arg[2]), "cm"),
                                                                                     width = grid::unit(as.numeric(panel.arg[3]), "cm"),
                                                                                     side = "right",
                                                                                     link_gp = grid::gpar(fill = panel.arg[4], col = panel.arg[5])))
  
  htf <- ComplexHeatmap::Heatmap(as.matrix(mat),
                                 name = "Z-score",
                                 cluster_rows  = T,
                                 cluster_columns = F,
                                 show_row_names = F,
                                 show_column_names = show_column_names,
                                 top_annotation = top_annotation,
                                 column_split = column_split,
                                 row_split = row_split,
                                 column_title = NULL,
                                 right_annotation = right_annotation,
                                 ...)
  return(htf)
}
plot_FC_trend <- function(se,group_by = 'condition'){
  
  logFC_long <- se2conc(se,group_by= group_by) %>% 
    dplyr::filter(type %in% c('Up','Down')) %>% 
    dplyr::select(
      Protein.Ids,
      gene_group,
      contains('log2FC')
    ) %>%
    pivot_longer(
      cols = -c('Protein.Ids','gene_group'),
      names_to = 'Sample',
      values_to = 'logFC'
    )
  
  
  levels_ordered <- logFC_long$gene_group |>
    unique() |>
    tibble::tibble(group = _) |>
    dplyr::mutate(
      round = as.integer(gsub("Up|Down", "", group)),
      type  = ifelse(grepl("^Up", group), "Up", "Down")
    ) |>
    dplyr::arrange(
      round,
      factor(type, levels = c("Up", "Down"))
    ) |>
    dplyr::pull(group)
  
  logFC_long$gene_group <- sapply(logFC_long$gene_group, as.character)
  logFC_long$gene_group <- factor(logFC_long$gene_group,levels = levels_ordered)
  logFC_long$Sample <- str_extract(logFC_long$Sample,'(?<=_).*')
  logFC_long$Sample <- factor(logFC_long$Sample,levels = unique(logFC_long$Sample))
  
  gene_n_df <- logFC_long %>%
    dplyr::select(Protein.Ids,gene_group) %>%
    distinct_all() %>%
    group_by(gene_group) %>%
    mutate(n = n()) %>%
    dplyr::select(gene_group,n) %>%
    distinct_all() 
  
  group_labels <- setNames(
    paste0(gene_n_df$gene_group, " (n = ", gene_n_df$n, ")"),
    gene_n_df$gene_group
  )
  
  p1 <- ggplot(logFC_long, aes(x = Sample, y = logFC)) +
    geom_line(
      aes(group = Protein.Ids),
      color = "grey80",
      alpha = 0.3
    ) +
    scale_y_continuous(limits = c(-1, 1)) + 
    stat_summary(
      aes(group = gene_group),
      fun = mean,
      geom = "line",
      color = "red",
      size = 1.4
    ) +
    facet_wrap(
      ~ gene_group,
      scales = "free_y",
      ncol = 2,
      labeller = labeller(gene_group = group_labels)
    ) +
    theme_test() +
    theme(
      strip.background = element_blank(),
      strip.text = element_text(face = "bold", size = 12),
      axis.text  = element_text(face = "bold"),
      axis.title = element_text(face = "bold")
    ) +
    labs(
      y = "logFC",
      x = NULL
    )
  
  return(p1)
}





#ui ----

timeFilterUI <- function(id) {
  ns <- NS(id)
  
  div(
    style = "border:1px solid #ddd; padding:15px; border-radius:8px; background:#fafafa;",
    
    h4( "Time-series parameters"),
  
    fluidRow(
      column(
        6,
        sliderInput(
          inputId = ns("time_threshold"),
          label   = "Time threshold (speaman cor)",
          min     = 0,
          max     = 1,
          value   = c(0.75),
          step    = 0.05
        )
      ),
      column(
        6,
        numericInput(
          inputId = ns("min_expression_threshold"),
          label   = "Minimum expression threshold",
          value   = 1,
          min     = 0,
          max     = 20,
          step    = 1
        )
      )
    ),
  
    fluidRow(
      column(
        6,
        sliderInput(
          inputId = ns("CV_with_time_threshold"),
          label   = "CV threshold",
          min     = 0,
          max     = 1,
          value   = 0.2,
          step    = 0.01
        )
      ),
      column(
        6,
        sliderInput(
          inputId = ns("padj_threshold"),
          label   = "Adjusted P-value threshold",
          min     = 0,
          max     = 0.1,
          value   = 0.05,
          step    = 0.001
        )
      )
    )
  )
}

#server logic------
make_download_se_qc_zip <- function(
    se_reactive,        # reactiveExpr: 返回 SummarizedExperiment 对象
    miss_gene,          # reactiveVal: missing_gene_rv
    unstable_gene,      # reactiveVal: un_stable_gene_rv
    input,              # Shiny input 对象
    file_input_id,      # 上传文件ID
    suffix = "_filtered"  # 输出文件后缀
) {
  downloadHandler(
    filename = function() {
      req(input[[file_input_id]]$name)
      base <- stringr::str_extract(basename(input[[file_input_id]]$name), ".*(?=_report)")
      paste0(base, suffix, ".zip")
    },
    content = function(file) {
      req(se_reactive(), input[[file_input_id]])
      
      se_obj <- se_reactive()
      miss_gene_df <- miss_gene()         # ✅ reactive 取值
      unstable_gene_df <- unstable_gene() # ✅ reactive 取值
      
      base_full <- tools::file_path_sans_ext(basename(input[[file_input_id]]$name))
      tmpdir <- tempfile("pack_")
      dir.create(tmpdir, showWarnings = FALSE, recursive = TRUE)
      
      # ---- 定义输出路径
      rds_path   <- file.path(tmpdir, paste0(base_full, "_se.Rds"))
      excel1_path <- file.path(tmpdir, paste0(base_full, "_imputed_intensity.xlsx"))
      excel2_path <- file.path(tmpdir, paste0(base_full, "_normalized_expression.xlsx"))
      excel3_path <- file.path(tmpdir, paste0(base_full, "_Zscaled_expression.xlsx"))
      excel4_path <- file.path(tmpdir, paste0(base_full, "_many_missing_value_gene.xlsx"))
      excel5_path <- file.path(tmpdir, paste0(base_full, "_unstable_gene.xlsx"))
      
      # ---- 写文件 
      saveRDS(se_obj, rds_path)
      writexl::write_xlsx(se2internstiy(se_obj), path = excel1_path)
      writexl::write_xlsx(se2conc(se_obj), path = excel2_path)
      writexl::write_xlsx(se2scale(se_obj), path = excel3_path)
      if (!is.null(miss_gene_df)) writexl::write_xlsx(miss_gene_df, path = excel4_path)
      if (!is.null(unstable_gene_df)) writexl::write_xlsx(unstable_gene_df, path = excel5_path)
      
      # ---- 打包 ZIP
      zip::zipr(
        zipfile = file,
        files = c(
          rds_path,
          excel1_path,
          excel2_path,
          excel3_path,
          excel4_path,
          excel5_path
        ),
        mode = "cherry-pick"
      )
    },
    contentType = "application/zip"
  )
}

make_download_se_zip <- function(
    se_reactive,        # reactiveExpr：返回 SummarizedExperiment 对象
    input,              # Shiny input 对象
    file_input_id,      # e.g. "se_file" —— 上传文件 ID，用来取原始文件名
    suffix = "_filtered"  # 文件名后缀（可选）
) {
  downloadHandler(
    filename = function() {
      req(input[[file_input_id]]$name)
      base <- str_extract(basename(input[[file_input_id]]$name), ".*(?=_report)")
      paste0(base, suffix, ".zip")
    },
    content = function(file) {
      req(se_reactive(), input[[file_input_id]])
      se_obj <- se_reactive()
      base_full <- tools::file_path_sans_ext(basename(input[[file_input_id]]$name))
      
      tmpdir <- tempfile("pack_")
      dir.create(tmpdir, showWarnings = FALSE, recursive = TRUE)
      
      # 临时文件路径
      rds_path   <- file.path(tmpdir, paste0(base_full, "_se.Rds"))
      excel1_path <- file.path(tmpdir, paste0(base_full, "_imputated_intensity.xlsx"))
      excel2_path <- file.path(tmpdir, paste0(base_full, "_normalized_expression.xlsx"))
      excel3_path <- file.path(tmpdir, paste0(base_full, "_Zscaled_expression.xlsx"))
      
      # 写文件
      saveRDS(se_obj, rds_path)
      writexl::write_xlsx(se2internstiy(se_obj), path = excel1_path)
      writexl::write_xlsx(se2conc(se_obj),       path = excel2_path)
      writexl::write_xlsx(se2scale(se_obj),      path = excel3_path)
      
      # 打包 ZIP
      zip::zipr(
        zipfile = file,
        files = c(rds_path, excel1_path, excel2_path, excel3_path),
        mode = "cherry-pick"
      )
    },
    contentType = "application/zip"
  )
}
make_download_time_se_zip <- function(
    se_reactive,       
    input,              
    file_input_id,      
    suffix = "_time_series"  
) {
  downloadHandler(
    filename = function() {
      req(input[[file_input_id]]$name)
      base <- str_extract(basename(input[[file_input_id]]$name), ".*(?=_report)")
      paste0(base, suffix, ".zip")
    },
    content = function(file) {
      req(se_reactive(), input[[file_input_id]])
      se_obj <- se_reactive()
      base_full <- tools::file_path_sans_ext(basename(input[[file_input_id]]$name))
      
      tmpdir <- tempfile("pack_")
      dir.create(tmpdir, showWarnings = FALSE, recursive = TRUE)
      
      # 临时文件路径
      rds_path   <- file.path(tmpdir, paste0(base_full, "_se.Rds"))
      #excel1_path <- file.path(tmpdir, paste0(base_full, "_imputated_intensity.xlsx"))
      excel2_path <- file.path(tmpdir, paste0(base_full, "_normalized_expression.xlsx"))
      excel3_path <- file.path(tmpdir, paste0(base_full, "_Zscaled_expression.xlsx"))
      
  
      saveRDS(se_obj, rds_path)
      #writexl::write_xlsx(se2internstiy(se_obj), path = excel1_path)
      writexl::write_xlsx(se2conc(se_obj),       path = excel2_path)
      writexl::write_xlsx(se2scale(se_obj),      path = excel3_path)
      
      # 打包 ZIP
      zip::zipr(
        zipfile = file,
        files = c(rds_path, excel2_path, excel3_path),
        mode = "cherry-pick"
      )
    },
    contentType = "application/zip"
  )
}
# make_grouped_select <- function(id, df, default_all = FALSE) {
#   
#   choices <- map(names(df), function(col) {
#     vals <- unique(df[[col]])
#     vals <- as.character(vals)
#     setNames(paste0(col, "||", vals), vals)
#   })
#   names(choices) <- names(df)
#   
#   # 去掉 rep / group
#   choices <- choices[!names(choices) %in% c("rep", "group")]
#   choices <- choices[c(2:length(choices),1)]
# 
#   default_selected <- NULL
#   if (default_all && length(choices) > 0) {
#     default_selected <- unname(unlist(choices[[1]]))  # 默认选中第一个分组全部选项
#   }
#   
#   
#   ui <- virtualSelectInput(
#     inputId = id,
#     label   = "选择一个列（单列多值）",
#     choices = choices,
#     multiple = TRUE,
#     search = TRUE,
#     selected = default_selected,
#     placeholder = "选择列及取值..."
#   )
#   
#   
#   server <- function(input, output, session) {
#     observeEvent(input[[id]], ignoreInit = TRUE, {
#       vals <- input[[id]]
#       if (is.null(vals) || length(vals) <= 1) return()
#       
#       groups <- sub("\\|\\|.*$", "", vals)
#       g0 <- groups[1]
#       keep <- vals[groups == g0]
#       
#       if (length(keep) != length(vals)) {
#         updateVirtualSelect(
#           inputId = id,
#           selected = keep
#         )
#       }
#     })
#   }
#   
#   list(ui = ui, server = server)
# }

make_grouped_select <- function(id, df, default_all = FALSE, max_levels = 1000) {
  

  valid_cols <- names(df)[
    sapply(df, function(x) {
      is.character(x) && length(unique(x)) <= max_levels
    })
  ]
  
  df <- df[, valid_cols, drop = FALSE]
  
  if (ncol(df) == 0) {
    ui <- helpText("No valid columns (character type with <= 1000 levels).")
    return(list(ui = ui, server = function(...) {}))
  }
  
  choices <- purrr::map(names(df), function(col) {
    vals <- unique(df[[col]])
    vals <- as.character(vals)
    setNames(paste0(col, "||", vals), vals)
  })
  names(choices) <- names(df)
  
  # 去掉 rep / group
  choices <- choices[!names(choices) %in% c("rep", "group")]
  
  if (length(choices) > 1) {
    choices <- choices[c(2:length(choices), 1)]
  }
  
  default_selected <- NULL
  if (default_all && length(choices) > 0) {
    default_selected <- unname(unlist(choices[[1]]))
  }
  
  ui <- virtualSelectInput(
    inputId = id,
    label   = "选择一个列",
    choices = choices,
    multiple = TRUE,
    search = TRUE,
    selected = default_selected,
    placeholder = "选择列及取值..."
  )
  
  server <- function(input, output, session) {
    observeEvent(input[[id]], ignoreInit = TRUE, {
      vals <- input[[id]]
      if (is.null(vals) || length(vals) <= 1) return()
      
      groups <- sub("\\|\\|.*$", "", vals)
      g0 <- groups[1]
      keep <- vals[groups == g0]
      
      if (length(keep) != length(vals)) {
        updateVirtualSelect(
          inputId = id,
          selected = keep
        )
      }
    })
  }
  
  list(ui = ui, server = server)
}

show_heatmap_param_modal <- function(se_data = NULL) {
  
  #all_cols <- if (!is.null(se_data)) colnames(se_data) else character(0)
  all_cols <- if (!is.null(se_data)) as.data.frame(colData(se_data)) else character(0)
  showModal(modalDialog(
    title = "Please set Heatmap parameters",

    # shinyWidgets::pickerInput(
    #   inputId = "selected_cols",
    #   label   = "Select columns to include in heatmap",
    #   choices = all_cols,
    #   selected = all_cols,
    #   multiple = TRUE,
    #   options = list(
    #     `actions-box` = TRUE,       # 全选/全不选按钮
    #     `live-search` = TRUE,       # 搜索功能
    #     size = 10                   # 下拉显示行数
    #   )
    # ),
    make_grouped_select("selected_cols", all_cols,default_all = T)$ui,
    
    selectInput(
      "row_k",
      "Row k-means clusters",
      choices = c("AUTO", 2:10),
      selected = "AUTO"
    ),
    
    radioButtons(
      "col_cluster_mode",
      "Column clustering mode",
      choices = c("K-means" = "kmeans", "Use colData group" = "coldata"),
      selected = "kmeans",
      inline = TRUE
    ),
    
    checkboxInput("enable_col_cluster", "Enable column clustering", value = TRUE),
    checkboxInput("show_col_names", "Display column names", value = FALSE),
    
    conditionalPanel(
      condition = "input.col_cluster_mode == 'kmeans'",
      selectInput(
        "col_k",
        "Column k-means clusters",
        choices = c("AUTO", 2:10),
        selected = "AUTO"
      )
    ),
    
    conditionalPanel(
      condition = "input.col_cluster_mode == 'coldata'",
      uiOutput("coldata_col_selector")
    ),
    selectInput(
      "top_annotaion_legend",
      "Select grouping column of top annotaion",
      choices = c('NULL',colnames(all_cols)),
      selected = "condition"
    ),
    sliderInput(
      inputId = "expr_min",
      label   = "Filter low-expression genes ",
      min     = 0,
      max     = 20,
      value   = 0,
      step    = 1
    ),
    sliderInput(
      inputId = "cv_min",
      label   = "Filter low-CV genes ",
      min     = 0,
      max     = 1,
      value   = 0.1,
      step    = 0.05
    ),
    sliderInput(
      inputId = "column_title_size",
      label   = "Fontsize of column title ",
      min     = 0,
      max     = 20,
      value   = 8,
      step    = 1
    ),
    numericInput("pdf_width", "PDF width (inch)", value = 7, min = 3, step = 1),
    numericInput("pdf_height", "PDF height (inch)", value = 7, min = 3, step = 1),
    
    footer = tagList(
      modalButton("Cancel"),
      actionButton("confirm_params", "Confirm")
    ),
    
    easyClose = TRUE
  ))
}
show_gene_selection_modal <- function(df) {
  showModal(
    modalDialog(
      title = "Select gene column and optional grouping",
      size = "l",
      easyClose = TRUE,
      
      tagList(
        selectInput(
          "gene_col",
          "Select the gene column:",
          choices  = colnames(df),
          selected = colnames(df)[1]
        ),
        selectInput(
          "group_col",
          "Select grouping column (optional):",
          choices  = c("None", colnames(df)),
          selected = "None"
        ),
        uiOutput("qc_group_value_ui")
      ),
      
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_gene_selection", "Confirm", class = "btn-primary")
      )
    )
  )
}
show_gene_selection_modal_go <- function(df) {
  showModal(
    modalDialog(
      title = "Select gene column and optional grouping",
      
      # 如果多于 1 列，提供选择列的选项
      if (ncol(df) > 1) {
        tagList(
          selectInput(
            "gene_col",
            "Select the gene column:",
            choices  = colnames(df),
            selected = colnames(df)[1]
          ),
          selectInput(
            "group_col",
            "Select grouping column (optional):",
            choices  = c("None", setdiff(colnames(df), grep("SYMBOL|GENE", colnames(df), value = TRUE))),
            selected = "None"
          ),
          uiOutput("group_value_ui")
        )
      } else {
        h4("Only one column detected — will use all genes in this column for enrichment.")
      },
      
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_gene_selection", "Confirm", class = "btn-primary")
      ),
      easyClose = TRUE
    )
  )
}
show_time_series_param <- function(se_data = NULL) {
  
  #all_cols <- if (!is.null(se_data)) colnames(se_data) else character(0)
  all_cols <- if (!is.null(se_data)) as.data.frame(colData(se_data)) else character(0)
  showModal(modalDialog(
    title = "Please set Heatmap parameters",
    
    # shinyWidgets::pickerInput(
    #   inputId = "selected_cols",
    #   label   = "Select columns to include in heatmap",
    #   choices = all_cols,
    #   selected = all_cols,
    #   multiple = TRUE,
    #   options = list(
    #     `actions-box` = TRUE,       # 全选/全不选按钮
    #     `live-search` = TRUE,       # 搜索功能
    #     size = 10                   # 下拉显示行数
    #   )
    # ),
    make_grouped_select("selected_cols", all_cols,default_all = T)$ui,
    
    selectInput(
      "coldata_col_selector",
      "Select grouping column from colData",
      choices = colnames(colData(se_data)),
      selected = "condition"
    ),
    timeFilterUI("time_filter"),
    footer = tagList(
      modalButton("Cancel"),
      actionButton("confirm_time_params", "Confirm")
    ),
    
    easyClose = TRUE
  ))
}

show_gene_selection_modal_gse_go <- function(df){
  showModal(
    modalDialog(
      title = "Select gene column and optional grouping",
      
      
      tagList(
        selectInput(
          "gse_GO_gene_col",
          "Select the gene column:",
          choices  = colnames(df),
          selected = colnames(df)[1]
        ),
        selectInput(
          "logFC_col",
          "Select logFC_col column:",
          choices  = colnames(df),
          selected = colnames(df)[2]
        )
      ),
      
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_ges_go_gene_selection", "Confirm", class = "btn-primary")
      ),
      easyClose = TRUE
    )
  )
}

string_post_tsv <- function(endpoint, query = list(), body = list()) {
  Sys.sleep(1)
  res <- httr::POST(
    url   = sprintf("%s/tsv/%s", STRING_BASE, endpoint),
    query = query,
    body  = body,
    encode = "form"
  )
  httr::stop_for_status(res)
  
  readr::read_tsv(
    httr::content(res, as = "text", encoding = "UTF-8"),
    show_col_types = FALSE
  )
}

pick_one_per_query <- function(idmap) {
  nms <- names(idmap)
  if ("queryItem" %in% nms) {
    idmap %>% group_by(.data$queryItem) %>% slice_head(n = 1) %>% ungroup()
  } else if ("queryIndex" %in% nms) {
    idmap %>% arrange(.data$queryIndex) %>%
      group_by(.data$queryIndex) %>% slice_head(n = 1) %>% ungroup()
  } else {
    idmap %>% distinct(.data$stringId, .keep_all = TRUE)
  }
}

##
show_se_selector_modal <- function(se_data) {
  col_df <- as.data.frame(colData(se_data))
  row_df <- as.data.frame(rowData(se_data))
  
  showModal(
    modalDialog(
      title = "Select samples and genes from SE",
      size = "l",
      
      h4("Subset samples in SE"),
      make_grouped_select("se_col_selector", col_df, TRUE)$ui,
      
      tags$hr(),
      h4("Option1:Subset genes in SE by gene group"),
      make_grouped_select("se_row_selector", row_df, TRUE)$ui,
      
      
      h4("Option2:Upload excel contains protein group"),
      fileInput(
        inputId = "se_excel_upload",
        label   = "Upload Excel file",
        accept  = c(
          ".xls",
          ".xlsx"
        )
      ),
      
      
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_se_index", "Confirm", class = "btn-primary")
      ),
      easyClose = TRUE
    )
  )
}