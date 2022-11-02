## -- Functions related to single-sample analysis / stage 02_norm_clustering.

filter_ensembl_multivals_closure <- function(sce) {
  row_names <- rownames(rowData(sce))

  filter_ensembl_multivals <- function(x) {
    if (length(x) > 1) {
      x <- x[x %in% row_names]
    }

    if (length(x) == 0) {
      x <- NA
    }

    return(x)
  }

  return(filter_ensembl_multivals)
}

#' @title Get dataframe of cell cycle genes.
#' @description This is a wrapper around data bundled in the `Seurat` package (`cc.genes.updated.2019`).
#' @param sce_final_input_qc (*input target*) A `SingleCellExperiment` object.
#' @param organism A character scalar: if `"mouse"`, gene symbols will be converted to sentence-case (e.g. "Gapdh").
#' @param annotation_db_file A character scalar: path to `AnnotationDbi` SQLite file.
#' @return A dataframe. *Output target*: `cc_genes`
#'
#' @concept single_sample_norm_clustering_fn
#' @export
cc_genes_fn <- function(sce_final_input_qc, organism, annotation_db_file) {
  ## -- TODO: Is there a better way to assign package data to a variable?
  e <- new.env()
  name <- utils::data("cc.genes.updated.2019", package = "Seurat", envir = e)[1]
  cc_genes <- get(name, envir = e)

  s_genes <- get("s.genes", cc_genes) %>%
    data.frame(SYMBOL = ., phase = "S")
  g2m_genes <- get("g2m.genes", cc_genes) %>%
    data.frame(SYMBOL = ., phase = "G2M")
  genes_df <- rbind(s_genes, g2m_genes)

  if (organism == "mouse") {
    genes_df$SYMBOL <- stringr::str_to_sentence(genes_df$SYMBOL)
  }

  genes_df <- genes_df %>%
    dplyr::mutate(
      ENSEMBL = with_dbi(
        annotation_db_file,
        AnnotationDbi::mapIds,
        keys = genes_df$SYMBOL,
        column = "ENSEMBL",
        keytype = "SYMBOL",
        multiVals = filter_ensembl_multivals_closure(sce_final_input_qc)
      )
    ) %>%
    dplyr::filter(!is.na(.data$ENSEMBL)) %>%
    set_rownames(.$ENSEMBL)

  return(genes_df)
}

#' @title Assign cell cycle phase to cells.
#' @description This is a wrapper around [Seurat::CellCycleScoring()].
#' @param sce_final_input_qc (*input target*) A `SingleCellExperiment` object.
#' @param cc_genes (*input target*) A dataframe.
#' @param data Passed to [Seurat::as.Seurat()].
#' @return A modified `sce_final_input_qc` objects with columns appended to `colData()`:
#'   `phase`, `s_score`, `g2m_score`, `cc_difference`. *Output target*: `sce_cc`
#'
#' @concept single_sample_norm_clustering_fn
#' @export
sce_cc_fn <- function(sce_final_input_qc, cc_genes, data = NULL) {
  seu <- Seurat::as.Seurat(sce_final_input_qc, data = data)
  ## -- CellCycleScoring() can fail if there are no cell cycle genes.
  seu_cc <- tryCatch(
   Seurat::CellCycleScoring(
    seu,
    s.features = cc_genes[cc_genes$phase == "S", "ENSEMBL"],
    g2m.features = cc_genes[cc_genes$phase == "G2M", "ENSEMBL"],
    set.ident = TRUE
    ),
    error = function(e) {
      cli_alert_warning(str_space(
        "{.code Seurat::CellCycleScoring()} failed, setting {.field phase}, {.field s_score}, {.field g2m_score} and",
        "{.field cc_difference} to {.val NA}."
      ))
      return(NULL)
    }
  )

  if (is_null(seu_cc)) {
    seu_cc <- seu
    seu_cc$phase <- NA
    seu_cc$s_score <- NA
    seu_cc$g2m_score <- NA
    ## -- NA minus NA is NA, but let's be explicit here.
    seu_cc$cc_difference <- NA
    sce_final_input_qc@metadata$cc_phase_failed <- TRUE
  } else {
    seu_cc$phase <- factor(seu_cc$Phase, levels = c("G1", "G2M", "S"))
    seu_cc$s_score <- seu_cc$S.Score
    seu_cc$g2m_score <- seu_cc$G2M.Score
    seu_cc$cc_difference <- seu_cc$s_score - seu_cc$g2m_score
    sce_final_input_qc@metadata$cc_phase_failed <- FALSE
  }

  assert_that(are_equal(rownames(seu_cc@meta.data), colnames(sce_final_input_qc)))

  sce_final_input_qc <- sce_add_colData(
    sce_final_input_qc,
    df = seu_cc@meta.data[, c("phase", "s_score", "g2m_score", "cc_difference")]
  )

  assert_that(are_equal(rownames(seu_cc@meta.data), colnames(sce_final_input_qc)))

  return(sce_final_input_qc)
}

#' @title Normalize counts either by `scran` or `sctransform`.
#' @param sce_cc (*input target*) A `SingleCellExperiment` object.
#' @param norm_type A character scalar: type of normalization.
#' @param ... Passed to [scran_normalization()] or [sctransform_normalization()].
#' @return A `SingleCellExperiment` object. `normalization_type = norm_type` is appended to `metadata()` of the returned
#' `SingleCellExperiment` object. *Output target*: `sce_norm`
#'
#' The following items are added to `metadata()` of the returned `SingleCellExperiment` object:
#' - `normalization_type`: value of the `norm_type` function argument.
#' - For `norm_type = "sctransform"`: `sctransform_hvg_ids`, `sctransform_pearson_residuals`, `sctransform_vst.out`,
#'   `sctransform_model_list`.
#'
#' @concept single_sample_norm_clustering_fn
#' @rdname sce_norm
#' @export
sce_norm_fn <- function(sce_cc, norm_type = c("scran", "sctransform", "none"), ...) {
  norm_type <- arg_match(norm_type)
  cli_alert_info("Normalization type: {.val {norm_type}}")

  if (norm_type == "scran") {
    scran_normalization(sce_cc, ...)
  } else if (norm_type == "sctransform") {
    sctransform_normalization(sce_cc, ...)
  } else {
    return(sce_cc)
  }
}

#' @param sce A `SingleCellExperiment` object.
#' @param use_quickcluster A logical scalar: if `TRUE`, do [scran::quickCluster()] prior to normalization.
#' @param quickcluster_method A character scalar: clustering method:
#' - `"igraph"` uses graph-based clustering
#' - `"hclust"` uses hierarchical clustering
#'
#' See `?scran::quickCluster` for more details.
#' @inheritParams bsparam_
#' @inheritParams bpparam_
#'
#' @rdname sce_norm
#' @export
scran_normalization <- function(sce, use_quickcluster = TRUE, quickcluster_method = c("igraph", "hclust"),
                                BSPARAM = BiocSingular::IrlbaParam(), BPPARAM = BiocParallel::SerialParam(),
                                ...) {
  if (use_quickcluster) {
    quickcluster_method <- arg_match(quickcluster_method)
    cluster_quickcluster <- scran::quickCluster(
      sce,
      method = quickcluster_method, BSPARAM = BSPARAM, BPPARAM = BPPARAM
    )
    sce$cluster_quickcluster <- factor(cluster_quickcluster)
  } else {
    cluster_quickcluster <- NULL
  }

  sce <- scran::computeSumFactors(sce, clusters = cluster_quickcluster, BPPARAM = BPPARAM)
  sce <- scater::logNormCounts(sce)
  sce <- sce_add_metadata(sce, normalization_type = "scran")

  return(sce)
}

#' @section `sctransform_normalization()`:
#' [Seurat::SCTransform()] is returning counts in log1p (natural log)
#' scale, but these are transformed to log2.
#' @param sce A `SingleCellExperiment` object.
#' @param vars_to_regress A list of character scalars: which variables to regress out during normalization.
#'   Passed to [Seurat::SCTransform()].
#' @param n_hvg An integer scalar: number of HVGs to take. Passed to [Seurat::SCTransform()].
#' @param method A character scalar: passed to [Seurat::SCTransform()].
#' @param seed An integer scalar: passed to [Seurat::SCTransform()].
#' @param verbose A logical scalar: passed to [Seurat::SCTransform()].
#'
#' @rdname sce_norm
#' @importFrom glmGamPoi glm_gp
#' @export
sctransform_normalization <- function(sce,
                                      vars_to_regress = NULL,
                                      n_hvg = 3000L,
                                      method = "glmGamPoi",
                                      seed = 1L,
                                      verbose = TRUE,
                                      ...) {
  seu <- as_seurat(sce, seu_assay = "RNA", add_rowData = TRUE, data = NULL)
  ## -- Prevent within-target future parallelism, it may break drake.
  seu_norm <- with_plan(
    Seurat::SCTransform(
      seu,
      vars.to.regress = vars_to_regress, variable.features.n = n_hvg, return.only.var.genes = FALSE,
      seed.use = seed, verbose = verbose, method = method
    ),
    future::sequential
  )

  sce_norm <- Seurat::as.SingleCellExperiment(seu_norm, assay = "SCT")
  mainExpName(sce_norm) <- NULL

  ## -- Convert log1p (natural log) to log2
  logcounts(sce_norm)[logcounts(sce_norm) != 0] <- logcounts(sce_norm)[logcounts(sce_norm) != 0] %>%
    expm1(.) %>%
    log2()
  rowData(sce_norm) <- cbind(rowData(sce)[rownames(sce_norm), ], rowData(sce_norm))

  sce_norm <- sce_add_metadata(
    sce_norm,
    normalization_type = "sctransform",
    sctransform_hvg_ids = Seurat::VariableFeatures(seu_norm),
    sctransform_pearson_residuals = Seurat::GetAssayData(seu_norm, "scale.data"),
    sctransform_vst.out = seu_norm@assays$SCT@misc$vst.out,
    sctransform_model_list = seu_norm@assays$SCT@SCTModel.list
  )

  return(sce_norm)
}

#' @title Find highly variable genes (HVGs).
#' @description Prior to HVG selection, cell cycle-related gene can be removed.
#' For more details see the HVG selection section in `02_norm_clustering.yaml` config.
#' @param sce_norm (*input target*) A `SingleCellExperiment` object.
#' @param hvg_selection_value,hvg_metric,hvg_selection Passed to [get_top_hvgs()] - see its help page.
#' @param hvg_rm_cc_genes A logical scalar: if `TRUE`, remove cell cycle-related genes exceeding the
#'   `hvg_cc_genes_var_expl_threshold` prior to HVG selection.
#' @param hvg_cc_genes_var_expl_threshold A numeric scalar: threshold for variance explained.
#'   Genes exceeding this threshold will be marked as CC-related.
#' @inheritParams bsparam_
#' @inheritParams bpparam_
#' @return A modified `sce_norm` object with added HVG data in `metadata()`.
#'   *Output target*: `sce_norm_hvg`
#'
#' The following items of `metadata(sce_norm)` are added or modified:
#' - `hvg_metric`, `hvg_selection`, `hvg_selection_value`, `hvg_rm_cc_genes`, `hvg_cc_genes_var_expl_threshold`:
#'   values passed to the function.
#' - `hvg_metric_fit`: either `DataFrame` returned from [scran::modelGeneVar()] or [scran::modelGeneCV2()],
#'   or `feature.attributes` dataframe returned from [Seurat::SCTransform()].
#' - Values added by [sce_remove_cc_genes()].
#' - `hvg_ids`: a character vector of HVG ENSEMBL IDs.
#'
#' @concept single_sample_norm_clustering_fn
#' @export
sce_norm_hvg_fn <- function(sce_norm,
                            hvg_selection_value,
                            hvg_metric = c("gene_var", "gene_cv2", "sctransform"),
                            hvg_selection = c("top", "significance", "threshold"),
                            hvg_rm_cc_genes = FALSE,
                            hvg_cc_genes_var_expl_threshold = 5,
                            BSPARAM = BiocSingular::IrlbaParam(),
                            BPPARAM = BiocParallel::SerialParam()) {
  hvg_metric <- arg_match(hvg_metric)
  hvg_selection <- arg_match(hvg_selection)

  if (sce_norm@metadata$normalization_type == "scran") {
    assert_that_(
      hvg_metric != "sctransform",
      msg = str_space(
        "{.field hvg_metric} {.val sctransform} can be used only if",
        "{.field {sce_norm@metadata$normalization_type} is {.val sctransform}"
      )
    )
  }

  sce_norm <- sce_add_metadata(
    sce_norm,
    hvg_metric = hvg_metric, hvg_selection = hvg_selection, hvg_selection_value = hvg_selection_value,
    hvg_rm_cc_genes = hvg_rm_cc_genes
  )

  if (hvg_metric == "gene_var") {
    hvg_metric_fit <- scran::modelGeneVar(sce_norm, BPPARAM = BPPARAM)
  } else if (hvg_metric == "gene_cv2") {
    hvg_metric_fit <- scran::modelGeneCV2(sce_norm, BPPARAM = BPPARAM)
  } else if (hvg_metric == "sctransform") {
    ## -- Now stored in metadata(sce_norm)$sctransform_model_list
    # row_data <- rowData(sce_norm) %>% as.data.frame()
    # sct_fit_cols <- stringr::str_detect(colnames(row_data), "^sct\\.")
    # hvg_metric_fit <- row_data[, sct_fit_cols]
    hvg_metric_fit <- metadata(sce_norm)$sctransform_model_list[[1]]@feature.attributes
  }

  assert_that(are_equal(rownames(sce_norm), rownames(hvg_metric_fit)))
  sce_norm <- sce_add_metadata(sce_norm, hvg_metric_fit = hvg_metric_fit)

  hvg_ids <- get_top_hvgs(
    sce_norm = sce_norm,
    hvg_metric_fit = hvg_metric_fit,
    hvg_selection_value = hvg_selection_value,
    hvg_metric = hvg_metric,
    hvg_selection = hvg_selection
  )

  if (length(hvg_ids) <= 100) {
    cli_alert_warning("Found a small number of HVGs ({length(hvg_ids)}). This may cause problems in downstream tasks, e.g. PCA.")
  }

  if (hvg_rm_cc_genes) {
    if (sce_norm@metadata$cc_phase_failed) {
      cli_alert_warning("Cannot remove cell cycle-related genes from HVGs: cell cycle phase could not have been estimated.")
      sce_norm@metadata$hvg_rm_cc_genes <- FALSE
      sce_norm <- sce_add_metadata(sce_norm, hvg_ids = hvg_ids)
    } else {
      sce_norm <- sce_remove_cc_genes(
        sce_norm,
        var_expl_threshold = hvg_cc_genes_var_expl_threshold,
        hvg_metric_fit = hvg_metric_fit,
        hvg_selection_value = hvg_selection_value,
        hvg_metric = hvg_metric,
        hvg_selection = hvg_selection
      ) %>%
        scater::runPCA(
          name = "pca_with_cc",
          subset_row = hvg_ids,
          BSPARAM = BSPARAM,
          BPPARAM = BPPARAM
        )
    }
  } else {
    sce_norm <- sce_add_metadata(sce_norm, hvg_ids = hvg_ids)
  }

  is_hvg <- rownames(sce_norm) %in% metadata(sce_norm)$hvg_ids
  rowData(sce_norm) <- cbind(rowData(sce_norm), is_hvg)

  return(sce_norm)
}

#' @title Remove cell doublets from a `SingleCellExperiment` object.
#' @param sce_norm_hvg (*input target*) A `SingleCellExperiment` object.
#' @param doublet_density (*input target*) A numeric vector.
#' @param max_doublet_score A numeric scalar: cells with doublet score (`log10(doublet_density + 1)`) exceeding
#'   this value will be marked as doublets and removed from `sce_norm_hvg`.
#'   If `NULL`, cells won't be filtered out.
#' @return A modified `sce_norm_hvg` object with removed cell doublets.
#'
#' The following items of `metadata(sce_norm_hvg)` are added or modified:
#' - `has_filtered_doublets`: `TRUE` if `max_doublet_score` is not `NULL`.
#' - `max_doublet_score`: the value of `max_doublet_score` function argument.
#'
#' @concept single_sample_norm_clustering_fn
#' @export
sce_rm_doublets_fn <- function(sce_norm_hvg, doublet_density, max_doublet_score) {
  doublet_score <- log10(doublet_density + 1)
  sce_norm_hvg$doublet_score <- doublet_score

  if (!is_null(max_doublet_score)) {
    cli_alert_info("Removing doublets from sce.")
    is_doublet <- doublet_score > max_doublet_score
    sce_norm_hvg <- sce_norm_hvg[, !is_doublet]
    sce_norm_hvg <- sce_add_metadata(sce_norm_hvg, has_filtered_doublets = TRUE, max_doublet_score = max_doublet_score)
  } else {
    is_doublet <- NA
    sce_norm_hvg <- sce_add_metadata(sce_norm_hvg, has_filtered_doublets = FALSE, max_doublet_score = NULL)
  }

  colData(sce_norm_hvg)$is_doublet <- is_doublet
  return(sce_norm_hvg)
}

#' @title Make dimred plots of PCA colored by cell cycle phase.
#' @description If HVG selection was performed with removal of CC-related genes, list of two plots will be returned -
#' one with PCA performed on all genes, and the second one without CC-related genes.
#' @param sce_pca (*input target*) A `SingleCellExperiment` object.
#' @return A list of one or two `ggplot2` objects. *Output target*: `pca_phase_plots`
#'
#' @concept single_sample_norm_clustering_fn
#' @export
pca_phase_plots_fn <- function(sce_pca) {
  if (sce_pca@metadata$cc_phase_failed) {
    return(create_dummy_plot("Cell cycle phases could not be estimated."))
  }

  pca_phase_plots <- list(
    plotReducedDim_mod(sce_pca, "pca", colour_by = "phase", title = "Cell cycle phases")
  )

  if (metadata(sce_pca)$hvg_rm_cc_genes && !is_empty(metadata(sce_pca)$hvg_rm_cc_genes_ids)) {
    pca_phase_plots <- c(
      pca_phase_plots,
      list(plotReducedDim_mod(sce_pca, "pca_with_cc", colour_by = "phase", title = "Cell cycle phases with included CC-related genes"))
    )
  }

  return(pca_phase_plots)
}

#' @title Make a grid of feature plots for selected genes.
#' @param sce_final_norm_clustering (*input target*) A `SingleCellExperiment` object.
#' @param selected_markers_df (*input target*) A tibble.
#' @return A tibble. *Output target*: `selected_markers_plots`
#'
#' @concept single_sample_norm_clustering_fn
#' @export
selected_markers_plots_fn <- function(sce_final_norm_clustering, selected_markers_df) {
  dimred_name <- selected_markers_df$dimred_name[[1]]

  p <- selected_markers_dimplot(
    sce = sce_final_norm_clustering,
    dimred = dimred_name,
    selected_markers_df = selected_markers_df,
    assay = "logcounts"
  )

  res <- tibble(dimred_name = dimred_name, plot = list(p))
  return(res)
}
