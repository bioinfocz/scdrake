## -- Common functions related to cell clustering.

#' @title Calculate PCA of a `SingleCellExperiment` object.
#' @description
#' Columns in the resulting PCA matrix will be named as `{name}_1`, `{name}_2`, ... .
#' This ensures compatibility with conversion to a `Seurat` object ([Seurat::as.Seurat.SingleCellExperiment()]).
#' @param sce A `SingleCellExperiment` object.
#' @param name,exprs_values,BSPARAM,BPPARAM,... Passed to [scater::runPCA()].
#' @param subset_row A character vector. If `NULL`, IDs of highly variable genes stored in `metadata(sce)$hvg_ids` will be used for
#'   the `subset_row` parameter in [scater::runPCA()]. If HVGs are not found, all genes will be used.
#' @return A `SingleCellExperiment` object with calculated PCA.
#'
#' @concept sc_clustering
#' @export
sce_calc_pca <- function(sce,
                         name = "pca",
                         exprs_values = "logcounts",
                         subset_row = NULL,
                         BSPARAM = BiocSingular::IrlbaParam(),
                         BPPARAM = BiocParallel::SerialParam(),
                         ...) {
  if (is_null(subset_row)) {
    hvg_ids <- metadata(sce)$hvg_ids

    if (is_null(hvg_ids)) {
      cli_alert_warning("metadata(sce)$hvg_ids in sce object is NULL -> using all features for PCA calculation")
    }
  }

  sce_pca <- scater::runPCA(
    sce,
    name = name, subset_row = hvg_ids, exprs_values = exprs_values, BSPARAM = BSPARAM, BPPARAM = BPPARAM, ...
  )
  colnames(reducedDim(sce_pca, name)) <- str_c(name, "_", seq(ncol(reducedDim(sce_pca, name))))
  return(sce_pca)
}

#' @title Get a number of PCs corresponding to biological variation.
#' @description This strategy is further described in
#' [OSCA](https://bioconductor.org/books/release/OSCA.advanced/dimensionality-reduction-redux.html#using-the-technical-noise).
#' @param sce_pca A `SingleCellExperiment` object with calculated PCA.
#' @inheritParams bsparam_
#' @inheritParams bpparam_
#' @return An integer scalar.
#'
#' @concept sc_clustering
#' @export
get_pca_gene_var_pcs <- function(sce_pca, BSPARAM = BiocSingular::IrlbaParam(), BPPARAM = BiocParallel::SerialParam()) {
  denoised_pcs <- scran::getDenoisedPCs(
    sce_pca,
    technical = scran::modelGeneVar(sce_pca),
    subset.row = metadata(sce_pca)$hvg_ids,
    BSPARAM = BSPARAM, BPPARAM = BPPARAM
  )

  return(ncol(denoised_pcs$components))
}

#' @title Get a final selection of number of PCs.
#' @description The number of PCs and the selection strategy is controlled:
#'
#' - For single-sample analysis, via `PCA_SELECTION_METHOD` and `PCA_FORCED_PCS` parameters in `02_norm_clustering.yaml`.
#' - For integration analysis, via `pca_selection_method` and `pca_forced_pcs` subparameters of `INTEGRATION_METHODS`
#'   parameter in `01_integration.yaml`.
#'
#' @param sce_pca A `SingleCellExperiment` object with calculated PCA.
#' @param pca_elbow_pcs A numeric scalar: number of PCs selected by elbow point strategy.
#' @param pca_gene_var_pcs A numeric scalar: number of PCs selected by technical noise strategy.
#' @param pca_selection_method A character scalar: final selection strategy.
#' @param pca_forced_pcs A numeric scalar: constant number of PCs defined in config file.
#' @return A modified `SingleCellExperiment` object:
#'
#' - The full PCA matrix in `"pca_all"` slot of `reducedDim()`.
#' - The subsetted PCA matrix in `"pca"` slot of `reducedDim()`.
#' - New items added to `metadata()`: `pca_selection_method`, `pca_selected_pcs`
#'
#' @concept sc_clustering
#' @export
get_pca_selected_pcs <- function(sce_pca,
                                 pca_elbow_pcs,
                                 pca_gene_var_pcs,
                                 pca_selection_method = c("elbow", "technical_noise", "forced"),
                                 pca_forced_pcs = NULL) {
  pca_selection_method <- arg_match(pca_selection_method)

  if (pca_selection_method == "elbow") {
    pca_selected_pcs <- pca_elbow_pcs
  } else if (pca_selection_method == "technical_noise") {
    pca_selected_pcs <- pca_gene_var_pcs
  } else if (pca_selection_method == "forced") {
    assert_that_(
      !is_null(pca_forced_pcs),
      msg = "{.var pca_forced_pcs} cannot be {.code NULL} when {.var pca_selection_method} is {.val forced}"
    )
    pca_selected_pcs <- pca_forced_pcs
  }

  reducedDim(sce_pca, "pca_all") <- reducedDim(sce_pca, "pca")
  reducedDim(sce_pca, "pca") <- reducedDim(sce_pca, "pca_all")[, seq_len(pca_selected_pcs)]
  sce_pca <- sce_add_metadata(sce_pca, pca_selection_method = pca_selection_method, pca_selected_pcs = pca_selected_pcs)
  return(sce_pca)
}

#' @title Make a plot showing numbers of selected PCs from all strategies.
#' @param pca_percent_var The percentage of variance explained by each PC.
#'   Obtained from `attr(reducedDim(sce_pca, "pca"), "percentVar")`.
#' @param pca_elbow_pcs A numeric scalar: number of PCs selected by elbow point strategy.
#' @param pca_gene_var_pcs A numeric scalar: number of PCs selected by technical noise strategy.
#' @param pca_forced_pcs A numeric scalar: constant number of PCs defined in config file.
#' @return A `ggplot` object.
#'
#' @concept sc_clustering
#' @export
make_pca_selected_pcs_plot <- function(pca_percent_var, pca_elbow_pcs, pca_gene_var_pcs, pca_forced_pcs) {
  df <- data.frame(
    PC = seq_along(pca_percent_var),
    pca_percent_var = pca_percent_var,
    xintercept = NA,
    line_color = NA
  )

  df[pca_elbow_pcs, c("xintercept", "line_color")] <- c(pca_elbow_pcs, "green")
  df[pca_gene_var_pcs, c("xintercept", "line_color")] <- c(pca_gene_var_pcs, "blue")

  line_color_levels <- c("green", "blue")

  if (!is_null(pca_forced_pcs)) {
    df[pca_forced_pcs, c("xintercept", "line_color")] <- c(pca_forced_pcs, "red")
    line_color_levels <- c(line_color_levels, "red")
  }

  df <- df %>%
    dplyr::mutate(
      xintercept = as.integer(.data$xintercept),
      line_color = factor(.data$line_color, levels = line_color_levels)
    )

  p <- ggplot(df) +
    ggplot2::geom_point(aes(x = .data$PC, y = .data$pca_percent_var)) +
    ggplot2::geom_vline(aes(xintercept = .data$xintercept, color = .data$line_color)) +
    ggplot2::scale_color_identity(
      name = "# PCs selection method",
      breaks = c("green", "blue", "red"),
      labels = c(
        glue("Elbow point: {pca_elbow_pcs} PCs"),
        glue("Technical variance: {pca_gene_var_pcs} PCs"),
        if (is_null(pca_forced_pcs)) "" else glue("Forced: {pca_forced_pcs} PCs")
      ),
      na.translate = FALSE,
      drop = FALSE,
      guide = "legend"
    ) +
    labs(y = "Variance explained (%)") +
    ggplot2::theme_bw()

  return(p)
}

#' @title Compute t-SNE and UMAP dimreds on a `SingleCellExperiment` object.
#' @param sce_pca_selected_pcs A `SingleCellExperiment` object with calculated PCA and known number of selected PCs.
#' @param tsne_perp A numeric scalar: t-SNE perplexity.
#' @param tsne_max_iter A numeric scalar: number of t-SNE iterations.
#' @param dimred A character scalar: name of matrix in `reducedDim()` used to calculate the dimreds.
#' @inheritParams bpparam_
#' @return A `SingleCellExperiment` object with calculated t-SNE and UMAP dimreds. Column names of matrices of these
#' dimreds will be named `tsne_<i>` and `umap_<i>`, respectively.
#'
#' @concept sc_clustering
#' @export
sce_compute_dimreds <- function(sce_pca_selected_pcs, tsne_perp, tsne_max_iter, dimred = "pca",
                                BPPARAM = BiocParallel::SerialParam()) {
  selected_pcs <- metadata(sce_pca_selected_pcs)$pca_selected_pcs
  assert_that_(
    !is_null(selected_pcs),
    msg = "{.code metadata(sce_pca_selected_pcs)$pca_selected_pcs} is {.code NULL} -> have you run {.code get_pca_selected_pcs()} on this sce object?"
  )

  sce_dimred <- scater::runUMAP(
    sce_pca_selected_pcs,
    name = "umap",
    dimred = dimred,
    n_dimred = selected_pcs,
    BPPARAM = BPPARAM
  )
  colnames(reducedDim(sce_dimred, "umap")) <- str_c("umap_", seq(ncol(reducedDim(sce_dimred, "umap"))))

  sce_dimred <- scater::runTSNE(
    sce_dimred,
    name = "tsne",
    dimred = dimred,
    n_dimred = selected_pcs,
    perplexity = tsne_perp,
    max_iter = tsne_max_iter,
    BPPARAM = BPPARAM
  )
  colnames(reducedDim(sce_dimred, "tsne")) <- str_c("tsne_", seq(ncol(reducedDim(sce_dimred, "tsne"))))

  return(sce_dimred)
}

#' @title Make a plot of k-means gaps.
#' @description
#' For more details see [this](https://bioconductor.org/books/3.12/OSCA/clustering.html#base-implementation)
#' chapter in OSCA.
#' @param kmeans_gaps An output from [cluster::clusGap()].
#' @param best_k A numeric scalar: best k for k-means calculated by [cluster::maxSE()].
#' @return A `ggplot` object.
#'
#' @concept sc_clustering
#' @export
make_kmeans_gaps_plot <- function(kmeans_gaps, best_k) {
  ggplot(as.data.frame(kmeans_gaps$Tab), aes(x = seq_along(.data$gap), y = .data$gap)) +
    ggplot2::geom_point(size = 4) +
    ggplot2::geom_vline(xintercept = best_k, color = "red") +
    labs(x = "Number of clusters") +
    ggplot2::theme_bw()
}

#' @title Create a named list of k-means clusters for a selected number of clusters.
#' @description "kc" = "k custom" :)
#' @param sce_pca A `SingleCellExperiment` object with calculated PCA and known number of selected PCs.
#' @param kmeans_k A numeric vector: numbers of clusters for k-means.
#' @param integration A logical scalar: if `TRUE`, a different names in the returned list will be used.
#' @return A named list. The names will be:
#' - For single-sample analysis (`integration = FALSE`): `cluster_kmeans_kc_<k>`
#' - For integration analysis (`integration = TRUE`): `cluster_int_kmeans_kc_<k>`
#'
#' where `<k>` depends on the `kmeans_k` function parameter.
#'
#' @concept sc_clustering
#' @export
cluster_kmeans_kc_fn <- function(sce_pca, kmeans_k, integration = FALSE) {
  if (integration) {
    clustering_names_new <- glue("cluster_int_kmeans_kc_{kmeans_k}")
  } else {
    clustering_names_new <- glue("cluster_kmeans_kc_{kmeans_k}")
  }

  lapply(kmeans_k, FUN = function(k) {
    stats::kmeans(reducedDim(sce_pca, "pca"), centers = k, nstart = 25, iter.max = 1e3)$cluster %>%
      factor()
  }) %>% set_names(clustering_names_new)
}

#' @title Calculate SC3 clustering.
#' @description See [SC3::sc3()] for details.
#' @param sce_pca A `SingleCellExperiment` object with calculated PCA and known number of selected PCs.
#' @param sc3_k A numeric vector: numbers of clusters for SC3.
#' @param sc3_dry A logical scalar: if `TRUE`, SC3 won't be run, but random cell-cluster assignments will be made.
#'   Used for testing as SC3 is very time-consuming.
#' @param integration A logical scalar: if `TRUE`, `logcounts()` will be coerced to matrix.
#' @inheritParams bpparam_
#' @return
#' - If `sc3_dry = FALSE`: a `SingleCellExperiment` object as returned from [SC3::sc3()].
#' - If `sc3_dry = TRUE`: a `SingleCellExperiment` object with random cell-cluster assignments mimicking output from
#'   [SC3::sc3()].
#'
#' The `sc3_dry` item is added to `metadata()` of the returned `SingleCellExperiment` object.
#'
#' @concept sc_clustering
#' @export
calc_sc3 <- function(sce_pca, sc3_k, sc3_dry = FALSE, integration = FALSE, BPPARAM = BiocParallel::SerialParam()) {
  if (sc3_dry) {
    sce_sc3 <- sce_pca
    for (k in sc3_k) {
      cluster_col <- glue("sc3_{k}_clusters")
      colData(sce_sc3)[, cluster_col] <- sample.int(k, ncol(sce_sc3), replace = TRUE)
    }
  } else {
    if (integration && "integrated" %in% assayNames(sce_pca)) {
      logcounts(sce_pca) <- as.matrix(assay(sce_pca, "integrated"))
    }

    rowData(sce_pca)$feature_symbol <- rownames(sce_pca)
    counts(sce_pca) <- as.matrix(counts(sce_pca))
    logcounts(sce_pca) <- as.matrix(logcounts(sce_pca))
    sce_sc3 <- SC3::sc3(sce_pca, ks = sc3_k, BPPARAM = BPPARAM)
  }

  sce_sc3 <- sce_add_metadata(sce_sc3, sc3_dry = sc3_dry)

  return(sce_sc3)
}

#' @title Create a named list of SC3 clusters for a selected number of clusters.
#' @param sce_sc3 A `SingleCellExperiment` object with calculated SC3 clustering.
#' @param sc3_k A numeric vector: numbers of clusters for SC3.
#' @param integration A logical scalar: if `TRUE`, a different names in the returned list will be used.
#' @return A named list. The names will be:
#' - For single-sample analysis (`integration = FALSE`): `cluster_sc3_<k>`
#' - For integration analysis (`integration = TRUE`): `cluster_int_sc3_<k>`
#'
#' where `<k>` depends on the `sc3_k` function parameter.
#'
#' @concept sc_clustering
#' @export
cluster_sc3_fn <- function(sce_sc3, sc3_k, integration = FALSE) {
  cluster_cols <- glue("sc3_{sc3_k}_clusters")
  if (integration) {
    clustering_names_new <- glue("cluster_int_sc3_{sc3_k}")
  } else {
    clustering_names_new <- glue("cluster_sc3_{sc3_k}")
  }

  colData(sce_sc3)[, cluster_cols] %>%
    as.list() %>%
    set_names(clustering_names_new) %>%
    purrr::map(factor)
}

#' @title Create a list of plots with SC3 clustering stability.
#' @description For more details see [SC3::sc3_plot_cluster_stability()].
#'
#' `metadata()` in `sce_sc3` are checked for `sc3_dry` item; if its value is `TRUE`, than SC3 clustering was run
#' in dry mode, producing random cell-cluster assignments, and thus, blank plots will be produced.
#' @param sce_sc3 A `SingleCellExperiment` object with calculated SC3 clustering.
#' @param cluster_sc3 A named list of SC3 cell-cluster assignments, as produced by [cluster_sc3_fn()].
#' @param sc3_k A numeric vector: numbers of clusters for SC3.
#' @return A named list of `ggplot` objects. The names are the same as in the `cluster_sc3` function parameter.
#'
#' @concept sc_clustering
#' @export
make_sc3_stability_plots <- function(sce_sc3, cluster_sc3, sc3_k) {
  sc3_dry <- metadata(sce_sc3)$sc3_dry %||% FALSE

  lapply(sc3_k, FUN = function(k) {
    if (sc3_dry) {
      p <- create_dummy_plot("SC3 was run in dry mode.")
    } else {
      p <- SC3::sc3_plot_cluster_stability(sce_sc3, k = k) +
        ggtitle("SC3 cluster stability", subtitle = glue("k = {k}"))
    }

    return(p)
  }) %>% set_names(names(cluster_sc3))
}

#' @title Get a number of unique clusters.
#' @param clustering An integer vector.
#' @return A number of unique clusters.
#'
#' @concept sc_clustering
#' @rdname clustering_helpers
#' @export
get_n_clusters <- function(clustering) {
  clustering %>%
    unique() %>%
    length()
}

#' @title Get a frequency table of cell-cluster assignments.
#' @description [janitor::tabyl()] is used underhood.
#' @param clusters An integer vector or dataframe (in that case the first column is taken as vector).
#' @param var_name A character scalar: name of the first column in the returned dataframe.
#' @return A dataframe with frequency of cell-cluster assignments.
#'
#' @examples
#' cells_per_cluster_table(rep(1:3, each = 2))
#' @concept sc_clustering
#' @export
cells_per_cluster_table <- function(clusters, var_name = "Cluster") {
  if (inherits(clusters, "data.frame")) {
    clusters <- clusters[, 1]
  }

  res <- janitor::tabyl(clusters) %>%
    janitor::adorn_pct_formatting(rounding = "half up", digits = 0) %>%
    as.data.frame()

  if (!is_null(var_name)) {
    colnames(res)[1] <- var_name
  }

  return(res)
}
