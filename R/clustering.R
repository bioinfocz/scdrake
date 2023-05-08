## -- Common functions related to cell clustering.

#' @param is_integration A logical scalar: if `TRUE`, clusterings will be named as `cluster_int_*`, otherwise `cluster_*`.
#' @name is_integration_param
NULL

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
#' [OSCA](https://bioconductor.org/books/3.15/OSCA.advanced/dimensionality-reduction-redux.html#using-the-technical-noise).
#' @param sce_pca A `SingleCellExperiment` object with calculated PCA.
#' @inheritParams bsparam_param
#' @inheritParams bpparam_param
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
#' @inheritParams bpparam_param
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

#' @title Compute shared nearest neighbors (SNN) graph.
#' @param sce A `SingleCellExperiment` object.
#' @param snn_k An integer scalar: number of shared nearest neighbors, passed to [scran::buildSNNGraph()]
#' @param snn_type A character scalar: type of weighting scheme to use for SNN, passed to [scran::buildSNNGraph()]
#' @param dimred A character scalar: name of matrix in `reducedDim()` used to calculate SNN.
#' @inheritParams bpparam_param
#' @return An object of class `igraph`. *Output target*: `graph_snn`
#'
#' @concept sc_clustering
#' @export
graph_snn_fn <- function(sce, snn_k, snn_type, dimred = "pca", BPPARAM = BiocParallel::SerialParam()) {
  graph_snn <- scran::buildSNNGraph(
    sce,
    k = snn_k,
    use.dimred = dimred,
    type = snn_type,
    BPPARAM = BPPARAM
  )

  igraph::V(graph_snn)$name <- colnames(sce)
  graph_snn
}

#' @title Find clusters in SNN graph using a community detection algorithm and if possible, using a specified resolution.
#' @param graph_snn (*input target*) An object of class `igraph`.
#' @inheritParams is_integration_param
#' @param algorithm A character scalar: community detection algorithm:
#'
#' - `louvain`: [igraph::cluster_louvain()]
#' - `walktrap`: [igraph::cluster_walktrap()]
#' - `leiden`: [igraph::cluster_leiden()]
#'
#' @param resolution A numeric scalar: resolution of the `algorithm` (not used in `walktrap`).
#'   Higher values result in more fine-grained clusters.
#' @return A `tibble` whose columns are mostly self-explanatory, except the `data` column, which is of `list` type and
#' contains an another `tibble` with `community_detection` column holding an object of class `communities` returned from
#' the used `igraph` clustering function.
#'
#' @concept sc_clustering
#' @export
run_graph_based_clustering <- function(graph_snn, is_integration, algorithm = c("louvain", "walktrap", "leiden"), resolution = 0.8) {
  algorithm <- arg_match(algorithm)

  if (algorithm == "louvain") {
    communities_object <- igraph::cluster_louvain(graph_snn, resolution = resolution)
  } else if (algorithm == "walktrap") {
    communities_object <- igraph::cluster_walktrap(graph_snn)
    resolution <- NA
  } else {
    communities_object <- igraph::cluster_leiden(graph_snn, resolution_parameter = resolution)
  }

  if (is_integration) {
    clustering_name_prefix <- "cluster_int_graph"
  } else {
    clustering_name_prefix <- "cluster_graph"
  }

  if (all(is.na(resolution))) {
    clustering_name <- glue("{clustering_name_prefix}_{algorithm}")
    subtitle <- glue("{stringr::str_to_sentence(algorithm)} algorithm")
  } else {
    clustering_name <- glue("{clustering_name_prefix}_{algorithm}_r{resolution}")
    subtitle <- glue("{stringr::str_to_sentence(algorithm)} algorithm, resolution = {resolution}")
  }

  cell_membership <- communities_object$membership %>%
    as.character() %>%
    factor() %>%
    set_names(igraph::V(graph_snn)$name)
  levels(cell_membership) <- stringr::str_sort(unique(cell_membership), numeric = TRUE)
  n_clusters <- cell_membership %>%
    unique() %>%
    length()
  cluster_table <- cells_per_cluster_table(cell_membership)

  data <- tibble::tibble(
    communities_object = list(.env$communities_object)
  )

  tibble::tibble(
    algorithm_category = "graph",
    algorithm = .env$algorithm,
    resolution = .env$resolution,
    cell_membership = list(.env$cell_membership) %>% set_names(clustering_name),
    n_clusters = .env$n_clusters,
    sce_column = .env$clustering_name,
    title = "Graph-based clustering",
    subtitle = .env$subtitle,
    cluster_table = list(.env$cluster_table),
    data = list(.env$data)
  )
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
    labs(title = glue("K-means best K ({best_k}) gap statistics"), x = "Number of clusters") +
    ggplot2::theme_bw()
}

#' @title Run k-means clustering for a specific `k`.
#' @param sce A `SingleCellExperiment` object.
#' @param kmeans_k An integer scalar: number of clusters for k-means.
#' @inheritParams is_integration_param
#' @param dimred A character scalar: name of matrix in `reducedDim()` used for k-means.
#' @param nstart,iter.max Passed to [stats::kmeans()].
#' @return A `tibble` whose columns are mostly self-explanatory, except the `data` column, which is of `list` type and
#' contains an another `tibble` with `kmeans_object` column holding an object of class `kmeans` returned from
#' the [stats::kmeans()] function.
#'
#' @concept sc_clustering
#' @export
run_kmeans_clustering <- function(sce, kmeans_k, is_integration, dimred = "pca", nstart = 25, iter.max = 1000) {
  kmeans_object <- stats::kmeans(reducedDim(sce, dimred), centers = kmeans_k, nstart = nstart, iter.max = iter.max)

  if (is_integration) {
    clustering_name <- glue("cluster_int_kmeans_k{kmeans_k}")
  } else {
    clustering_name <- glue("cluster_kmeans_k{kmeans_k}")
  }

  cell_membership <- kmeans_object$cluster %>%
    as.character() %>%
    factor() %>%
    set_names(colnames(sce))
  levels(cell_membership) <- stringr::str_sort(unique(cell_membership), numeric = TRUE)
  n_clusters <- cell_membership %>%
    unique() %>%
    length()
  cluster_table <- cells_per_cluster_table(cell_membership)

  data <- tibble::tibble(
    kmeans_object = list(.env$kmeans_object)
  )

  tibble::tibble(
    algorithm_category = "kmeans",
    algorithm = "k",
    k = .env$kmeans_k,
    cell_membership = list(.env$cell_membership) %>% set_names(clustering_name),
    n_clusters = .env$n_clusters,
    sce_column = .env$clustering_name,
    title = "K-means clustering",
    subtitle = glue("k = {kmeans_k}"),
    cluster_table = list(.env$cluster_table),
    data = list(.env$data)
  )
}

cluster_sce_sc3_bpparam_fn <- function(cluster_sc3_n_cores, seed) {
  if (cluster_sc3_n_cores == 1) {
    BiocParallel::SerialParam()
  } else {
    BiocParallel::SnowParam(workers = cluster_sc3_n_cores, type = "SOCK", RNGseed = seed, progressbar = TRUE)
  }
}

#' @title Run SC3 clustering for a specific `k`.
#' @description See [SC3::sc3()] for more details.
#' @param sce_pca A `SingleCellExperiment` object with calculated PCA.
#' @param sc3_k An integer vector: numbers of clusters for SC3.
#' @inheritParams is_integration_param
#' @inheritParams bpparam_param
#' @return A `SingleCellExperiment` object as returned from [SC3::sc3()].
#'
#' @details If `is_integration` is `TRUE`, `logcounts(sce_pca)` will be coerced to matrix.
#'
#' @concept sc_clustering
#' @export
calc_sc3 <- function(sce_pca, sc3_k, is_integration, BPPARAM = BiocParallel::SerialParam()) {
  if (is_integration && "integrated" %in% assayNames(sce_pca)) {
    logcounts(sce_pca) <- as.matrix(assay(sce_pca, "integrated"))
  }

  rowData(sce_pca)$feature_symbol <- rownames(sce_pca)
  counts(sce_pca) <- as.matrix(counts(sce_pca))
  logcounts(sce_pca) <- as.matrix(logcounts(sce_pca))

  if (check_sc3_version() == "github") {
    cli_alert_info("SC3: using {.val {BPPARAM$workers}} workers")
    cli_alert_info("The {.pkg SC3} package version from {.url github.com/gorgitko/SC3} will be used.")
    sce_sc3 <- SC3::sc3(sce_pca, ks = sc3_k, BPPARAM = BPPARAM)
  } else {
    cli_alert_info("SC3: using one worker")
    cli_alert_info("The original {.pkg SC3} package version from Bioconductor will be used.")
    sce_sc3 <- SC3::sc3(sce_pca, ks = sc3_k)
  }

  return(sce_sc3)
}

#' @title Create a dataframe of SC3 clusters for a selected number of clusters.
#' @description This function just extracts cell-cluster membership from the `cluster_sce_sc3` object and generates
#' a cluster stability plots.
#' @param cluster_sce_sc3 (*input target*) A `SingleCellExperiment` object with calculated SC3 clustering.
#' @param sc3_k A numeric vector: numbers of clusters for SC3.
#' @inheritParams is_integration_param
#' @return A `tibble` whose columns are mostly self-explanatory, except the `data` column, which is of `list` type and
#' contains an another `tibble` with `cluster_stability_plot` column holding a `ggplot` object returned from
#' [SC3::sc3_plot_cluster_stability()].
#'
#' @concept sc_clustering
#' @export
cluster_sc3_df_fn <- function(cluster_sce_sc3, sc3_k, is_integration) {
  purrr::map_dfr(sc3_k, function(k) {
    cluster_col <- glue("sc3_{k}_clusters")

    if (is_integration) {
      clustering_name <- glue("cluster_int_sc3_k{k}")
    } else {
      clustering_name <- glue("cluster_sc3_k{k}")
    }

    cell_membership <- colData(cluster_sce_sc3)[, cluster_col, drop = TRUE] %>%
      as.character() %>%
      factor() %>%
      set_names(colnames(cluster_sce_sc3))
    levels(cell_membership) <- stringr::str_sort(unique(cell_membership), numeric = TRUE)
    n_clusters <- cell_membership %>%
      unique() %>%
      length()
    cluster_table <- cells_per_cluster_table(cell_membership)

    cluster_stability_plot <- SC3::sc3_plot_cluster_stability(cluster_sce_sc3, k = k) +
        ggtitle("SC3 cluster stability", subtitle = glue("k = {k}"))

    data <- tibble::tibble(
      cluster_stability_plot = list(.env$cluster_stability_plot)
    )

    tibble::tibble(
      algorithm_category = "sc3",
      algorithm = "sc3",
      k = .env$k,
      cell_membership = list(.env$cell_membership) %>% set_names(clustering_name),
      n_clusters = .env$n_clusters,
      sce_column = .env$clustering_name,
      title = "SC3 clustering",
      subtitle = glue("k = {k}"),
      cluster_table = list(.env$cluster_table),
      data = list(.env$data)
    )
  })
}

#' @title Save SC3 cluster stability plots to a single PDF.
#' @param cluster_sc3_df (*input target*) A `tibble` returned from [cluster_sc3_df_fn()].
#' @param out_dir A character scalar: output directory.
#' @inheritParams is_integration_param
#' @return A character scalar: path to output PDF file. It will be `{out_dir}/cluster_sc3_stability_plots_k{k}.pdf` where `k`
#' are `k`s joined by `-`.
#'
#' @concept sc_clustering
#' @export
cluster_sc3_cluster_stability_plots_file_fn <- function(cluster_sc3_df, out_dir, is_integration) {
  out_file <- fs::path(out_dir, glue("cluster_sc3_stability_plots_k{k}.pdf", k = stringr::str_c(cluster_sc3_df$k, collapse = "-")))
  save_pdf(purrr::map(cluster_sc3_df$data, "cluster_stability_plot") %>% purrr::map(1), out_file)
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
