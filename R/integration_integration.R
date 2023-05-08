#' @title Import `sce_final_norm_clustering` targets from `drake` caches.
#' @description `SingleCellExperiment` objects are also checked for common HVG metric,
#' and HVG combination method is validated.
#' @param integration_sources A list of sources to load `sce_final_norm_clustering` targets from.
#'   See `01_integration.yaml` config for more details.
#' @param hvg_combination A character scalar: method to use for combining HVGs:
#'   - `"hvg_metric"`: combine gene variance or CV2,
#'     e.g. all samples had `HVG_METRIC` set to `"gene_var"`([scran::combineVar()]) or
#'     `"gene_cv2"` ([scran::combineCV2()]).
#'   - `"intersection"`: make intersection of HVGs.
#'   - `"union"`: make union of HVGs.
#'   - `"all"`: take all HVGs from all samples (without duplicates).
#' @return A list of `SingleCellExperiment` objects. *Output target*: `sce_int_import`
#'
#' The following items of `metadata()` of each imported `SingleCellExperiment` object are added or modified:
#' - `single_sample_path`, `single_sample_path_type`, `single_sample_name`, `single_sample_description`, `hvg_rm_cc_genes`,
#'   `hvg_cc_genes_var_expl_threshold`: taken from `INTEGRATION_SOURCES` config parameter.
#' - `hvg_combination`: the value of the `hvg_combination` function argument.
#'
#' @concept integration_integration_fn
#' @export
sce_int_import_fn <- function(integration_sources, hvg_combination = c("hvg_metric", "intersection", "union", "all")) {
  hvg_combination <- arg_match(hvg_combination)

  sce_int_import <- lapply(integration_sources, function(single_sample) {
    path <- single_sample$path
    assert_that_(
      fs::file_exists(path),
      msg = "The file or directory {.file {path}} for sample {.val {single_sample$name}} does not exist."
    )
    path_type <- single_sample$path_type
    sample_name <- single_sample$name

    if (path_type == "drake_cache") {
      cli_alert_info("Loading sample {.field {sample_name}} from {.pkg drake} cache {.file {path}}")
      sce <- drake::readd(sce_final_norm_clustering, path = path)
    } else if (path_type == "sce") {
      cli_alert_info("Loading sample {.field {sample_name}} from {.file {path}}")
      sce <- readRDS(path)
    } else {
      cli_abort("Unknown {.field path_type}: {.val {path_type}}. Only {.val drake_cache} and {.val sce} is allowed.")
    }

    sce <- sce %>%
      sce_add_metadata(
        single_sample_path = here(path),
        single_sample_path_type = path_type,
        single_sample_name = single_sample$name,
        single_sample_description = single_sample$description,
        hvg_rm_cc_genes = single_sample$hvg_rm_cc_genes
      ) %>%
      sce_add_colData(df = data.frame(batch = rep(single_sample$name, ncol(sce)) %>% factor()))

    ## -- Make unique sce colnames (barcodes).
    colnames(sce) <- str_c(colnames(sce), "-", single_sample$name)

    if (single_sample$hvg_rm_cc_genes) {
      sce <- sce_add_metadata(sce, hvg_cc_genes_var_expl_threshold = single_sample$hvg_cc_genes_var_expl_threshold)
    }

    return(sce)
  })

  common_normalization_type <- purrr::map_chr(sce_int_import, ~ metadata(.)$normalization_type) %>% unique()

  assert_that_(
    length(common_normalization_type) == 1,
    common_normalization_type == "scran",
    msg = str_space(
      "Integration is only possible when all single-samples have been normalized by {.code scran}",
      "({.var NORMALIZATION_TYPE}: {.val scran})."
    )
  )

  if (hvg_combination == "hvg_metric") {
    common_hvg_metric <- purrr::map_chr(sce_int_import, ~ metadata(.)$hvg_metric) %>% unique()

    assert_that_(
      length(common_hvg_metric) == 1,
      common_hvg_metric %in% c("gene_var", "gene_cv2"),
      msg = str_space(
        "{.field HVG_COMBINATION} is set to {.val hvg_metric} -> in order to combine HVG metrics (CV2, gene var),",
        "all single samples must be processed with the same {.field HVG_METRIC} parameter",
        "({.val gene_var} or {.val gene_cv2})."
      )
    )

    cli_alert_info("Combining HVGs: using {.val {common_hvg_metric}}.")
  } else {
    cli_alert_info("Combining HVGs: using HVG {.val {hvg_combination}}.")
  }

  sce_int_import <- lapply(sce_int_import, function(sce) {
    sce_add_metadata(sce, hvg_combination = hvg_combination)
  })

  return(sce_int_import)
}

#' @title Perform a fast shared nearest neighbor clustering of each sample.
#' @description Clustering is used for integration diagnostics.
#' @param sce_int_raw (*input target*) A named list of `SingleCellExperiment` objects.
#' @param snn_k An integer scalar: number of nearest neighbors. Passed to [scran::buildSNNGraph()].
#' @param snn_type A character scalar: type of weighting scheme to use for shared neighbors.
#'   Passed to [scran::buildSNNGraph()].
#' @param snn_clustering_method A character scalar: type of graph clustering method:
#' - `"walktrap"`: [igraph::cluster_walktrap()]
#' - `"louvain"`: [igraph::cluster_louvain()]
#' @inheritParams bpparam_param
#' @return A list of `SingleCellExperiment` objects. *Output target*: `sce_int_raw_snn_clustering`
#'
#' @concept integration_integration_fn
#' @export
sce_int_raw_snn_clustering_fn <- function(sce_int_raw, snn_k = 10L, snn_type = "rank",
                                          snn_clustering_method = c("walktrap", "louvain"),
                                          BPPARAM = BiocParallel::SerialParam()) {
  snn_clustering_method <- arg_match(snn_clustering_method)

  sce_int_raw_snn_clustering <- lapply(sce_int_raw, function(sce) {
    snn_graph <- scran::buildSNNGraph(sce, k = snn_k, use.dimred = "pca", type = snn_type, BPPARAM = BPPARAM)
    if (snn_clustering_method == "walktrap") {
      clusters <- igraph::cluster_walktrap(snn_graph)
    } else {
      clusters <- igraph::cluster_louvain(snn_graph)
    }

    sce$single_sample_cluster_snn <- factor(clusters$membership)
    return(sce)
  })

  return(sce_int_raw_snn_clustering)
}

#' @title Subset each object in a list of `SingleCellExperiment` objects to common data and their corresponding
#' metadata (e.g. HVGs).
#' @description This will subset all `SingleCellExperiment` objects to common:
#' - Features and HVGs (whose data are saved in metadata).
#' - Column data (`colData()`). Columns starting with `"cluster_"` will be renamed to `"cluster_<batch>"`,
#'   e.g. `"cluster_sc3_3_pbmc1k"`.
#'
#' @param sce_int_raw (*input target*) A named list of `SingleCellExperiment` objects.
#' @return A list of `SingleCellExperiment` objects. *Output target*: `sce_int_processed`
#'
#' The following items of `metadata()` of each `SingleCellExperiment` object are added or modified:
#' - `hvg_ids`, `hvg_metric_fit`: subsetted to common features.
#' - `n_features_orig`: an integer scalar: number of features (rows) before subsetting to common space.
#'
#' @concept integration_integration_fn
#' @export
sce_int_processed_fn <- function(sce_int_raw) {
  common_features <- Reduce(intersect, purrr::map(sce_int_raw, rownames))
  common_coldata <- Reduce(intersect, purrr::map(sce_int_raw, ~ colnames(colData(.))))
  common_clusterings <- stringr::str_subset(common_coldata, "^cluster_")
  common_cell_groupings <- lapply(sce_int_raw, function(sce) {
    cell_groupings <- metadata(sce)$cell_groupings
    if (!is_null(cell_groupings) && length(cell_groupings)) {
      return(names(cell_groupings))
    } else {
      return(character())
    }
  })

  common_cell_groupings <- Reduce(intersect, common_cell_groupings) %>%
    purrr::discard(is_empty)
  common_columns <- c(common_clusterings, common_cell_groupings) %>% unique()
  sce_int_processed <- lapply(sce_int_raw, function(sce) {
    n_features_orig <- nrow(sce)
    sce <- sce[common_features, ]
    colData(sce) <- colData(sce)[, common_coldata]

    for (common_column in common_columns) {
      sce[[common_column]] <- glue("{sce[[common_column]]}_{sce$batch}") %>% as.factor()
    }

    # metadata(sce)$hvg_metric_fit <- metadata(sce)$hvg_metric_fit[common_features, ]
    hvg_ids <- metadata(sce)$hvg_ids
    hvg_ids <- hvg_ids[hvg_ids %in% common_features]
    # metadata(sce)$hvg_ids <- hvg_ids
    sce <- sce_add_metadata(
      sce,
      n_features_orig = n_features_orig,
      hvg_ids = hvg_ids,
      hvg_metric_fit = metadata(sce)$hvg_metric_fit[common_features, ]
    )

    return(sce)
  })

  common_rowdata <- rowData(sce_int_processed[[1]])[, c("ENSEMBL", "SYMBOL", "ENTREZID", "GENENAME")]
  sce_int_processed <- lapply(sce_int_processed, function(sce) {
    assert_that(are_equal(rowData(sce) %>% rownames(), rownames(common_rowdata)))
    rowData(sce) <- common_rowdata
    return(sce)
  })

  return(sce_int_processed)
}

#' @title Make a tibble with integration methods and their parameters.
#' @param integration_methods A named list of integration methods.
#'   See the `INTEGRATION_METHODS` parameter in `01_integration.yaml` config for more details.
#' @param hvg_int (*input target*) A named list of HVG data for each `SingleCellExperiment` object.
#'   Depending on settings, it may have CC-related HVGs removed.
#' @param hvg_int_with_cc (*input target*) `NULL` or a named list of HVG data for each `SingleCellExperiment` object.
#'   Depending on settings, it may have all genes (including CC-related) included.
#' @return A `tibble`. *Output target*: `integration_methods_df`
#'
#' @concept integration_integration_fn
#' @export
integration_methods_df_fn <- function(integration_methods, hvg_int, hvg_int_with_cc = NULL) {
  integration_methods <- lapply(integration_methods, function(integration_method) {
    integration_params <- integration_method$integration_params

    if (is_null(integration_params)) {
      integration_params <- list(list())
    } else {
      integration_params <- list(integration_params)
    }

    integration_method$integration_params <- integration_params
    return(integration_method)
  })

  res <- lists_to_tibble(integration_methods) %>%
    dplyr::select(name, dplyr::everything())

  if (!is_null(hvg_int_with_cc)) {
    res <- res %>%
      tidyr::crossing(hvg_rm_cc_genes = c(TRUE, FALSE)) %>%
      dplyr::mutate(
        hvg_int = dplyr::if_else(hvg_rm_cc_genes, list(hvg_int), list(hvg_int_with_cc))
      )
  } else {
    res <- res %>%
      dplyr::mutate(hvg_rm_cc_genes = FALSE, hvg_int = list(hvg_int))
  }

  return(res)
}

#' @title Combine HVG data from a list of `SingleCellExperiment` objects.
#' @description This is, in fact, similar to [get_top_hvgs()]. The only difference is that HVGs are first combined
#' from all `SingleCellExperiment` objects in `sce_list` and consequently, top HVGs are selected.
#' @param sce_list A named list of `SingleCellExperiment` objects.
#' @param hvg_selection_value,hvg_selection See [get_top_hvgs()].
#' @param hvg_combination See [sce_int_import_fn()].
#' @param hvg_metric_fit_name A character scalar: name of a list item with HVG data in `metadata()`.
#' @return A named list of combined HVG data.
#'
#' @concept integration_integration_fn
#' @export
sce_int_combine_hvgs <- function(sce_list,
                                 hvg_selection_value = 0,
                                 hvg_metric_fit_name = "hvg_metric_fit",
                                 hvg_combination = c("hvg_metric", "intersection", "union", "all"),
                                 hvg_selection = c("top", "significance", "threshold")) {
  hvg_combination <- arg_match(hvg_combination)
  hvg_selection <- arg_match(hvg_selection)

  hvg_metrics <- purrr::map_chr(sce_list, ~ metadata(.)$hvg_metric)

  if (hvg_combination == "hvg_metric") {
    common_hvg_metric <- unique(hvg_metrics)
    hvg_metric_fits <- purrr::map(sce_list, ~ metadata(.)[[hvg_metric_fit_name]])
    common_features <- purrr::map(hvg_metric_fits, rownames) %>% Reduce(intersect, .)
    hvg_metric_fits <- purrr::map(hvg_metric_fits, ~ .[common_features, ])

    if (common_hvg_metric == "gene_var") {
      hvg_metric_fit_combined <- scran::combineVar(hvg_metric_fits)
    } else if (common_hvg_metric == "gene_cv2") {
      hvg_metric_fit_combined <- scran::combineCV2(hvg_metric_fits)
    } else {
      cli_abort("Unsupported HVG metric: {.val {common_hvg_metric}}")
    }

    hvg_ids <- get_top_hvgs(
      hvg_metric_fit = hvg_metric_fit_combined,
      hvg_selection = hvg_selection,
      hvg_selection_value = hvg_selection_value,
      hvg_metric = common_hvg_metric
    )
  } else {
    hvg_ids_list <- purrr::map(sce_list, ~ metadata(.)$hvg_ids)

    if (hvg_combination == "intersection") {
      hvg_ids <- Reduce(base::intersect, hvg_ids_list)
    } else if (hvg_combination == "union") {
      hvg_ids <- Reduce(base::union, hvg_ids_list)
    } else {
      hvg_ids <- metadata(sce_list[[1]])$hvg_metric_fit %>% rownames()
    }

    hvg_metric_fit_combined <- NULL
    hvg_selection <- NULL
    hvg_selection_value <- NULL
    common_hvg_metric <- NULL
  }

  return(list(
    hvg_ids = hvg_ids, hvg_metric_fit = hvg_metric_fit_combined, hvg_metric = common_hvg_metric,
    hvg_combination = hvg_combination, hvg_selection = hvg_selection, hvg_selection_value = hvg_selection_value
  ))
}

#' @title Get a named list of HVG data (gene IDs, fit, metadata).
#' @param sce_int_multibatchnorm (*input target*) A `SingleCellExperiment` object
#'   processed by [batchelor::multiBatchNorm()].
#' @param hvg_selection_value,hvg_combination,hvg_selection See [sce_int_combine_hvgs()].
#' @return A named list. *Output target*: `hvg_int_list`
#'
#' @concept integration_integration_fn
#' @export
hvg_int_list_fn <- function(sce_int_multibatchnorm,
                            hvg_selection_value,
                            hvg_combination = c("hvg_metric", "intersection", "union", "all"),
                            hvg_selection = c("top", "significance", "threshold")) {
  hvg_combination <- arg_match(hvg_combination)
  hvg_selection <- arg_match(hvg_selection)

  rm_cc_genes_any <- purrr::map_lgl(sce_int_multibatchnorm, ~ metadata(.)$hvg_rm_cc_genes) %>% any()

  if (rm_cc_genes_any) {
    sce_int_multibatchnorm <- lapply(sce_int_multibatchnorm, function(sce) {
      hvg_metric_fit <- metadata(sce)$hvg_metric_fit
      metadata(sce)$hvg_metric_fit_all <- hvg_metric_fit
      hvg_ids <- metadata(sce)$hvg_ids
      metadata(sce)$hvg_ids_all <- hvg_ids

      if (metadata(sce)$hvg_rm_cc_genes) {
        cli_alert_info("Sample {.val {metadata(sce)$single_sample_name}}: removing cell-cycle related genes from HVGs.")
        sce <- sce_remove_cc_genes(
          sce,
          hvg_selection_value = 0,
          var_expl_threshold = metadata(sce)$hvg_cc_genes_var_expl_threshold
        )

        hvg_rm_cc_genes_ids <- metadata(sce)$hvg_rm_cc_genes_ids
        metadata(sce)$hvg_metric_fit <- hvg_metric_fit[!rownames(hvg_metric_fit) %in% hvg_rm_cc_genes_ids, ]
        metadata(sce)$hvg_ids <- hvg_ids[!hvg_ids %in% hvg_rm_cc_genes_ids]
      }

      return(sce)
    })

    hvg_int_with_cc <- sce_int_combine_hvgs(
      sce_int_multibatchnorm,
      hvg_selection_value = hvg_selection_value,
      hvg_metric_fit_name = "hvg_metric_fit_all",
      hvg_combination = hvg_combination,
      hvg_selection = hvg_selection
    )
    hvg_int_with_cc$hvg_rm_cc_genes <- FALSE
  } else {
    hvg_int_with_cc <- NULL
  }

  hvg_int <- sce_int_combine_hvgs(
    sce_int_multibatchnorm,
    hvg_selection_value = hvg_selection_value,
    hvg_metric_fit_name = "hvg_metric_fit",
    hvg_combination = hvg_combination,
    hvg_selection = hvg_selection
  )

  if (rm_cc_genes_any) {
    hvg_int$hvg_rm_cc_genes <- TRUE
    hvg_int$hvg_rm_cc_genes_ids <- purrr::map(sce_int_multibatchnorm, ~ metadata(.)$hvg_rm_cc_genes_ids) %>% unlist()
  }

  return(list(hvg_int = hvg_int, hvg_int_with_cc = hvg_int_with_cc))
}

#' @title Perform integration of single-samples.
#' @param sce_int_multibatchnorm (*input target*) A `SingleCellExperiment` object
#'   processed by [batchelor::multiBatchNorm()].
#' @param integration_methods_df (*input target*) A tibble: transformed from `INTEGRATION_METHODS` parameter in
#'   `01_integration.yaml` config.
#' @inheritParams bpparam_param
#' @return A modified `integration_methods_df` tibble with appended integrated `SingleCellExperiment` object.
#'   *Output target*: `sce_int_df`
#'
#' The following items of `metadata()` of each `SingleCellExperiment` object are added or modified:
#' - `single_samples_metadata_df`: a tibble with information about individual samples:
#'   - `sample_name`, `description`, `cache_path`, `n_features`, `hvg_rm_cc_genes`, `hvg_cc_genes_var_expl_threshold`,
#'     `has_filtered_doublets`, `max_doublet_score`, `Samples`: vectors merged from `metadata()` of individual samples.
#'   - `n_cells`: number of cells.
#'   - `hvg_ids`, `hvg_rm_cc_genes_ids`: lists of character vectors of HVG / removed HVG IDs.
#'   - `cell_groupings`: list of cell grouping definitions.
#'   - `hvg_metric`: list of HVG metrics.
#' - `hvg_rm_cc_genes`, `int_method_name`, `int_param`: taken from integration method parameters.
#' - `hvg_rm_cc_genes_ids`: a character scalar of removed CC-related genes' IDs.
#'
#' @concept integration_integration_fn
#' @export
sce_int_df_fn <- function(sce_int_multibatchnorm, integration_methods_df, BPPARAM = BiocParallel::SerialParam()) {
  sce_int_df <- lapply_rows(integration_methods_df, FUN = function(par) {
    integration_params_other <- list()
    if (par$name == "uncorrected") {
      int_param_fn <- batchelor::NoCorrectParam
    } else if (par$name == "rescaling") {
      int_param_fn <- batchelor::RescaleParam
    } else if (par$name == "regression") {
      int_param_fn <- batchelor::RegressParam
      integration_params_other$BPPARAM <- BPPARAM
    } else if (par$name == "mnn") {
      int_param_fn <- batchelor::FastMnnParam
      integration_params_other$BPPARAM <- BPPARAM
    } else {
      cli_abort("Unknown name of the integration method: {.val {par$name}}")
    }

    int_param <- do.call(int_param_fn, args = c(par$integration_params, integration_params_other))
    sce_int <- batchelor::correctExperiments(
      sce_int_multibatchnorm,
      subset.row = par$hvg_int$hvg_ids, correct.all = TRUE, PARAM = int_param
    )
    assayNames(sce_int)[1] <- "integrated"

    single_samples_metadata_df <- tibble(
      sample_name = merge_sce_metadata(sce_int_multibatchnorm, "single_sample_name"),
      description = merge_sce_metadata(sce_int_multibatchnorm, "single_sample_description"),
      path = merge_sce_metadata(sce_int_multibatchnorm, "single_sample_path"),
      path_type = merge_sce_metadata(sce_int_multibatchnorm, "single_sample_path_type"),
      n_cells = purrr::map_int(sce_int_multibatchnorm, ncol),
      n_features = merge_sce_metadata(sce_int_multibatchnorm, "n_features_orig"),
      hvg_ids = merge_sce_metadata(sce_int_multibatchnorm, "hvg_ids", as_vector = FALSE),
      hvg_rm_cc_genes = merge_sce_metadata(sce_int_multibatchnorm, "hvg_rm_cc_genes"),
      hvg_cc_genes_var_expl_threshold = merge_sce_metadata(sce_int_multibatchnorm, "hvg_cc_genes_var_expl_threshold"),
      hvg_rm_cc_genes_ids = merge_sce_metadata(sce_int_multibatchnorm, "hvg_rm_cc_genes_ids", as_vector = FALSE),
      has_filtered_doublets = merge_sce_metadata(sce_int_multibatchnorm, "has_filtered_doublets"),
      max_doublet_score = merge_sce_metadata(sce_int_multibatchnorm, "max_doublet_score"),
      cell_groupings = merge_sce_metadata(sce_int_multibatchnorm, "cell_groupings", as_vector = FALSE),
      Samples = merge_sce_metadata(sce_int_multibatchnorm, "Samples")
    )

    if (par$hvg_int$hvg_combination == "hvg_metric") {
      single_samples_metadata_df$hvg_metric <- par$hvg_int$common_hvg_metric
    } else {
      single_samples_metadata_df$hvg_metric <- merge_sce_metadata(sce_int_multibatchnorm, "hvg_metric")
    }

    mdata <- c(
      par$hvg_int,
      list(
        single_samples_df = single_samples_metadata_df,
        hvg_rm_cc_genes = par$hvg_rm_cc_genes,
        hvg_rm_cc_genes_ids = unlist(single_samples_metadata_df$hvg_rm_cc_genes_ids),
        int_method_name = par$name,
        int_param = int_param
      )
    )

    mdata_to_rm <- c(
      "Samples",
      "hvg_rm_cc_genes", "hvg_cc_genes_var_expl_threshold", "hvg_without_cc_genes",
      "has_filtered_doublets", "max_doublet_score", "cell_groupings",
      "single_sample_path", "single_sample_path_type", "single_sample_name", "single_sample_description"
    )

    sce_int@metadata[names(metadata(sce_int)) %in% mdata_to_rm] <- NULL
    metadata(sce_int) <- utils::modifyList(metadata(sce_int), mdata, keep.null = TRUE)
    rowData(sce_int)$is_hvg <- rownames(sce_int) %in% par$hvg_int$hvg_ids
    sce_int$batch <- factor(sce_int$batch, levels = mdata$single_samples_df$sample_name)

    par$sce_int <- list(sce_int)
    return(par)
  })

  return(sce_int_df)
}

#' @title Compute PCA for each integration method result (`SingleCellExperiment` object).
#' @param pca_params_df (*input target*) A tibble: derived from `sce_int_df` and `integration_methods_df` targets.
#'   PCA parameters are defined for each integration method in `INTEGRATION_METHODS` parameter in
#'   `01_integration.yaml` config.
#' @inheritParams bpparam_param
#' @return A modified `pca_params_df` tibble with appended `SingleCellExperiment` object with computed PCA.
#'   *Output target*: `sce_int_pca_df`
#'
#' The following items of `metadata()` of each integrated `SingleCellExperiment` object are added or modified:
#' - `pca_selection_method`, `pca_selected_pcs`: taken from parameters of the integration method.
#'
#' @concept integration_integration_fn
#' @export
sce_int_pca_df_fn <- function(pca_params_df, BPPARAM = BiocParallel::SerialParam()) {
  sce_int_pca_df <- lapply_rows(pca_params_df, FUN = function(par) {
    if (par$pca_selection_method == "corrected") {
      assert_that_(
        "corrected" %in% reducedDimNames(par$sce_int),
        msg = str_space(
          "{.val corrected} was specified as {.code pca_selection_method} for the integration method",
          "{.val {par$name}}, but the reduced dim with this name was not found. This {.code pca_selection_method}",
          "is not possible for this integration method, or {.field d} parameter in {.field integration_params}",
          "was {.code NA}."
        )
      )
      corrected_pca <- reducedDim(par$sce_int, "corrected")
      reducedDim(par$sce_int, "pca") <- corrected_pca
      n_pcs <- ncol(corrected_pca)
      colnames(reducedDim(par$sce_int, "pca")) <- paste0("pca_", seq(n_pcs))
      colnames(reducedDim(par$sce_int, "corrected")) <- paste0("corrected_", seq(n_pcs))
      par$sce_int <- sce_add_metadata(par$sce_int, pca_selection_method = par$pca_selection_method, pca_selected_pcs = n_pcs)

      title <- glue("{n_pcs} PCs were used from multiBatchPCA() calculated during integration.")
      pca_selected_pcs_plot <- create_dummy_plot(title)

      par <- c(par, list(
        sce_pca = par$sce_int, pca_percent_var = NA, pca_elbow_pcs = NA, pca_gene_var_pcs = NA,
        pca_selected_pcs_plot = pca_selected_pcs_plot, pca_selected_pcs = n_pcs
      ))
    } else {
      sce_pca <- sce_calc_pca(par$sce_int, name = "pca", exprs_values = "integrated", BPPARAM = BPPARAM)
      pca_percent_var <- attr(reducedDim(sce_pca, "pca"), "percentVar")
      pca_elbow_pcs <- PCAtools::findElbowPoint(pca_percent_var)
      pca_gene_var_pcs <- get_pca_gene_var_pcs(sce_pca, BPPARAM = BPPARAM)

      title <- glue("PCs selection for integration method '{par$name}'")
      subtitle <- NULL
      if (par$hvg_rm_cc_genes) {
        subtitle <- glue("Removed cell-cycle related genes from HVGs.")
      }
      pca_selected_pcs_plot <- make_pca_selected_pcs_plot(
        pca_percent_var, pca_elbow_pcs, pca_gene_var_pcs,
        pca_forced_pcs = par$pca_forced_pcs
      ) +
        ggtitle(label = title, subtitle = subtitle)

      sce_pca <- get_pca_selected_pcs(
        sce_pca,
        pca_elbow_pcs = pca_elbow_pcs,
        pca_gene_var_pcs = pca_gene_var_pcs,
        pca_selection_method = par$pca_selection_method,
        pca_forced_pcs = par$pca_forced_pcs
      )

      par <- c(par, list(
        sce_pca = sce_pca, pca_percent_var = pca_percent_var, pca_elbow_pcs = pca_elbow_pcs,
        pca_gene_var_pcs = pca_gene_var_pcs, pca_selected_pcs_plot = pca_selected_pcs_plot,
        pca_selected_pcs = metadata(sce_pca)$pca_selected_pcs
      ))
    }

    par$sce_int <- NULL
    return(par)
  })

  return(sce_int_pca_df)
}

#' @title Calculate t-SNE and UMAP for each integration method result (`SingleCellExperiment` object).
#' @param dimred_params_df (*input target*) A tibble: derived from `sce_int_pca_df` and `integration_methods_df` targets.
#'   Dimred parameters are defined for each integration method in `INTEGRATION_METHODS` parameter
#'   in `01_integration.yaml` config.
#' @inheritParams bpparam_param
#' @return A modified `dimred_params_df` tibble with appended `SingleCellExperiment` object with computed PCA.
#'   *Output target*: `sce_int_dimred_df`
#'
#' @concept integration_integration_fn
#' @export
sce_int_dimred_df_fn <- function(dimred_params_df, BPPARAM = BiocParallel::SerialParam()) {
  sce_int_dimred_df <- lapply_rows(dimred_params_df, FUN = function(par) {
    par$sce_dimred <- sce_compute_dimreds(
      par$sce_pca,
      tsne_perp = par$tsne_perp, tsne_max_iter = par$tsne_max_iter, BPPARAM = BPPARAM
    )
    par$sce_pca <- NULL

    return(par)
  })

  return(sce_int_dimred_df)
}

#' @title Make a HVG plot for uncorrected `SingleCellExperiment` object (processed by [batchelor::multiBatchNorm()]).
#' @param sce_int_uncorrected (*input target*) A tibble.
#' @param ... Passed to [plot_hvg()].
#'
#' @return A modified `sce_int_uncorrected` with appended HVG plot (`ggplot2` object).
#'   *Output target*: `hvg_plot_int`
#'
#' @concept integration_integration_fn
#' @export
hvg_plot_int_fn <- function(sce_int_uncorrected, ...) {
  if (metadata(sce_int_uncorrected)$hvg_rm_cc_genes) {
    title <- "(removed cell cycle related genes)"
  } else {
    title <- ""
  }

  if (metadata(sce_int_uncorrected)$hvg_combination == "hvg_metric") {
    p <- plot_hvg(sce_int_uncorrected, ...) &
      patchwork::plot_annotation(title = glue("HVGs by combined {metadata(sce_int_uncorrected)$hvg_metric} {title}"))
  } else {
    n_hvgs <- metadata(sce_int_uncorrected)$hvg_ids %>% length()
    text <- glue(str_space(
      "Combined HVGs ({n_hvgs}) were selected by {metadata(sce_int_uncorrected)$hvg_combination}",
      "of individual sample's HVGs.\n{title}"
    ))
    p <- create_dummy_plot(text)
  }

  return(p)
}

#' @title Compute a quick graph-based clustering for each integration method.
#' @param sce_int_pca_df (*input target*) A tibble.
#' @param snn_k,snn_type,snn_clustering_method See [sce_int_raw_snn_clustering_fn()].
#' @inheritParams bpparam_param
#' @return A modified `sce_int_pca_df` with appended `SingleCellExperiment` object with computed clustering.
#'   *Output target*: `sce_int_clustering_df`
#'
#' The following items of `metadata()` of each integrated `SingleCellExperiment` object are added or modified:
#' - `int_cluster_snn_k`, `int_cluster_snn_type`, `int_cluster_snn_method`:
#'   taken from parameters of the integration method.
#'
#' @concept integration_integration_fn
#' @export
sce_int_clustering_df_fn <- function(sce_int_pca_df, snn_k = 10, snn_type = "rank",
                                     snn_clustering_method = c("walktrap", "louvain"),
                                     BPPARAM = BiocParallel::SerialParam()) {
  snn_clustering_method <- arg_match(snn_clustering_method)

  sce_int_clustering_df <- lapply_rows(sce_int_pca_df, FUN = function(par) {
    snn_graph <- scran::buildSNNGraph(par$sce_pca, k = snn_k, use.dimred = "pca", type = snn_type, BPPARAM = BPPARAM)
    if (snn_clustering_method == "walktrap") {
      clusters <- igraph::cluster_walktrap(snn_graph)
    } else {
      clusters <- igraph::cluster_louvain(snn_graph)
    }

    clusters <- factor(clusters$membership)
    snn_params <- list(
      int_cluster_snn_k = snn_k, int_cluster_snn_type = snn_type, int_cluster_snn_method = snn_clustering_method
    )

    par$sce_pca$int_cluster_snn <- clusters
    par$sce_pca <- do.call(sce_add_metadata, args = c(list(par$sce_pca), snn_params))
    return(c(
      list(name = par$name, hvg_rm_cc_genes = par$hvg_rm_cc_genes, sce_clustering = par$sce_pca),
      snn_params
    ))
  })

  return(sce_int_clustering_df)
}

#' @title Compute and make integration diagnostics and plots for each integration method.
#' @param sce_int_clustering_df (*input target*) A tibble.
#' @param sce_int_raw_snn_clustering (*input target*) A tibble.
#' @return A tibble. *Output target*: `int_diagnostics_df`
#'
#' @concept integration_integration_fn
#' @export
int_diagnostics_df_fn <- function(sce_int_clustering_df, sce_int_raw_snn_clustering) {
  int_diagnostics_df <- lapply_rows(sce_int_clustering_df, FUN = function(par) {
    sce_clustering <- par$sce_clustering

    tbl <- sce_clustering %>%
      colData() %>%
      as.data.frame() %>%
      dplyr::rename(cluster = .data$int_cluster_snn) %>%
      janitor::tabyl(.data$cluster, .data$batch)

    tbl_percentage <- tbl %>%
      janitor::adorn_totals("row") %>%
      janitor::adorn_percentages(denominator = "col") %>%
      janitor::adorn_pct_formatting() %>%
      janitor::adorn_ns()

    tbl_long <- tidyr::pivot_longer(tbl, -.data$cluster, names_to = "sample", values_to = "n_cells")
    title <- glue("Integration method: '{par$name}'")
    if (par$hvg_rm_cc_genes) {
      title <- glue("{title} (removed cell cycle-related genes from HVGs)")
    }

    plot_batch_cluster_absolute <- ggplot(tbl_long, aes(x = .data$cluster, y = .data$n_cells, fill = .data$sample)) +
      ggplot2::geom_bar(stat = "identity", position = ggplot2::position_dodge()) +
      ggplot2::theme_minimal() +
      labs(title = title, subtitle = "Number of cells from each batch per cluster.", x = "Cluster", y = "Number of cells")

    plot_batch_cluster_ratio <- ggplot(tbl_long, aes(x = .data$cluster, y = .data$n_cells, fill = .data$sample)) +
      ggplot2::geom_bar(stat = "identity", position = "fill") +
      ggplot2::theme_minimal() +
      labs(title = title, subtitle = "Ratio of cells from each batch per cluster.", x = "Cluster", y = "Ratio of cells")

    tbl_base <- table(cluster = sce_clustering$int_cluster_snn, Batch = sce_clustering$batch) %>% unclass()
    norm <- scater::normalizeCounts(tbl_base, pseudo.count = 10)
    rv <- MatrixGenerics::rowVars(norm)
    df_var <- tibble::tibble(
      as.data.frame(tbl_base) %>%
        tibble::rownames_to_column("cluster"), var = rv
    ) %>%
      dplyr::arrange(-rv)

    clusters <- as.integer(sce_clustering$int_cluster_snn)
    batch_names <- levels(sce_clustering$batch)

    rand_indices <- lapply(batch_names, function(batch) {
      sce_raw <- sce_int_raw_snn_clustering[[batch]]
      clusters_single_sample <- as.integer(sce_raw$single_sample_cluster_snn)
      clusters_int <- clusters[sce_clustering$batch == batch]
      assert_that(length(clusters_int) == length(clusters_single_sample))
      bluster::pairwiseRand(clusters_int, clusters_single_sample, mode = "index")
    }) %>%
      set_names(batch_names) %>%
      tibble::as_tibble()

    return(list(
      name = par$name, hvg_rm_cc_genes = par$hvg_rm_cc_genes, cells_per_batch_cluster = tbl,
      cells_per_batch_cluster_percentages = tbl_percentage,
      plot_batch_cluster_absolute = plot_batch_cluster_absolute, plot_batch_cluster_ratio = plot_batch_cluster_ratio,
      batch_cluster_var_df = list(df_var), rand_indices = list(rand_indices)
    ))
  })

  return(int_diagnostics_df)
}

#' @title Make dimred plot for each combination of integration method, dimred method, and coloring by
#' batch (single-sample) and cell cycle phase.
#' @param dimred_plots_params_df (*input target*) A tibble.
#' @return A modified `dimred_plots_params_df` tibble with appended `ggplot2` object.
#'   *Output target*: `sce_int_dimred_plots_df`
#'
#' @concept integration_integration_fn
#' @export
sce_int_dimred_plots_df_fn <- function(dimred_plots_params_df) {
  sce_int_dimred_plots_df <- lapply_rows(dimred_plots_params_df, FUN = function(par) {
    title <- glue("Integration method: '{par$name}'")
    if (par$hvg_rm_cc_genes) {
      title <- glue("{title} (removed cell-cycle related genes from HVGs)")
    }

    par$plot <- plotReducedDim_mod(
      par$sce_dimred,
      dimred = par$dimred_name,
      colour_by = par$colour_by,
      title = title,
      subtitle = str_to_upper(par$dimred_name)
    )
    par$sce_dimred <- NULL

    return(par)
  })

  return(sce_int_dimred_plots_df)
}

#' @title Prepare parameters for expression plots of selected markers.
#' @param selected_markers_file A character scalar: path to CSV file with marker definitions.
#'   Defined in `SELECTED_MARKERS_FILE` parameter in `01_integration.yaml` config.
#' @param sce_int_dimred_df (*input target*) A tibble.
#' @return A modified `sce_int_dimred_df` tibble. *Output target*: `selected_markers_int_df`
#'
#' @concept integration_integration_fn
#' @export
selected_markers_int_df_fn <- function(selected_markers_file, sce_int_dimred_df) {
  sce_int_dimred_df <- dplyr::select(sce_int_dimred_df, name, hvg_rm_cc_genes, sce_dimred)

  readr::read_csv(selected_markers_file, col_names = c("group", "markers"), col_types = "cc") %>%
    tidyr::crossing(dplyr::select(sce_int_dimred_df, .data$name, .data$hvg_rm_cc_genes), dimred_name = c("pca", "umap", "tsne")) %>%
    dplyr::mutate(rmcc = dplyr::if_else(.data$hvg_rm_cc_genes, "rmcc", "normcc")) %>%
    tidyr::unite("int_rmcc_dimred", .data$name, .data$rmcc, .data$dimred_name, remove = FALSE) %>%
    tidyr::nest(selected_markers = c(.data$group, .data$markers))
}

#' @title Make expression plots of selected markers.
#' @param selected_markers_int_df (*input target*) A tibble.
#' @param sce_int_dimred_df (*input target*) A tibble.
#' @return A modified `selected_markers_int_df` tibble. *Output target*: `selected_markers_int_plots_df`
#'
#' @concept integration_integration_fn
#' @export
selected_markers_int_plots_df_fn <- function(selected_markers_int_df, sce_int_dimred_df) {
  sce_dimred <- sce_int_dimred_df %>%
    dplyr::filter(
      .data$name == unique(selected_markers_int_df$name),
      .data$hvg_rm_cc_genes == unique(selected_markers_int_df$hvg_rm_cc_genes)
    ) %>%
    dplyr::pull(.data$sce_dimred) %>%
    .[[1]]

  selected_markers_int_plots_df <- lapply_rows(selected_markers_int_df, FUN = function(par) {
    title <- glue("Integration method: '{par$name}'")
    if (par$hvg_rm_cc_genes) {
      title <- glue("{title} (removed cell-cycle related genes from HVGs)")
    }

    p <- selected_markers_dimplot(
      sce = sce_dimred,
      dimred = par$dimred_name,
      selected_markers_df = selected_markers_int_df$selected_markers[[1]],
      assay = "logcounts"
    ) + patchwork::plot_annotation(title = title)

    par$plot <- list(p)
    return(par)
  })

  return(selected_markers_int_plots_df)
}

#' @title Return description for an integration method.
#' @param int_method_name A character scalar: integration method name.
#' @return A named list with character scalars: `header`, `description`, `fn_link`.
#'
#' @concept integration_integration_fn
#' @export
get_int_method_description <- function(int_method_name = c("uncorrected", "rescaling", "regression", "mnn")) {
  int_method_name <- arg_match(int_method_name)

  if (int_method_name == "uncorrected") {
    header <- "Uncorrected (`multiBatchNorm()`)"
    description <- str_space(
      "Data were rescaled to adjust for differences in sequencing depth between samples.",
      "These data will be used for identification of cluster markers (stage `cluster_markers`) and",
      "differential expression analysis (stage `contrasts`).",
      "\n\nMore details in [OSCA](https://bioconductor.org/books/3.15/OSCA.multisample/integrating-datasets.html#no-correction)"
    )
    fn_link <- downlit::downlit_md_string("`batchelor::multiBatchNorm()`") %>% stringr::str_trim()
  } else if (int_method_name == "rescaling") {
    header <- "Rescaling (`rescaleBatches()`)"
    description <- str_space(
      "We use the `rescaleBatches()` function from the batchelor package to remove the batch effect.",
      "This is roughly equivalent to applying a linear regression to the log-expression values per gene,",
      "with some adjustments to improve performance and efficiency. For each gene, the mean expression in",
      "each batch is scaled down until it is equal to the lowest mean across all batches.",
      "We deliberately choose to scale all expression values down as this mitigates differences in variance",
      "when batches lie at different positions on the mean-variance trend.",
      "(Specifically, the shrinkage effect of the pseudo-count is greater for smaller counts,",
      "suppressing any differences in variance across batches.)",
      "\n\nMore details in [OSCA](https://bioconductor.org/books/3.15/OSCA.multisample/integrating-datasets.html#by-rescaling-the-counts)"
    )
    fn_link <- downlit::downlit_md_string("`batchelor::rescaleBatches()`") %>% stringr::str_trim()
  } else if (int_method_name == "regression") {
    header <- "Linear regression (`regressBatches()`)"
    description <- str_space(
      "Batch effects in bulk RNA sequencing studies are commonly removed with linear regression.",
      "This involves fitting a linear model to each gene's expression profile,",
      "setting the undesirable batch term to zero and recomputing the observations sans the batch effect,",
      "yielding a set of corrected expression values for downstream analyses.\n\n",
      "To use this approach in a scRNA-seq context, we assume that the composition of cell",
      "subpopulations is the same across batches. We also assume that the batch effect is additive,",
      "i.e., any batch-induced fold-change in expression is the same across different cell subpopulations",
      "for any given gene. These are strong assumptions as batches derived from different individuals will",
      "naturally exhibit variation in cell type abundances and expression.",
      "Nonetheless, they may be acceptable when dealing with batches that are technical replicates",
      "generated from the same population of cells. (In fact, when its assumptions hold,",
      "linear regression is the most statistically efficient as it uses information from all cells",
      "to compute the common batch vector.) Linear modelling can also accommodate situations where",
      "the composition is known a priori by including the cell type as a factor in the linear model,",
      "but this situation is even less common.",
      "\n\nMore details in [OSCA](https://bioconductor.org/books/3.15/OSCA.multisample/integrating-datasets.html#by-fitting-a-linear-model)"
    )
    fn_link <- downlit::downlit_md_string("`batchelor::regressBatches()`") %>% stringr::str_trim()
  } else if (int_method_name == "mnn") {
    header <- "Mutual nearest neighbors (`fastMNN()`)"
    description <- str_space(
      "Mutual nearest neighbors (MNN) are pairs of cells from different batches that belong in each",
      "other's set of nearest neighbors. The reasoning is that MNN pairs represent cells from the same",
      "biological state prior to the application of a batch effect - see Haghverdi et al. (2018)",
      "for full theoretical details. Thus, the difference between cells in MNN pairs can be used as an",
      "estimate of the batch effect, the subtraction of which yields batch-corrected values.\n\n",
      "Compared to linear regression, MNN correction does not assume that the population composition",
      "is the same or known beforehand. This is because it learns the shared population structure via",
      "identification of MNN pairs and uses this information to obtain an appropriate estimate of the batch effect.",
      "Instead, the key assumption of MNN-based approaches is that the batch effect is orthogonal to the biology",
      "in high-dimensional expression space. Violations reduce the effectiveness and accuracy of the correction,",
      "with the most common case arising from variations in the direction of the batch effect between clusters.",
      "Nonetheless, the assumption is usually reasonable as a random vector is very likely to be orthogonal in high-dimensional space.",
      "\n\nMore details in [OSCA](https://bioconductor.org/books/3.15/OSCA.multisample/integrating-datasets.html#mnn-correction)"
    )
    fn_link <- downlit::downlit_md_string("`batchelor::fastMNN()`") %>% stringr::str_trim()
  }

  return(list(header = header, description = description, fn_link = fn_link))
}
