## -- Functions related to cell type annotation.

#' @title Load a list of cell annotation references into a `tibble`.
#' @description Cell annotation references are defined in `02_norm_clustering.yaml` (single-sample) and
#' `02_int_clustering.yaml` (integration) config files in `CELL_ANNOTATION_SOURCES` parameter.
#' @param cell_annotation_sources A list of cell annotation reference sources.
#' @param biomart_dataset Currently not used.
#' @return A `tibble`. *Output target*: `cell_annotation_params`
#'
#' @concept sc_cell_annotation
#' @export
cell_annotation_params_fn <- function(cell_annotation_sources, biomart_dataset = NULL) {
  if (is_empty(cell_annotation_sources)) {
    return(list(NA))
  }

  cell_annotation_sources <- lapply(cell_annotation_sources, function(cell_annotation_source) {
    err_msg <- function() {
      cli::format_error("Error in {.var CELL_ANNOTATION_SOURCES} item {.field {cell_annotation_source$name}}")
    }

    reference_type <- cell_annotation_source$reference_type
    reference <- cell_annotation_source$reference
    label_column <- cell_annotation_source$label_column
    label_subsets <- cell_annotation_source$label_subsets
    train_assay_type <- cell_annotation_source$train_params$assay_type

    if (reference_type == "celldex") {
      reference_se <- tryCatch(
        expr = utils::getFromNamespace(reference, "celldex"),
        error = function(e) {
          return(FALSE)
        }
      )

      if (is_false(reference_se) || !rlang::is_function(reference_se)) {
        cli_abort("{err_msg()}: cannot find reference data {.var {reference}} in the {.pkg celldex} package.")
      }

      cell_ont <- dplyr::if_else(label_column == "label.ont", "nonna", "all")
      ## -- Without loading the ensembldb package the following error appears: 'require("ensembldb") failed'
      reference_se <- withr::with_package("ensembldb", reference_se(ensembl = TRUE, cell.ont = cell_ont))
    } else if (reference_type == "file") {
      assert_that_(
        fs::file_exists(reference),
        msg = "{err_msg()}: Cannot find file {.file {reference}} in {.var reference}."
      )
      reference_se <- readRDS(reference)
      if (is(reference_se, "SingleCellExperiment")) {
        reference_se <- as(reference_se, "SummarizedExperiment")
      }

      assert_that_(
        is(reference_se, "SummarizedExperiment"),
        msg = "{err_msg()}: {.file {reference}} is not a {.var SummarizedExperiment} object."
      )
    } else {
      cli_abort("{err_msg()}: {.var reference_type} must be {.var 'celldex'} or {.var 'file'}.")
    }

    # biomart_dataset_ref <- cell_annotation_source$biomart_dataset

    ## -- TODO: would it be possible to unambiguously map ENSEMBL IDs in reference to test dataset?
    ## -- E.g. when the reference dataset is from mouse experiment and out test one is human.
    # if (biomart_dataset_ref != biomart_dataset) {
    #   cli_alert_info("Fetching {.var {biomart_dataset}} ortholog IDs for {.var {biomart_dataset_ref}}.")
    #   mart_test <- biomaRt::useMart("ensembl", dataset = biomart_dataset)
    #   mart_ref <- biomaRt::useMart("ensembl", dataset = biomart_dataset_ref)
    #   matches <- biomaRt::getLDS(
    #     mart = mart_ref,
    #     martL = mart_test,
    #     attributes = "ensembl_gene_id",
    #     attributesL = "ensembl_gene_id",
    #     filters = "ensembl_gene_id",
    #     values = rownames(reference_se)
    #   )
    # }

    assert_that_(
      train_assay_type %in% assayNames(reference_se),
      msg = "{err_msg()}: {.var train_params/assay_type} {.field {train_assay_type}} not found in {.code assayNames()} of the reference dataset."
    )

    assert_that_(
      label_column %in% colnames(colData(reference_se)),
      msg = "{err_msg()}: {.var label_column} {.field {label_column}} not found in {code colData()} of the reference dataset."
    )

    labels <- colData(reference_se)[[label_column]]

    if (!is_na(label_subsets)) {
      reference_se <- reference_se[, labels %in% label_subsets[[1]]]
      labels <- colData(reference_se)[[label_column]]
      assert_that_(
        ncol(reference_se) > 0,
        msg = "{err_msg()}: reference dataset contains zero cells after filtering labels by {.var label_subsets}."
      )

      assert_that_(
        length(unique(labels)) > 1,
        msg = "{err_msg()}: reference dataset contains less than two distinct labels after filtering by {.var label_subsets}."
      )
    }

    cell_annotation_source$train_params <- list(cell_annotation_source$train_params)
    cell_annotation_source$classify_params <- list(cell_annotation_source$classify_params)
    cell_annotation_source$prune_score_params <- list(cell_annotation_source$prune_score_params)
    cell_annotation_source$diagnostics_params <- list(cell_annotation_source$diagnostics_params)
    cell_annotation_source <- c(cell_annotation_source, list(
      reference_se = list(reference_se),
      labels = list(labels)
    ))

    return(cell_annotation_source)
  })

  cell_annotation_params <- lists_to_tibble(cell_annotation_sources) %>%
    dplyr::relocate(name)

  return(cell_annotation_params)
}

#' @title Perform cell annotation via `SingleR::SingleR()`.
#' @description A new list column `cell_annotation` is added to `cell_annotation_params`, containing the returned
#' `DataFrame`s (one for each reference dataset) from `SingleR::SingleR()`.
#'
#' `train_params` and `classify_params` in `cell_annotation_params` are passed to `SingleR::SingleR()`, and internally
#' to `SingleR::trainSingleR()` and `SingleR::classifySingleR()`, respectively.
#'
#' Label pruning is performed via `SingleR::pruneScores()` after calling `SingleR::SingleR()`, with parameters defined in
#' `cell_annotation_params$prune_score_params`.
#' @param cell_annotation_params (*input target*) A `tibble`.
#' @param sce_test (*input target*) A `SingleCellExperiment` object to predict cell labels in.
#' @inheritParams bpparam_
#' @return A `tibble`. *Output target*: `cell_annotation`
#'
#' @concept sc_cell_annotation
#' @export
cell_annotation_fn <- function(cell_annotation_params, sce_test, BPPARAM = BiocParallel::SerialParam()) {
  if (is_na(cell_annotation_params)) {
    return(NULL)
  }

  res <- lapply_rows(cell_annotation_params, FUN = function(row) {
    cli_alert_info("Cell annotation using reference dataset {.field {row$name}}")

    res_cell_annotation <- SingleR::SingleR(
      test = sce_test,
      ref = row$reference_se,
      labels = row$labels,
      genes = row$train_params$genes,
      sd.thresh = row$train_params$sd_thresh,
      de.method = row$train_params$de_method,
      de.n = row$train_params$de_n,
      quantile = row$classify_params$quantile,
      fine.tune = TRUE,
      tune.thresh = row$classify_params$tune_thresh,
      prune = FALSE,
      assay.type.test = row$classify_params$assay_type,
      assay.type.ref = row$train_params$assay_type,
      BPPARAM = BPPARAM
    )

    pruned_scores <- SingleR::pruneScores(
      res_cell_annotation,
      nmads = row$prune_score_params$n_mads,
      min.diff.med = row$prune_score_params$min_diff_med,
      min.diff.next = row$prune_score_params$min_diff_next
    )

    res_cell_annotation$pruned.labels <- res_cell_annotation$labels
    res_cell_annotation$pruned.labels[pruned_scores] <- NA_character_

    row$cell_annotation <- list(res_cell_annotation) %>%
      magrittr::set_names(row$name)

    return(row)
  })

  return(res)
}

#' @title Create a named list of cell labels returned from `SingleR::SingleR()`.
#' @description `SingleR::SingleR()` returns three types of cell labels. In the returned list from this function,
#' names of those are prefixed by a reference dataset's name and modified such that:
#'
#' - `first.labels` ("raw" labels) -> `<reference_name>_labels_raw`
#' - `labels` (fine-tuned, unpruned labels) -> `<reference_name>_labels`
#' - `pruned.labels` (fine-tuned and pruned labels) -> `<reference_name>_labels_pruned`
#'
#' Pruned labels contain `NA`s for low quality labels.
#'
#' @param cell_annotation (*input target*) A `tibble`.
#' @return A named list. *Output target*: `cell_annotation_labels`
#'
#' @concept sc_cell_annotation
#' @export
cell_annotation_labels_fn <- function(cell_annotation) {
  if (is_null(cell_annotation)) {
    return(NULL)
  }

  purrr::map2(cell_annotation$name, cell_annotation$cell_annotation, function(name, cell_annotation) {
    res <- list(
      labels_raw = cell_annotation$first.labels,
      labels = cell_annotation$labels,
      labels_pruned = cell_annotation$pruned.labels
    ) %>% purrr::map(factor)
    names(res) <- glue("{name}_{names(res)}")
    return(res)
  }) %>%
    unlist(recursive = FALSE)
}

#' @title Generate diagnostic plots for cell annotation.
#' @description Three list and character vector columns are added to `cell_annotation`:
#'
#' - `score_heatmaps`: heatmaps of per-cell label scores created for each clustering (as column annotation)
#'   ([details](http://bioconductor.org/books/release/SingleRBook/annotation-diagnostics.html#based-on-the-scores-within-cells)).
#' - `marker_heatmaps`: `NULL` if `cell_annotation$train_params$de` is not `"de"`, otherwise heatmap for each label
#'   containing top upregulated markers from pairwise t-tests
#'   ([details](http://bioconductor.org/books/release/SingleRBook/annotation-diagnostics.html#based-on-marker-gene-expression)).
#'   Number of top markers is specified in `cell_annotation$diagnostics_params$heatmap_n_top_markers`.
#' - `delta_distribution_plot`: violin plots (in one figure / object) of per-cell deltas for each label.
#'    Deltas are differences between the score for the assigned label and the median across all labels for each cell
#'    ([details](http://bioconductor.org/books/release/SingleRBook/annotation-diagnostics.html#based-on-the-deltas-across-cells)).
#'
#' Columns with output PDF files are named as `score_heatmaps_out_file` etc.
#' @param cell_annotation (*input target*) A `tibble`.
#' @param cell_data (*input target*) A `DataFrame`.
#' @param sce A `SingleCellExperiment` object.
#' @param base_out_dir A character scalar: path to output directory under which will be for each reference dataset its
#'   diagnostic plots saved in.
#' @param cluster_cols_regex A character scalar: regex to match columns in `colData()` with cluster labels.
#'   Those columns will be used in annotation score heatmaps.
#' @return A `tibble`. *Output target*: `cell_annotation_diagnostic_plots`
#'
#' @concept sc_cell_annotation
#' @export
cell_annotation_diagnostic_plots_fn <- function(cell_annotation,
                                                cell_data,
                                                sce,
                                                base_out_dir,
                                                cluster_cols_regex = "^cluster_") {
  if (is_null(cell_annotation)) {
    return(NULL)
  }

  res <- lapply_rows(cell_annotation, FUN = function(row) {
    cluster_cols <- str_subset(colnames(cell_data), cluster_cols_regex)
    row$score_heatmaps <- lapply(cluster_cols, FUN = function(cluster_col) {
      heatmap <- SingleR::plotScoreHeatmap(
        results = row$cell_annotation,
        clusters = cell_data[[cluster_col]],
        show.pruned = TRUE,
        main = glue("Cell annotation score\n{row$name}\n{cluster_col}"),
        silent = TRUE
      ) %>%
        ggplotify::as.ggplot()

      return(heatmap)
    }) %>%
      magrittr::set_names(glue("{cluster_cols}_vs_{row$name}"))

    row$delta_distribution_plot <- SingleR::plotDeltaDistribution(row$cell_annotation) +
      ggtitle("Delta score distribution", subtitle = row$name)

    if (row$train_params$genes == "de") {
      labels <- row$cell_annotation$labels
      sce$labels <- labels
      heatmap_n_top_markers <- row$diagnostics_params$heatmap_n_top_markers
      ref_markers <- metadata(row$cell_annotation)$de.genes
      empirical_markers <- scran::findMarkers(sce, groups = labels, direction = "up")
      row$marker_heatmaps <- lapply(unique(labels), FUN = function(label) {
        ref_markers_label <- ref_markers[[label]] %>%
          unlist() %>%
          unique()
        m <- match(ref_markers_label, rownames(empirical_markers[[label]]))

        if (is_empty(m)) {
          heatmap <- create_dummy_plot(glue("No DE genes found for:\n{row$name} / {label}"))
        } else {
          m <- ref_markers_label[rank(m) <= heatmap_n_top_markers]
          heatmap <- scater::plotHeatmap(
            sce,
            features = m,
            order_columns_by = "labels",
            labels_row = rowData(sce)[m, "SYMBOL", drop = TRUE],
            main = glue("{row$name}\n{label} (top {heatmap_n_top_markers} markers)"),
            silent = TRUE
          ) %>%
            ggplotify::as.ggplot()
        }
        return(heatmap)
      }) %>%
        magrittr::set_names(unique(labels))
    } else {
      row$marker_heatmaps <- list(NULL)
    }

    return(row)
  })

  res <- res %>%
    dplyr::mutate(
      out_dir = fs::path(.env$base_out_dir, name),
      score_heatmaps_out_file = fs::path(.data$out_dir, "score_heatmaps.pdf"),
      delta_distribution_plot_out_file = fs::path(.data$out_dir, "delta_distribution_plot.pdf"),
      marker_heatmaps_out_file = dplyr::if_else(
        purrr::map_lgl(.data$marker_heatmaps, is_null),
        NA_character_,
        fs::path(.data$out_dir, "marker_heatmaps.pdf") %>% as.character()
      )
    )
}

#' @title Save cell annotation diagnostic plots to PDF files.
#' @param cell_annotation_diagnostic_plots (*input target*) A `tibble`.
#' @return A character vector. *Output target*: `cell_annotation_diagnostic_plots_files`
#'
#' @concept sc_cell_annotation
#' @export
cell_annotation_diagnostic_plots_files_fn <- function(cell_annotation_diagnostic_plots) {
  if (is_null(cell_annotation_diagnostic_plots)) {
    return("")
  }

  lapply_rows(cell_annotation_diagnostic_plots, FUN = function(row) {
    save_pdf(row$score_heatmaps, output_file = row$score_heatmaps_out_file)
    save_pdf(list(row$delta_distribution_plot), output_file = row$delta_distribution_plot_out_file)
    if (!is_null(row$marker_heatmaps)) {
      save_pdf(row$marker_heatmaps, output_file = row$marker_heatmaps_out_file)
    }
    return(row)
  })

  to_return <- c(
    cell_annotation_diagnostic_plots$score_heatmaps_out_file,
    cell_annotation_diagnostic_plots$delta_distribution_plot_out_file,
    cell_annotation_diagnostic_plots$marker_heatmaps_out_file
  ) %>%
    purrr::discard(is.na)

  return(to_return)
}
