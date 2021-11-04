## -- Functions related to single-sample analysis / stage 01_input_qc.

#' @title Subset cells in a `SingleCellExperiment` object to non-empty ones and add corresponding statistics.
#' @param sce_raw (*input target*) A `SingleCellExperiment` object.
#' @param empty_droplets (*input target*) A `DataFrame` object returned from [DropletUtils::emptyDrops()].
#' @param empty_droplets_fdr_threshold A numeric scalar: threshold value for FDR of a cell being empty.
#' @return A subsetted `sce_raw` object. *Output target*: `sce_valid_cells`
#'
#' @concept single_sample_input_qc_fn
#' @export
sce_valid_cells_fn <- function(sce_raw, empty_droplets, empty_droplets_fdr_threshold) {
  is_cell_index <- which(empty_droplets$FDR <= empty_droplets_fdr_threshold)
  sce_valid_cells <- sce_raw[, is_cell_index]
  sce_valid_cells$is_empty_fdr <- empty_droplets$FDR[is_cell_index]
  colnames(sce_valid_cells) <- sce_valid_cells$Barcode

  return(sce_valid_cells)
}

#' @title Get a logical filter for ratio of cells expressing a gene and minimum number of UMI per gene.
#' @description A gene is considered expressed when number of its UMIs across all cells is greater than `min_umi` and
#' at the same time it is expressed in at least `min_ratio_cells` ratio of cells.
#' @param sce A `SingleCellExperiment` object.
#' @param min_ratio_cells A numeric scalar: minimum ratio of cells expressing a gene.
#' @param min_umi A numeric scalar: minimum number of UMI per gene across all cells.
#' @return A logical vector.
#'
#' @concept single_sample_input_qc_fn
#' @export
get_gene_filter <- function(sce, min_ratio_cells, min_umi) {
  assert_that_(dplyr::between(min_ratio_cells, 0, 1))
  assert_that_(min_umi > 0)

  num_cells <- min_ratio_cells * ncol(sce)
  is_expressed <- rowSums(counts(sce) >= min_umi) >= num_cells
  return(is_expressed)
}

#' @title Create a tibble with history of cell and gene filtering.
#' @param sce_unfiltered (*input target*) A `SingleCellExperiment` object.
#' @param sce_qc_filter_genes (*input target*) A `SingleCellExperiment` object.
#' @param sce_custom_filter_genes (*input target*) A `SingleCellExperiment` object.
#' @return A tibble. *Output target*: `sce_history`
#'
#' @concept single_sample_input_qc_fn
#' @export
sce_history_fn <- function(sce_unfiltered, sce_qc_filter_genes, sce_custom_filter_genes) {
  tibble::tribble(
    stats::formula("~filtering_type"), stats::formula("~n_cells"), stats::formula("~n_genes"),
    "no_filtering", ncol(sce_unfiltered), nrow(sce_unfiltered),
    "qc", ncol(sce_qc_filter_genes), nrow(sce_qc_filter_genes),
    "custom", ncol(sce_custom_filter_genes), nrow(sce_custom_filter_genes)
  ) %>%
    dplyr::mutate(
      filtering_type = factor(.data$filtering_type, levels = c("no_filtering", "qc", "custom")),
      n_cells = as.integer(.data$n_cells),
      n_genes = as.integer(.data$n_genes)
    )
}

#' @title Plot history of cell and gene filtering.
#' @param sce_history (*input target*) A tibble.
#' @return A `patchwork` object. *Output target*: `sce_history_plot`
#'
#' @concept single_sample_input_qc_fn
#' @export
sce_history_plot_fn <- function(sce_history) {
  patchwork::wrap_plots(
    ggplot(sce_history) +
      ggplot2::geom_col(aes(x = .data$filtering_type, y = .data$n_cells, fill = .data$filtering_type)) +
      ggplot2::theme_bw() +
      ggtitle("Number of cells"),
    ggplot(sce_history) +
      ggplot2::geom_col(aes(x = .data$filtering_type, y = .data$n_genes, fill = .data$filtering_type)) +
      ggplot2::theme_bw() +
      ggtitle("Number of genes"),
    guides = "collect"
  )
}

#' @title Select a `SingleCellExperiment` object which will proceed to the `02_norm_clustering` stage.
#' @param sce_qc_filter_genes (*input target*) A `SingleCellExperiment` object.
#' @param sce_custom_filter_genes (*input target*) A `SingleCellExperiment` object.
#' @param save_dataset_sensitive_filtering A logical scalar: if `TRUE`, return `sce_qc_filter_genes` target,
#'   otherwise `sce_custom_filter_genes` target (both are `SingleCellExperiment` objects).
#' @return See the `save_dataset_sensitive_filtering` argument. *Output target*: `sce_selected`
#'
#' @concept single_sample_input_qc_fn
#' @export
sce_selected_fn <- function(sce_qc_filter_genes, sce_custom_filter_genes, save_dataset_sensitive_filtering) {
  if (save_dataset_sensitive_filtering) {
    return(sce_qc_filter_genes)
  } else {
    return(sce_custom_filter_genes)
  }
}

#' @title Finalize a `SingleCellExperiment` object which will proceed to the `02_norm_clustering` stage.
#' @param sce_selected (*input target*) A `SingleCellExperiment` object.
#' @param gene_annotation (*input target*) A dataframe.
#' @return A `SingleCellExperiment` object. *Output target*: `sce_final_input_qc`
#'
#' @concept single_sample_input_qc_fn
#' @export
sce_final_input_qc_fn <- function(sce_selected, gene_annotation) {
  assert_that(are_equal(rownames(sce_selected), rownames(gene_annotation)))

  sce_final_input_qc <- sce_selected
  rowData_orig <- rowData(sce_selected)
  rowData_orig$ID <- NULL
  rowData_orig$Symbol <- NULL
  rowData(sce_final_input_qc) <- cbind(rowData_orig, gene_annotation)

  return(sce_final_input_qc)
}
