## -- Functions related to single-sample analysis / stage 01_input_qc.

#' @title Import scRNA-seq data.
#' @description For more details see the `INPUT_DATA` parameter in `vignette("stage_input_qc")`.
#' @param input_data A named list of named lists containing character scalars:
#'   - `type`:
#'     - `"cellranger"`: a raw feature-barcode matrix from 10x Genomics `cellranger`.
#'     - `"table"`: a delimited text file (table).
#'     - `"sce"`: a `SingleCellExperiment` object (Rds file).
#'   - `path`: a path to input file (or directory in case of `type = "cellranger"`).
#'   - `delimiter`: a field delimiter when `type = "table"`.
#' @return A `SingleCellExperiment` object. *Output target*: `sce_raw`
#'
#' @concept single_sample_input_qc_fn
#' @export
sce_raw_fn <- function(input_data) {
  input_type <- input_data$type
  input_path <- input_data$path

  assert_that_(
    !is_null(input_type), !is_null(input_path),
    msg = "{.field INPUT_DATA$type} or {.field INPUT_DATA$path} is not set, cannot load data."
  )

  if (input_type == "cellranger") {
    sce_raw <- DropletUtils::read10xCounts(input_path)
    colnames(sce_raw) <- colData(sce_raw)$Barcode
  } else if (input_type == "table") {
    delimiter <- input_data$delimiter
    assert_that_(!is_null(delimiter), msg = "{.field INPUT_DATA$delimiter} is not set, cannot load {.file {input_path}}")
    sce_raw <- readr::read_delim(input_path, delim = delimiter)
    assert_that_(
      "ENSEMBL" %in% colnames(sce_raw),
      msg = "Column {.var ENSEMBL} with ENSEMBL IDs is missing in {.file {input_path}}"
    )
    sce_raw <- as.data.frame(sce_raw) %>%
      tibble::column_to_rownames("ENSEMBL") %>%
      as.matrix()
    mode(sce_raw) <- "integer"
    sce_raw <- Matrix::Matrix(sce_raw, sparse = TRUE)
    sce_raw <- SingleCellExperiment(assays = list(counts = sce_raw))
    ## -- This will add the same information as when a raw BC matrix from cellranger is loaded with
    ## -- DropletUtils::read10xCounts(), but we probably don't need to do so.
    # rowData(sce_raw) <- DataFrame(ID = rownames(sce_raw), Symbol = NA, Type = "Gene Expression")
    # colData(sce_raw) <- DataFrame(Sample = input_path, Barcode = colnames(sce_raw))
  } else if (input_type == "sce") {
    sce_raw <- readRDS(input_path)
    assert_that_(
      is(sce_raw, "SingleCellExperiment"),
      msg = "Object loaded from {.file {input_path}} is not a {.var SingleCellExperiment} object."
    )
    assert_that_(
      "counts" %in% assayNames(sce_raw),
      msg = "{.field counts} assay not found in SCE object loaded from {.file {input_path}}"
    )
  } else {
    cli::cli_abort("{.var input_data$type} must be {.field 'cellranger'}, {.field 'table'}, or {.field 'sce'}")
  }

  return(sce_raw)
}

#' @title Calculate empty droplet statistics for each cell.
#' @description See `DropletUtils::emptyDrops()` for implementation details.
#' @param sce_raw (*input target*) A `SingleCellExperiment` object.
#' @param empty_droplets_lower An integer scalar: lower bound on the total UMI count at or below which all barcodes
#'   are assumed to correspond to empty droplets. Passed to `DropletUtils::emptyDrops()`.
#' @param empty_droplets_enabled A logical scalar: if `TRUE`, skip empty droplets calculation and return `NULL`.
#' @inheritParams bpparam_
#' @return A `DataFrame` object (value returned from `DropletUtils::emptyDrops()`) if `empty_droplets_enabled`
#' is `TRUE`, `NULL` otherwise. *Output target*: `empty_droplets`
#'
#' @concept single_sample_input_qc_fn
#' @export
empty_droplets_fn <- function(sce_raw,
                              empty_droplets_lower,
                              empty_droplets_enabled = TRUE,
                              BPPARAM = BiocParallel::SerialParam()) {
  if (empty_droplets_enabled) {
    DropletUtils::emptyDrops(
      m = counts(sce_raw), lower = empty_droplets_lower, BPPARAM = BPPARAM
    )
  } else {
    NULL
  }
}

#' @title Subset cells in a `SingleCellExperiment` object to non-empty ones and add corresponding statistics.
#' @param sce_raw (*input target*) A `SingleCellExperiment` object.
#' @param empty_droplets (*input target*) A `DataFrame` object returned from [DropletUtils::emptyDrops()].
#' @param empty_droplets_fdr_threshold A numeric scalar: threshold value for FDR of a cell being empty.
#' @return A subsetted `sce_raw` object. *Output target*: `sce_valid_cells`
#'
#' @concept single_sample_input_qc_fn
#' @export
sce_valid_cells_fn <- function(sce_raw, empty_droplets, empty_droplets_fdr_threshold) {
  assert_that_(ncol(sce_raw) > 0, msg = "{.var sce_raw} contains zero cells")

  if (is_null(empty_droplets)) {
    sce_raw$is_empty_fdr <- NA
  } else {
    is_cell_index <- which(empty_droplets$FDR <= empty_droplets_fdr_threshold)
    colnames_orig <- colnames(sce_raw)
    sce_raw <- sce_raw[, is_cell_index]
    sce_raw$is_empty_fdr <- empty_droplets$FDR[is_cell_index]
  }

  return(sce_raw)
}

#' @title Get a logical filter for genes not passing a ratio of cells expressing a gene and a minimum number of UMI per gene.
#' @description A gene is considered expressed when number of its UMIs across all cells is greater than `min_umi` and
#' at the same time it is expressed in at least `min_ratio_cells` ratio of cells.
#' @param sce A `SingleCellExperiment` object.
#' @param min_ratio_cells A numeric scalar: minimum ratio of cells expressing a gene.
#' @param min_umi A numeric scalar: minimum number of UMI per gene across all cells.
#' @return A logical vector of length `nrow(sce)`: `TRUE` for genes **not passing** the filter, `FALSE` otherwise.
#' This is for consistency with cell filters, e.g. the `qc_filter` target, where `TRUE` means the same as here,
#' i.e. a cell is not passing the filter.
#'
#' @concept single_sample_input_qc_fn
#' @export
get_gene_filter <- function(sce, min_ratio_cells, min_umi) {
  assert_that_(dplyr::between(min_ratio_cells, 0, 1))
  assert_that_(min_umi >= 0)

  num_cells <- min_ratio_cells * ncol(sce)
  is_expressed <- rowSums(counts(sce) >= min_umi) >= num_cells
  return(!is_expressed)
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
