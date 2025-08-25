## -- Common functions related to SingleCellExperiment objects.

#' @title Get indices of genes whose annotation matches a regex.
#' @param sce A `SingleCellExperiment` object.
#' @param regex A character scalar.
#' @param colname A character scalar: name of column in `rowData(sce)` whose values will be tested by `regex`.
#' @param ignore_case A logical scalar: if `TRUE`, ignore case in `regex`.
#' @return An integer vector: indices of `rowData(sce)` matching the `regex`.
#'
#' @examples \dontrun{
#' # Indices of mitochondrial genes.
#' which_genes_regex(sce, regex = "^MT-", colname = "Symbol", ignore_case = TRUE)
#' }
#'
#' @concept sc_sce
#' @export
which_genes_regex <- function(sce, regex, colname = "Symbol", ignore_case = TRUE) {
  stringr::str_which(rowData(sce)[[colname]], stringr::regex(regex, ignore_case = ignore_case))
}

#' @title Append new columns to `colData` of a `SingleCellExperiment` object.
#' @param sce A `SingleCellExperiment` object.
#' @param df A dataframe which will be binded column-wise to `colData(sce)`.
#' @param replace A logical scalar: if `TRUE` and columns to be added already exist, they will be first removed
#'   from `colData(sce)`.
#' @return A modified `sce` object with added columns.
#'
#' @examples
#' \dontrun{
#' df <- data.frame(example = rownames(colnames))
#' sce <- sce_add_colData(sce, df = df)
#' }
#'
#' @concept sc_sce
#' @export
sce_add_colData <- function(sce, df, replace = TRUE) {
  assert_that_(
    !is_empty(df),
    msg = "{.var df} cannot be empty."
  )

  assert_that_(
    nrow(df) == ncol(sce),
    msg = "Number of rows in {.var df} must be same as number of columns in {.var sce}."
  )

  if (replace) {
    existing_columns <- intersect(colnames(colData(sce)), colnames(df))
    colData(sce) <- colData(sce)[, !colnames(colData(sce)) %in% existing_columns]
  }

  colData(sce) <- cbind(colData(sce), df)

  return(sce)
}

#' @title Append new columns to `colData` of a `SingleCellExperiment` or `SpatialExperiment` object.
#' @param sce A `SingleCellExperiment` object.
#' @param df A dataframe which will be binded column-wise to `colData(sce)`.
#' @param replace A logical scalar: if `TRUE` and columns to be added already exist, they will be first removed
#'   from `colData(sce)`.
#' @param spatial A logical scalar: if `TRUE` add spatial quality control results
#' @return A modified `sce` object with added columns.
#'
#' @examples
#' \dontrun{
#' df <- data.frame(example = rownames(colnames))
#' sce <- sce_add_colData(sce, df = df)
#' }
#'
#' @concept sc_sce
#' @export
sce_add_spatial_colData <- function(sce, df, replace = TRUE, spatial) {
  assert_that_(
    !is_empty(df),
    msg = "{.var df} cannot be empty."
  )
  
  assert_that_(
    nrow(df) == ncol(sce),
    msg = "Number of rows in {.var df} must be same as number of columns in {.var sce}."
  )
  
  if (replace) {
    existing_columns <- intersect(colnames(colData(sce)), colnames(df))
    colData(sce) <- colData(sce)[, !colnames(colData(sce)) %in% existing_columns]
  }
  
  colData(sce) <- cbind(colData(sce), df)
  ##only for inputqc section
  if (spatial) {
    sce <- SpotSweeper::localVariance(sce, metric = "subsets_mito_percent", 
                                      n_neighbors = 36, name = "local_mito_variance_k36")
    sce <- SpotSweeper::findArtifacts(sce,
                                      mito_percent = "subsets_mito_percent",
                                      mito_sum = "subsets_mito_detected",
                                      n_order = 10,log = FALSE,
                                      name = "artifact"
    )
  }
  return(sce)
}

#' @title Append data to `metadata()` list of a `SingleCellExperiment` object.
#' @description [utils::modifyList()] is used internally, so existing named items in `metadata()` can be overwritten.
#' @param sce A `SingleCellExperiment` object.
#' @param ... Objects to be added to `metadata(sce)`.
#' @return A modified `sce` object with data appended to its `metadata()`.
#'
#' @examples \dontrun{
#' sce <- sce_add_metadata(sce, a = 1, "some_data", b = list(c = 1))
#' }
#'
#' @concept sc_sce
#' @export
sce_add_metadata <- function(sce, ...) {
  # metadata(sce) <- c(metadata(sce), list(...))
  metadata(sce) <- utils::modifyList(metadata(sce), list(...), keep.null = TRUE)
  return(sce)
}

#' @title Append data to metadata o a `Seurat` object's assay.
#' @param seu A `Seurat` object.
#' @param assay A character scalar: name of assay.
#' @param ... Objects to be added
#' @return A modified `seu` object with data appended to its `assay` metadata.
#'
#' @concept sc_sce
#' @export
seu_add_metadata <- function(seu, assay = "RNA", ...) {
  seu@assays[[assay]]@misc <- c(seu@assays[[assay]]@misc, list(...))
  return(seu)
}

#' @title Convert a `SingleCellExperiment` to `Seurat` object.
#' @description  A wrapper around [Seurat::as.Seurat()].
#' @param sce A `SingleCellExperiment` object.
#' @param sce_assay A character scalar: name of assay in `sce` (e.g. `counts` or `logcounts`).
#'   Use `NULL` to convert all assays (default).
#' @param seurat_assay A character scalar: name of assay in the new `Seurat` object.
#' @param add_rowData A logical scalar: if `TRUE`, add `rowData(sce)` to `meta.features` slot of assay of the new
#'   `Seurat` object.
#' @param ... Passed to [Seurat::as.Seurat()].
#' @return A `Seurat` object.
#'
#' @concept sc_sce
#' @export
as_seurat <- function(sce, sce_assay = NULL, seurat_assay = "RNA", add_rowData = TRUE, ...) {
  seu <- Seurat::as.Seurat(sce, assay = sce_assay, ...) %>%
    SeuratObject::RenameAssays(originalexp = seurat_assay)

  if (add_rowData) {
    assert_that(are_equal(rownames(seu@assays[[seurat_assay]]), rownames(rowData(sce))))
    seu@assays[[seurat_assay]]@meta.features <- cbind(
      seu@assays[[seurat_assay]]@meta.features,
      rowData(sce) %>% as.data.frame()
    )
  }

  return(seu)
}

#' @title Create a `Seurat` object used for heatmap generation.
#' @param sce_dimred A `SingleCellExperiment` object with calculated dimreds.
#' @param calc_zscore A logical scalar: if `TRUE`, calculate z-scores of UMI counts in `assays$RNA@scale.data` slot.
#' @return A `Seurat` object.
#'
#' @concept sc_sce
#' @export
create_seu_for_heatmaps <- function(sce_dimred, calc_zscore = TRUE, spatial = FALSE) {
  if ("integrated" %in% assayNames(sce_dimred)) {
    data <- "integrated"
    assay(sce_dimred, data) <- as.matrix(assay(sce_dimred, data))
    reducedDim(sce_dimred, "corrected") <- NULL
  } else {
    data <- "logcounts"
  }
  
  # Convert to Seurat object
  if (spatial) {
    sce_dimred2 <- convertToSCE(sce_dimred) 
    seu <- Seurat::as.Seurat(sce_dimred2, data = data)
  } else {
    seu <- as_seurat(sce_dimred, data = data)
  }
  
  available_assays <- Seurat::Assays(seu)
  
  # Decide which assay to use
  if ("originalexp" %in% available_assays) {
    assay_to_use <- "originalexp"
  } else if ("RNA" %in% available_assays) {
    assay_to_use <- "RNA"
  } else {
    stop("Neither 'originalexp' nor 'RNA' assay found in Seurat object.")
  }
  # In Seurat v5, scale.data is not always populated by default
  if (calc_zscore) {
    # Ensure all features are scaled
    seu <- Seurat::ScaleData(seu, assay = assay_to_use, features = rownames(seu[[assay_to_use]]))
  }
  
  return(seu)
}

#' @title Make a dataframe of cells per cluster (counts and percentages).
#' @param clusters A vector.
#' @param var_name A character scalar: name of column in the returned dataframe.
#' @return A dataframe.
#'
#' @examples
#' cells_per_cluster_table(rep(c(1, 2), each = 5))
#' @concept sc_sce
#' @export
cells_per_cluster_table <- function(clusters, var_name = "Cluster") {
  if (is(clusters, "data.frame")) {
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

#' @title Merge `metadata()` of multiple `SingleCellExperiment` object.
#' @param sce_list A list of `SingleCellExperiment` objects.
#' @param what A character scalar: name of item to merge.
#' @param as_vector A logical scalar: if `TRUE`, return vector of merged values.
#' @param as_named_list A logical scalar: if `TRUE`, return a named list with name equal to `what`.
#' @return A vector or list.
#'
#' @concept sc_sce
#' @export
merge_sce_metadata <- function(sce_list, what, as_vector = TRUE, as_named_list = FALSE) {
  res <- purrr::map(sce_list, ~ metadata(.)[[what]]) %>%
    purrr::modify_if(is_null, ~NA_character_)

  if (as_vector) {
    res <- unlist(res)
  } else {
    names(res) <- names(sce_list)
  }

  if (as_named_list) {
    return(list(res) %>% set_names(what))
  } else {
    return(res)
  }
}

#' @title Create new cell groups based on existing ones.
#' @description This is basically a recoding of levels. A new column (cell group) will be based on levels in the
#' existing column.
#' @param df A dataframe.
#' @param cell_groupings A named list of named lists. Names of outer list will be used for new columns in `df`.
#'   The nested lists must have the following values:
#'   - `source_column`: a name of column in `df` to use.
#'   - `assignments`: a named list with values in the form `old_level = new_level`.
#'   See the `CELL_GROUPINGS` parameter in `02_norm_clustering.yaml` or `02_int_clustering.yaml` config.
#' @param do_cbind A logical scalar: if `TRUE`, bind column-wise `df` and dataframe with new cell groupings.
#' @return A dataframe.
#'
#' @concept sc_sce
#' @rdname cell_data
#' @export
make_cell_groupings <- function(df, cell_groupings, do_cbind = FALSE) {
  assert_that(!is_null(cell_groupings))

  new_groups <- lapply(names(cell_groupings), FUN = function(group_name) {
    if (group_name %in% colnames(df)) {
      cli_alert_warning("A new cell grouping ({.val {group_name}}) is already present and will replace the existing one.")
    }

    source_column <- cell_groupings[[group_name]][["source_column"]]
    assignments <- cell_groupings[[group_name]][["assignments"]]

    if (!source_column %in% colnames(df)) {
      cli_alert_warning("{.field source_column} {.val {source_column}} not found, skipping.")
      return(NULL)
    }

    old_group <- df[[source_column]]
    old_group <- old_group %>%
      as.character() %>%
      factor(levels = levels(old_group))
    new_group <- dplyr::recode(old_group, !!!assignments)
  }) %>%
    set_names(names(cell_groupings)) %>%
    purrr::discard(is_null)

  if (do_cbind) {
    return(cbind(df, as.data.frame(new_groups)))
  } else {
    return(as.data.frame(new_groups))
  }
}

#' @title Load additional cell data from a CSV or Rds (dataframe) file.
#' @param additional_cell_data_file A character scalar: path to input file.
#' @return A dataframe-like object.
#'
#' @concept sc_sce
#' @export
additional_cell_data_fn <- function(additional_cell_data_file) {
  if (is_empty(additional_cell_data_file)) {
    return(NULL)
  }

  assert_that_(fs::file_exists(additional_cell_data_file), msg = "File not found: {.file {additional_cell_data_file}}")
  extension <- fs::path_ext(additional_cell_data_file)
  if (extension == "csv") {
    obj <- readr::read_csv(additional_cell_data_file)
  } else if (stringr::str_to_lower(extension) == "rds") {
    obj <- readRDS(additional_cell_data_file)
    assert_that_(
      inherits(obj, "data.frame") || inherits(obj, "DFrame"),
      msg = "{.file {additional_cell_data_file}} is not {.field data.frame} or {.field DataFrame}."
    )
    if (inherits(obj, "DFrame")) {
      obj <- as.data.frame(obj)
    }
  } else {
    cli_abort(glue(str_space(
      "Allowed file types of additional cell data are CSV and Rds.",
      "Your file has an unrecognized extension {.file extension}."
    )))
  }
  return(obj)
}


#' @title Merge all cell-related data to a single DataFrame.
#' @param col_data A dataframe.
#' @param clusters_all A named list.
#' @param cell_annotation_labels A named list.
#' @param cell_groupings A dataframe: see [make_cell_groupings()].
#' @param additional_cell_data A data.frame-like object with additional cell data to be joined.
#' @param pipeline_type A character scalar:
#'   - If `"single_sample"`, then `additional_cell_data` must contain `Barcode` column.
#'   - If `"integration"`, then `additional_cell_data` must contain `Barcode` and `batch` columns.
#' @return A DataFrame object.
#'
#' @concept sc_sce
#' @export
cell_data_fn <- function(col_data,
                         clusters_all,
                         cell_annotation_labels,
                         cell_groupings,
                         spot_deconvolution_labels,
                         manual_annotation_labels,
                         additional_cell_data,
                         pipeline_type = c("single_sample", "integration")) {
  row_names <- rownames(col_data)
  existing_columns <- intersect(colnames(col_data), c(names(clusters_all), names(cell_annotation_labels)))
  col_data <- col_data[, !colnames(col_data) %in% existing_columns]

  if (!is_null(clusters_all)) {
    col_data <- dplyr::bind_cols(col_data, tibble::as_tibble(clusters_all))
  }

  if (!is_null(cell_annotation_labels)) {
    col_data <- dplyr::bind_cols(col_data, tibble::as_tibble(cell_annotation_labels))
  }
  if (!is_null(spot_deconvolution_labels)) {
    col_data <- dplyr::left_join(col_data, spot_deconvolution_labels[, c("Barcode", "Deconvolution_annot")], by = "Barcode")
  }
  
  if (!is_null(manual_annotation_labels)) {
    col_data <- dplyr::bind_cols(col_data, tibble::as_tibble(manual_annotation_labels))
  }

  if (!is_null(additional_cell_data)) {
    pipeline_type <- arg_match(pipeline_type)
    if (pipeline_type == "single_sample") {
      assert_that_("Barcode" %in% colnames(additional_cell_data), msg = "Additional cell data must contain {.field Barcode} column.")
      join_cols <- "Barcode"
    } else {
      assert_that_(
        all(c("Barcode", "batch") %in% colnames(additional_cell_data)),
        msg = "Additional cell data must contain {.field Barcode} and {.field batch} columns."
      )
      join_cols <- c("Barcode", "batch")
    }

    existing_columns <- intersect(colnames(col_data), setdiff(colnames(additional_cell_data), join_cols))
    if (!is_empty(existing_columns)) {
      cli_alert_info("Additional cell data will override existing columns: {.vals {existing_columns}}")
    }

    col_data <- col_data %>%
      dplyr::select(-dplyr::all_of(existing_columns)) %>%
      dplyr::left_join(additional_cell_data, by = join_cols)
  }

  col_data <- S4Vectors::DataFrame(col_data)

  if (!is_empty(cell_groupings)) {
    cell_groupings <- lapply(cell_groupings, FUN = function(grp) {
      grp$description <- if (is_null(grp$description)) "" else grp$description
      return(grp)
    })
    new_cell_groupings <- make_cell_groupings(col_data, cell_groupings = cell_groupings, do_cbind = FALSE)
    existing_columns <- intersect(colnames(col_data), colnames(new_cell_groupings))
    col_data <- col_data[, !colnames(col_data) %in% existing_columns]
    col_data <- cbind(col_data, new_cell_groupings)
    metadata(col_data)$cell_groupings <- cell_groupings
  }

  rownames(col_data) <- row_names
  return(col_data)
}

#' @title Add columns to `colData()` of a `SingleCellExperiment` object.
#' @param sce_dimred A `SingleCellExperiment` object.
#' @param cell_data A dataframe.
#' @param overwrite_sce A logical scalar: if `TRUE`, columns in `colData(sce_dimred)` will be overwritten by
#'   those with the same names in `cell_data`. Otherwise the opposite will happen.
#' @return A modified `sce_dimred` with added/updated `colData()` columns from `cell_data`.
#'
#' @concept sc_sce
#' @export
sce_add_cell_data <- function(sce_dimred, cell_data, overwrite_sce = TRUE) {
  if (overwrite_sce) {
    sce_cols <- setdiff(colnames(colData(sce_dimred)), colnames(cell_data))
    cell_data_cols <- colnames(cell_data)
  } else {
    sce_cols <- colData(sce_dimred) %>% colnames()
    cell_data_cols <- setdiff(colnames(cell_data), colnames(colData(sce_dimred)))
  }

  colData(sce_dimred) <- cbind(
    colData(sce_dimred)[, sce_cols],
    cell_data[, cell_data_cols]
  )

  metadata(sce_dimred)$cell_groupings <- metadata(cell_data)$cell_groupings

  return(sce_dimred)
}

#' @title Rewrite `SpatialExperiment` object to a `SingleCellExperiment` object.
#' @param spe_object A `SpatialExperiment` object.
#' @concept spatial_sce
#' @return A `SingleCellExperiment` object
#' @export
convertToSCE <- function(spe_object) {
  if (!inherits(spe_object, "SpatialExperiment")) {
    stop("Input must be a SpatialExperiment object.")
  }
  
  # Extract components
  assays_list <- SummarizedExperiment::assays(spe_object)
  col_data <- SummarizedExperiment::colData(spe_object)
  row_data <- SummarizedExperiment::rowData(spe_object)
  metadata_list <- S4Vectors::metadata(spe_object)
  
  # Coerce each assay to dgCMatrix if needed
  assays_list <- lapply(assays_list, function(x) {
    if (!inherits(x, "dgCMatrix")) {
      x <- as(x, "dgCMatrix")
    }
    return(x)
  })
  
  # Create SingleCellExperiment
  sce <- SingleCellExperiment::SingleCellExperiment(
    assays = assays_list,
    colData = col_data,
    rowData = row_data,
    metadata = metadata_list
  )
  
  return(sce)
}