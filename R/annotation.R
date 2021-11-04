##-- Common functions related to gene annotation.

#' @title Collapse a character vector by `","`.
#' @description Used as `multiVals` parameter in [AnnotationDbi::mapIds()].
#' @param x A character vector.
#' @return A character scalar.
#'
#' @concept sc_annotation
#' @export
collapse_ensembl_multivals <- function(x) {
  str_comma(x, space = FALSE)
}

#' @title Create a `dataframe` with annotation of SCE object's genes using an `AnnotationDbi` package.
#' @description
#' The following columns are retrieved from the `AnnotationDbi` package: `SYMBOL`, `GENENAME`, `ENTREZID`.
#' Multi-mapping entities are collapsed by "," (i.e. a single ENSEMBL ID having multiple symbols, etc.).
#' Unknown symbols are replaced by the corresponding ENSEMBL IDs.
#' @param sce A `SingleCellExperiment` object.
#' @param annotation_db_file A character scalar: path to SQLite file of annotation DB, e.g. `org.Hs.eg.db$conn@dbname`.
#' @return A `dataframe` with annotation.
#'
#' @concept sc_annotation
#' @export
make_gene_annotation <- function(sce, annotation_db_file) {
  genome_ann <- AnnotationDbi::loadDb(annotation_db_file)

  gene_annotation <- data.frame(
    ENSEMBL = rownames(sce),
    row.names = rownames(sce),
    stringsAsFactors = FALSE
  ) %>%
    dplyr::mutate(
      SYMBOL = AnnotationDbi::mapIds(
        genome_ann,
        keys = .data$ENSEMBL,
        column = "SYMBOL",
        keytype = "ENSEMBL",
        multiVals = collapse_ensembl_multivals
      ),
      SYMBOL = dplyr::if_else(is.na(.data$SYMBOL), .data$ENSEMBL, .data$SYMBOL),
      GENENAME = AnnotationDbi::mapIds(
        genome_ann,
        keys = .data$ENSEMBL,
        column = "GENENAME",
        keytype = "ENSEMBL",
        multiVals = collapse_ensembl_multivals
      ),
      ENTREZID = AnnotationDbi::mapIds(
        genome_ann,
        keys = .data$ENSEMBL,
        column = "ENTREZID",
        keytype = "ENSEMBL",
        multiVals = collapse_ensembl_multivals
      )
    ) %>%
    dplyr::select(.data$ENSEMBL, .data$SYMBOL, .data$ENTREZID, dplyr::everything()) %>%
    as.data.frame() %>%
    set_rownames(.$ENSEMBL)

  duplicated_gene_symbols <- duplicated(gene_annotation$SYMBOL) | duplicated(gene_annotation$SYMBOL, fromLast = TRUE)

  gene_annotation[duplicated_gene_symbols, "SYMBOL"] <- gluec(
    "{gene_annotation[duplicated_gene_symbols, 'SYMBOL']}_{rownames(gene_annotation[duplicated_gene_symbols, ])}"
  )

  return(gene_annotation)
}

#' @title Load a SQL database file and run a function from the `AnnotationDbi` package.
#' @description An `AnnotationDbi` object cannot be used in parallel, so this is a workaround.
#' @param annotation_db_file A character scalar: path to SQLite file of annotation DB, e.g. `org.Hs.eg.db$conn@dbname`.
#' @param dbi_fun A function from the `AnnotationDbi` package which accepts `AnnotationDbi` object as parameter `x`,
#'   e.g. [AnnotationDbi::mapIds()].
#' @param ... Parameters passed to `dbi_fun`.
#' @return An output from `dbi_fun`.
#'
#' @concept sc_annotation
#' @export
with_dbi <- function(annotation_db_file, dbi_fun, ...) {
  dbi <- AnnotationDbi::loadDb(annotation_db_file)
  rlang::exec(dbi_fun, x = dbi, !!!list(...))
}
