## -- Common functions related to highly variable genes (HVGs) selection.

#' @title Get top N highly variable genes (HVGs) by a specified metric.
#' @description Types of HVG metrics are specified in `02_norm_clustering.yaml` and `01_integration.yaml` configs.
#' See the corresponding section there.
#' @param sce_norm A `SingleCellExperiment` object with computed HVG metric.
#' @param hvg_metric_fit A `DataFrame` with HVG metric fit as produced by e.g. [scran::modelGeneVar()].
#' @param hvg_selection_value A numeric scalar: threshold value to select HVGs. This depends on `hvg_metric`.
#' @param hvg_metric A character scalar: type of HVG metric.
#'   If `sctransform` is used, HVGs are selected by the underlying method, and number of them is controlled by
#'   `SCT_N_HVG` parameter in `02_norm_clustering.yaml`.
#'   For the other metric types, see the `hvg_selection` and `hvg_selection_value` parameters.
#' @param hvg_selection A character scalar: method to use for selection of HVGs.
#'   This is only relevant when `hvg_metric` is "gene_var" or "gene_cv2".
#'   See <http://bioconductor.org/books/release/OSCA/feature-selection.html#hvg-selection> for more details.
#'
#' - `"top"`: Take top X genes according to a metric.
#'   `"bio"` and `"ratio"` columns are used for `hvg_metric` `"gene_var"` and `"gene_cv2"`, respectively.
#' - `"significance"`: Use FDR threshold.
#' - `"threshold"`: Use threshold on the minimum value of a metric.
#'   `"bio"` and `"ratio"` columns are used for `hvg_metric` of `"gene_var"` and `"gene_cv2"`, respectively.
#'
#' @return A character vector of HVG IDs (ENSEMBL).
#'
#' @concept sc_hvg
#' @export
get_top_hvgs <- function(sce_norm,
                         hvg_metric_fit,
                         hvg_selection_value,
                         hvg_metric = c("gene_var", "gene_cv2", "sctransform"),
                         hvg_selection = c("top", "significance", "threshold")) {
  hvg_metric <- arg_match(hvg_metric)
  hvg_selection <- arg_match(hvg_selection)

  if (hvg_metric == "sctransform") {
    hvg_ids <- sce_norm@metadata$sctransform_hvg_ids
  } else {
    var_field <- list(gene_var = "bio", gene_cv2 = "ratio")[[hvg_metric]]

    if (hvg_selection == "top") {
      hvg_ids <- scran::getTopHVGs(hvg_metric_fit, var.field = var_field, n = hvg_selection_value)
    } else if (hvg_selection == "significance") {
      hvg_ids <- scran::getTopHVGs(hvg_metric_fit, var.field = var_field, fdr.threshold = hvg_selection_value)
    } else if (hvg_selection == "threshold") {
      hvg_ids <- scran::getTopHVGs(hvg_metric_fit, var.field = var_field, var.threshold = hvg_selection_value)
    }
  }

  return(hvg_ids)
}

#' @title Identify cell cycle-related genes by using variance explained threshold.
#' @description For more details see the HVG selection section in `02_norm_clustering.yaml` config.
#' @param sce A `SingleCellExperiment` object.
#' @param var_expl_threshold A numeric scalar: threshold for variance explained.
#'   Features exceeding this threshold will be marked as CC-related.
#' @param variable A character scalar: column to use for variance explained computation.
#' @return A character vector of gene IDs marked as CC-related.
#'
#' @concept sc_hvg
#' @export
sce_get_cc_genes <- function(sce, var_expl_threshold, variable = "phase") {
  cc_genes_var_expl <- scater::getVarianceExplained(sce, variables = variable) %>% as.data.frame()
  to_remove <- cc_genes_var_expl$phase > var_expl_threshold

  if (any(to_remove)) {
    to_remove_ids <- rownames(sce)[to_remove]
    cli_alert_info("Found {length(to_remove_ids)} cell cycle-related genes.")
  } else {
    to_remove_ids <- c()
    cli_alert_info("No cell cycle-related genes exceeding the variance explained threshold were found.")
  }

  return(to_remove_ids)
}

#' @title Remove cell cycle-related genes from HVGs.
#' @param sce A `SingleCellExperiment` object.
#' @param var_expl_threshold A numeric scalar: threshold for variance explained.
#'   Genes exceeding this threshold will be marked as CC-related.
#' @param variable A character scalar: column to use for variance explained computation.
#' @param ... Passed to [get_top_hvgs()].
#' @return A `SingleCellExperiment` object with removed CC-related genes from HVGs.
#'
#' `is_cc_related` logical column is appended to `rowData(sce)`.
#'
#' The following items of `metadata(sce)` are added or modified:
#' - `hvg_cc_genes_var_expl_threshold`: the value of `var_expl_threshold`.
#' - `hvg_ids`: if CC-related genes are found, their IDs are removed.
#' - `hvg_rm_cc_genes_ids`: IDs of CC-related genes. If these are not found, an empty vector.
#'
#' Note that CC-related genes are not removed from `metadata(sce)$hvg_metric_fit`.
#'
#' @concept sc_hvg
#' @export
sce_remove_cc_genes <- function(sce,
                                var_expl_threshold,
                                variable = "phase",
                                ...) {
  sce <- sce_add_metadata(sce, hvg_rm_cc_genes = TRUE, hvg_cc_genes_var_expl_threshold = var_expl_threshold)
  cc_gene_ids <- sce_get_cc_genes(sce, var_expl_threshold, variable = variable)

  if (!is_empty(cc_gene_ids)) {
    cli_alert_info("Removing {length(cc_gene_ids)} cell cycle-related genes prior to HVG selection.")
    hvg_metric_fit <- metadata(sce)$hvg_metric_fit
    is_cc_related <- rownames(hvg_metric_fit) %in% cc_gene_ids
    rowData(sce) <- cbind(rowData(sce), is_cc_related = is_cc_related)
    hvg_ids <- get_top_hvgs(
      sce_norm = sce,
      hvg_metric_fit = hvg_metric_fit[!is_cc_related, ],
      ...
    )

    sce <- sce_add_metadata(sce, hvg_ids = hvg_ids, hvg_rm_cc_genes_ids = cc_gene_ids)
  } else {
    cli_alert_info("No cell cycle-related genes exceeding the variance explained threshold were found.")
    rowData(sce) <- cbind(rowData(sce), is_cc_related = rep(FALSE, nrow(sce)))
    sce <- sce_add_metadata(sce, hvg_rm_cc_genes_ids = c())
  }

  return(sce)
}

#' @title Plot average expression vs. HVG metrics (total, bio, and technical variance) and highlight HVGs.
#' @param sce A `SingleCellExperiment` object with HVG metadata.
#' @param ... Currently not used.
#' @return A `ggplot2` object if `hvg_metric` is `sctransform`, otherwise a `patchwork` object.
#'
#' @concept sc_hvg
#' @export
plot_hvg <- function(sce, ...) {
  hvg_metric <- metadata(sce)$hvg_metric
  assert_that_(!is_null(hvg_metric), msg = "{.code metadata(sce)$hvg_metric} is {.code NULL} - HVG selection has not been run")
  assert_that_(hvg_metric %in% c("gene_var", "gene_cv2", "sctransform"), msg = "Unknown {.code metadata(sce)$hvg_metric}: {.val {hvg_metric}}")
  hvg_metric_fit <- metadata(sce)$hvg_metric_fit
  is_hvg <- rowData(sce)[rownames(hvg_metric_fit), "is_hvg", drop = TRUE]

  if (hvg_metric == "sctransform") {
    # seu <- as_seurat(sce, assay = "SCT")
    # p <- VariableFeaturePlot(seu, selection.method = "sct") +
    #   ggtitle("HVGs selected by sctransform", subtitle = NULL) +
    #   guides(color = ggplot2::guide_legend(override.aes = list(size = 3)))

    to_plot <- hvg_metric_fit[, c("gmean", "residual_variance")] %>% as.data.frame()
    p <- utils::getFromNamespace("SingleCorPlot", "Seurat")(data = to_plot, col.by = is_hvg, pt.size = 1) +
      labs(title = "HVGs selected by sctransform", subtitle = NULL, x = "Geometric Mean of Expression", y = "Residual Variance") +
      ggplot2::scale_x_continuous(trans = "log10") +
      ggplot2::scale_color_manual(
        labels = str_space(c("not HVG", "HVG"), "count:", table(is_hvg), collapse = FALSE),
        values = c("black", "red")
      )
  } else {
    if (hvg_metric == "gene_var") {
      var_fields <- c("total", "bio", "tech")
    } else {
      var_fields <- c("total")
    }

    plots <- lapply(var_fields, FUN = function(var_field) {
      to_plot <- hvg_metric_fit[, c("mean", var_field)] %>% as.data.frame()

      ## -- TODO: Remove outliers by mean expression.
      # outliers <- boxplot.stats(to_plot$mean)$out

      utils::getFromNamespace("SingleCorPlot", "Seurat")(data = to_plot, col.by = is_hvg, pt.size = 1) +
        labs(
          title = NULL,
          subtitle = NULL,
          x = "Average expression",
          y = glue("{stringr::str_to_sentence(var_field)} variance")
        ) +
        ggplot2::scale_x_continuous(trans = "log10") +
        ggplot2::scale_color_manual(
          labels = str_space(c("not HVG", "HVG"), "count:", table(is_hvg), collapse = FALSE),
          values = c("black", "red")
        )
    })

    p <- patchwork::wrap_plots(plots, nrow = 1, guides = "collect") +
      patchwork::plot_annotation(
        title = glue("HVGs selected by {hvg_metric}"),
        subtitle = glue("HVG selection '{sce@metadata$hvg_selection}' with value {sce@metadata$hvg_selection_value}")
      ) &
      theme(
        legend.position = "top",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)
      ) &
      guides(color = ggplot2::guide_legend(override.aes = list(size = 3)))
  }

  return(p)
}

#' @title Plot fit of a HVG metric and highlight HVGs.
#' @param sce A `SingleCellExperiment` object with HVG metadata.
#' @param y A character scalar: which column from HVG fit `DataFrame` to plot on the y-axe.
#'   `var` or `cv2` if `hvg_metric` is `gene_var` or `gene_cv2`, respectively.
#' @return Invisibly `NULL` (drawing on a graphical device).
#'
#' @concept sc_hvg
#' @export
plot_hvg_fit <- function(sce, y = c("var", "cv2")) {
  y <- arg_match(y)

  hvg_fit <- metadata(sce)$hvg_metric_fit
  hvg_fit_metadata <- metadata(hvg_fit)
  is_hvg <- rowData(sce)$is_hvg

  plot(hvg_fit_metadata$mean, hvg_fit_metadata[[y]], log = "xy", xlab = "Mean", ylab = y)
  graphics::points(hvg_fit_metadata$mean[is_hvg], hvg_fit_metadata[[y]][is_hvg], col = "red")
  ## -- To avoid R CMD CHECK note "no visible binding for global variable 'x'".
  x <- NULL
  graphics::curve(hvg_fit_metadata$trend(x), col = "dodgerblue", add = TRUE, lwd = 2)
  return(invisible(NULL))
}
