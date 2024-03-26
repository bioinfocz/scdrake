## -- Common functions related to cluster markers and differential expression (contrasts).

assert_column_in_cell_data <- function(source_column, cell_data, marker_source_name) {
  assert_that_(
    source_column %in% colnames(cell_data),
    msg = str_space(
      "{.field source_column} {.val {source_column}} of marker source {.val {marker_source_name}}",
      "is not present in {.var cell_data} target. Check {.field CLUSTER_MARKERS_SOURCES}, {.field CONTRAST_SOURCES} and",
      "{.field CELL_GROUPINGS} in cluster_markers, contrasts and 02_norm_clustering configs, respectively."
    )
  )
}

#' @title Compute cell cluster markers.
#' @description This is a wrapper around [scran::findMarkers()]. Depending on test type, `stats.*` columns
#' in returned `DataFrame` are replaced by either `lfc_*` or `auc_*`.
#' `DataFrame`s with results are appended to the `params` tibble, but because [scran::findMarkers()] returns
#' `DataFrame` for each level of a factor, those are unnested in the returned tibble.
#' That means for each row of `params` you will get result (row) for each tested level.
#' @param sce A `SingleCellExperiment` object with normalized log-expression values and the following `rowData()`
#'   columns: `"ENSEMBL"`, `"ENTREZID"`, `"SYMBOL"`, `"GENENAME"`.
#' @param params A tibble. See `cluster_markers_test_params` or `contrasts_test_params` target for details.
#' @param markers_type A character scalar: type of markers to get. For `"contrast"`, the following parameters are
#' always used in [scran::findMarkers()]:
#' - `direction = "any"`
#' - `pval.type = "any"`
#' - `min.prop = NULL`
#' @return A tibble.
#'
#' @concept sc_markers
#' @export
scran_markers <- function(sce, params, markers_type = c("global", "contrast")) {
  markers_type <- arg_match(markers_type)

  params <- lapply_rows(params, FUN = function(par) {
    par_ <- par
    par <- replace_list_nas_with_nulls(par)

    if (par$test_type == "wilcox") {
      output_field <- "auc_"
    } else {
      output_field <- "lfc_"
    }

    findMarkers_ <- methods::getMethod(scran::findMarkers, signature = "SingleCellExperiment")

    if (markers_type == "global") {
      cli_alert_info(glue("{par$name} ({par$source_column}), {par$test_type} test"))
      args <- list(
        x = sce,
        direction = par$lfc_direction,
        lfc = par$lfc_test,
        groups = par$groups,
        test.type = par$test_type,
        block = par$blocks,
        pval.type = par$pval_type,
        min.prop = par$min_prop,
        std.lfc = par$std_lfc,
        log.p = FALSE,
        full.stats = TRUE,
        row.data = rowData(sce)[, c("ENSEMBL", "ENTREZID", "SYMBOL", "GENENAME")],
        add.summary = TRUE
      )
    } else {
      cli_alert_info(glue("{par$source_column}, {par$test_type} test"))
      args <- list(
        x = sce,
        direction = "any",
        lfc = par$lfc_test,
        groups = par$groups,
        block = par$blocks,
        test.type = par$test_type,
        pval.type = "any",
        # min.prop = min_prop,
        std.lfc = par$std_lfc,
        log.p = FALSE,
        full.stats = TRUE,
        row.data = rowData(sce)[, c("ENSEMBL", "ENTREZID", "SYMBOL", "GENENAME")],
        add.summary = TRUE
      )
    }

    if (args$test.type != "t") {
      args$std.lfc <- NULL
    }

    markers <- do.call(findMarkers_, args = args, quote = TRUE)

    ## -- Replace stats.* by lfc_* or auc_* (depends on the test type).
    par_$markers <- lapply(markers, FUN = function(res_level) {
      colnames(res_level) <- stringr::str_replace(colnames(res_level), "^stats\\.", output_field)
      res_level[[glue("{output_field}summary")]] <- res_level$summary.stats
      res_level$summary.stats <- NULL
      return(res_level)
    })

    return(par_)
  })

  ## -- Make a row for each pairwise test result.
  params <- params %>%
    tidyr::unnest(.data$markers, names_repair = "minimal") %>%
    dplyr::mutate(
      group_level = names(.data$markers),
      id = str_c(.data$name, .data$test_type, .data$group_level, sep = "_"),
      markers = set_names(.data$markers, .data$id)
    )

  return(params)
}

#' @title Filter a dataframe with cluster markers.
#' @param markers A dataframe-like object.
#' @param top_n A numeric scalar: number of top markers to keep.
#' @param top_n_wt A character scalar: name of column in `markers` by which top markers will be determined:
#'   - `"top"`: the minimum rank across all pairwise comparisons. Only possible if markers were computed
#'     with `pval.type="any"`.
#'   - `"fdr"`: False Discovery Rate
#'   - `"lfc"`: log2 fold change
#'   - `"auc"`: Area Under Curve. Only possible if marker test was `"wilcox"`.
#' @param distinct_by A character scalar: name of column in `markers` by which unique rows will be kept.
#' @return A filtered `markers` object.
#'
#' @concept sc_markers
#' @export
filter_markers <- function(markers, top_n, top_n_wt = c("top", "fdr", "lfc", "auc"), distinct_by = NULL) {
  top_n_wt <- arg_match(top_n_wt)
  markers <- as.data.frame(markers)

  if (top_n_wt == "top") {
    markers_top <- dplyr::filter(markers, .data$Top <= top_n)
  } else if (top_n_wt == "fdr") {
    markers_top <- dplyr::slice_min(markers, order_by = .data$FDR, n = top_n)
  } else if (top_n_wt == "lfc") {
    if (all(markers$lfc_summary <= 0)) {
      markers_top <- dplyr::slice_min(markers, order_by = -.data$lfc_summary, n = top_n)
    } else if (all(markers$lfc_summary >= 0)) {
      markers_top <- dplyr::slice_max(markers, order_by = .data$lfc_summary, n = top_n)
    } else {
      markers_top <- dplyr::slice_max(markers, order_by = -abs(.data$lfc_summary), n = top_n * 2)
    }
  } else if (top_n_wt == "auc") {
    markers_top <- dplyr::slice_max(markers, order_by = .data$auc_summary, n = top_n)
  }

  if (!is_null(distinct_by)) {
    markers_top <- dplyr::distinct(markers_top, !!sym(distinct_by), .keep_all = TRUE)
  }

  return(markers_top)
}

#' @title Make a heatmap of global or contrast markers.
#' @param seu A `Seurat` object.
#' @param params A tibble. See `cluster_markers_heatmap_params` or `contrasts_heatmap_params` target for details.
#' @param marker_type A character scalar.
#' @param save A logical scalar. If `TRUE`, the heatmap will be saved to `params$out_file`.
#' @param return_type A character scalar. For `"file"`, return `params$out_file`.
#'   For `"tibble"`, return a modified `params` tibble with appended list columns:
#'   - `heatmaps`: contains named lists of heatmap objects (`p_heatmap`, `p_heatmap_z_score`)
#'   - `markers_top`: dataframe with top markers used for heatmap generation.
#' @return See the `return_type` argument.
#'
#' @concept sc_markers
#' @export
marker_heatmaps_wrapper <- function(seu,
                                    params,
                                    marker_type = c("global", "contrast"),
                                    save = TRUE,
                                    return_type = c("file", "tibble")) {
  marker_type <- arg_match(marker_type)
  return_type <- arg_match(return_type)

  params <- lapply_rows(params, FUN = function(par) {
    subtitle <- glue("{par$test_label}")

    if (!is.na(par$block_column)) {
      subtitle <- glue("{subtitle} (blocking on {par$block_column})")
    }

    if (marker_type == "global") {
      title <- glue("{par$description}\nMarkers of cluster {par$group_level}")

      if (par$top_n_wt_heatmap == "top") {
        subtitle <- glue("{subtitle}, top {par$top_n_heatmap} markers from each pairwise comparison")
      } else {
        subtitle <- glue("{subtitle}, top {par$top_n_heatmap} markers by {par$top_n_wt_heatmap}")
      }
    } else {
      title <- glue("{par$description}\nContrast {par$contrast_name} (target: {par$target}, reference: {par$reference})")

      if (par$top_n_wt_heatmap == "top") {
        cli_alert_warning("top_n_wt_heatmap 'top' is not possible for contrast heatmaps -> changing to 'fdr'")
        par$top_n_wt_heatmap <- "fdr"
      }

      subtitle <- glue("{subtitle}, top {par$top_n_heatmap} DEGs by {par$top_n_wt_heatmap}")

      which_cells_contrast <- which(par$groups %in% c(par$target, par$reference))
      par$groups <- par$groups[which_cells_contrast] %>% droplevels()
      seu <- seu[, which_cells_contrast]
      seu@assays$RNA@scale.data <- t(scale(t(seu@assays$RNA@data)))
    }

    p_heatmap <- marker_heatmap(
      seu = seu,
      markers = par$markers,
      groups = par$groups,
      assay = "RNA",
      slot = "data",
      top_n = par$top_n_heatmap,
      top_n_wt = par$top_n_wt_heatmap,
      title = title,
      subtitle = subtitle
    )

    p_heatmap_zscore <- marker_heatmap(
      seu = seu,
      markers = par$markers,
      groups = par$groups,
      assay = "RNA",
      slot = "scale.data",
      top_n = par$top_n_heatmap,
      top_n_wt = par$top_n_wt_heatmap,
      title = title,
      subtitle = subtitle,
      fill_scale = ggplot2::scale_fill_gradient2(
        low = "#67a9cf", mid = "#f7f7f7", high = "#ef8a62", midpoint = 0, name = "z-score"
      )
    )

    if (save) {
      out_dir <- fs::path_dir(par$out_file)
      fs::dir_create(out_dir, recurse = TRUE)

      if (nrow(p_heatmap$markers_top) >= 50) {
        height <- (7 / 50) * nrow(p_heatmap$markers_top)
      } else {
        height <- NULL
      }

      grDevices::pdf(par$out_file, useDingbats = FALSE, height = height)
      print(p_heatmap$p_heatmap)
      print(p_heatmap_zscore$p_heatmap)
      grDevices::dev.off()
    }

    par$heatmaps <- list(p_heatmap = p_heatmap$p_heatmap, p_heatmap_zscore = p_heatmap_zscore$p_heatmap)
    par$markers_top <- p_heatmap$markers_top
    par$markers <- NULL

    return(par)
  })

  names(params$heatmaps) <- params$id

  if (return_type == "file") {
    return(params$out_file)
  } else if (return_type == "tibble") {
    return(params)
  }
}

#' @title Make a heatmap of cell clusters.
#' @param seu A `Seurat` object.
#' @param markers A dataframe with markers (returned from [scran_markers()]) or a character vector of gene SYMBOLs.
#' @param groups A character vector: assignment of cells to groups (clusters).
#' @param assay,slot,angle,... Passed to [Seurat::DoHeatmap()].
#' @param top_n,top_n_wt Passed to [filter_markers()]. If `top_n` is `NULL`, all markers in `markers` will be used
#'   (`SYMBOL` column in case `markers` is a dataframe-like object).
#'   If `top_n` is numeric, `markers` must be a dataframe-like object.
#' @param title A character scalar: heatmap title.
#' @param subtitle A character scalar: heatmap subtitle.
#' @param fill_scale A `Scale` object from `ggplot2`,
#'   e.g. `ggplot2::scale_fill_gradient(low = "white", high = "red")` (default).
#' @param y_text_size A numeric scalar: text size on y-axe of the heatmap.
#' @return A named list.
#'
#' @concept sc_markers
#' @export
marker_heatmap <- function(seu,
                           markers,
                           groups,
                           assay = "RNA",
                           slot = "data",
                           top_n = 5,
                           top_n_wt = "fdr",
                           title = "Heatmap",
                           subtitle = "",
                           fill_scale = ggplot2::scale_fill_gradient(low = "white", high = "red"),
                           y_text_size = 8,
                           angle = 0,
                           ...) {
  # if (inherits(assay(sce, assay), "ResidualMatrix")) {
  #   assay(sce, "logcounts") <- assay(sce, "logcounts") %>% as.matrix()
  # }

  gene_annotation <- seu@assays[[assay]]@meta.features
  seu <- Seurat::SetIdent(seu, value = groups)
  rownames(seu@assays[[assay]]@counts) <- gene_annotation$SYMBOL
  rownames(seu@assays[[assay]]@data) <- gene_annotation$SYMBOL
  rownames(seu@assays[[assay]]@meta.features) <- gene_annotation$SYMBOL

  if (nrow(seu@assays[[assay]]@scale.data) > 0) {
    rownames(seu@assays[[assay]]@scale.data) <- gene_annotation$SYMBOL
  }

  if (!is_null(top_n)) {
    assert_that_(dplyr::between(top_n, 1, Inf), msg = "{.var top_n} must be between 1 and Inf")
    markers_top <- filter_markers(markers = markers, top_n = top_n, top_n_wt = top_n_wt)
  } else {
    if (is_character(markers)) {
      markers_top <- data.frame(SYMBOL = markers)
    } else {
      markers_top <- markers
    }
  }

  if (is_empty(markers_top$SYMBOL)) {
    p_heatmap <- create_dummy_plot(glue0("{title}\n{subtitle}\nNo top markers were found."))
  } else {
    p_heatmap <- suppressMessages(
      Seurat::DoHeatmap(seu, features = markers_top$SYMBOL, assay = assay, slot = slot, angle = angle, ...) +
        fill_scale +
        ggtitle(label = title, subtitle = subtitle) +
        theme(
          axis.text.y = element_text(size = y_text_size),
          title = element_text(size = 8),
          plot.subtitle = element_text(size = 7)
        )
    )
  }

  return(list(p_heatmap = p_heatmap, markers_top = markers_top))
}

#' @title Get a tibble of top markers extracted from each test result.
#' @description This is basically an unnesting of filtered `markers` in each row of `markers_processed`.
#' The filtering is based on `top_n_plot` and `top_n_wt_plot` columns in `markers_plot_params`.
#' @param markers_processed A tibble. See `cluster_markers_processed` or `contrasts` target for details.
#' @param markers_plot_params A tibble. See `cluster_markers_plot_params` or `contrasts_plot_params` for details.
#' @param out_dir A character scalar: path to base output directory for plots.
#'   Subdirectories and plot files will be created as
#'   `<clustering_name>/<dimred>/<ENSEMBL>_<SYMBOL>_<clustering_name>_<dimred>.pdf`.
#' @param marker_type A character scalar.
#' @return A tibble. See `cluster_markers_plots_top` or `contrasts_plots_top` target for details.
#'
#' @concept sc_markers
#' @export
markers_plots_top <- function(markers_processed, markers_plot_params, out_dir, marker_type = c("global", "contrast")) {
  marker_type <- arg_match(marker_type)
  markers_processed <- dplyr::select(markers_processed, .data$source_column, .data$test_type, .data$markers)

  if (marker_type == "global") {
    markers_plot_params <- dplyr::select(markers_plot_params, -.data$name)
  }

  res <- dplyr::left_join(markers_processed, markers_plot_params, by = c("source_column", "test_type")) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      markers_top = list(
        filter_markers(markers = .data$markers, top_n = .data$top_n_plot, top_n_wt = .data$top_n_wt_plot) %>%
          dplyr::select(dplyr::all_of(c("ENSEMBL", "SYMBOL", "ENTREZID", "GENENAME")))
      ),
      markers = NULL
    ) %>%
    dplyr::ungroup() %>%
    tidyr::unnest(.data$markers_top, names_repair = "minimal") %>%
    dplyr::distinct(.data$source_column, .data$plot_dimreds, .data$ENSEMBL, .keep_all = TRUE) %>%
    dplyr::select(dplyr::all_of(c("source_column", "plot_dimreds", "ENSEMBL", "SYMBOL", "groups"))) %>%
    dplyr::mutate(
      out_file = fs::path(
        .env$out_dir,
        .data$source_column,
        .data$plot_dimreds,
        glue("{ENSEMBL}_{SYMBOL}_{source_column}_{plot_dimreds}.pdf")
      )
    )

  return(res)
}

#' @title Make a marker plot.
#' @description A marker plot is composed of:
#' - Two plots of a dimred: one colored by a clustering, and second colored by expression of a marker (feature plot).
#' - A plot of summarized marker expression: proportion of cells expressing a marker, colored by average expression.
#' - A violin plot: expression of a marker in each cluster.
#' @param sce A `SingleCellExperiment` object.
#' @param dimred_name A character scalar.
#' @param gene_ensembl_id A character scalar. ENSEMBL ID of a marker.
#' @param group
#' - A character scalar: name of column in `colData(sce)` by which are cells separated (e.g. clustering).
#' - A character vector or factor: assignment of cells to groups (clusters). Must have the same length as the number
#'   of cells (columns) in `sce`.
#' @param cluster_plot_title,cluster_plot_subtitle,cluster_plot_legend_title,cluster_plot_label
#'   A character scalar. Applied to the dimred plot which is colored by `group`.
#' @param cluster_plot_point_size A numeric scalar.
#' @param feature_plot_point_size A numeric scalar. Applied to the feature plot.
#' @param vln_plot_legend_title A character scalar. Applied to the violin plot.
#' @return A `patchwork` object.
#'
#' @concept sc_markers
#' @export
marker_plot <- function(sce,
                        dimred_name,
                        gene_ensembl_id,
                        group,
                        cluster_plot_title = NULL,
                        cluster_plot_subtitle = NULL,
                        cluster_plot_legend_title = NULL,
                        cluster_plot_label = TRUE,
                        cluster_plot_point_size = 1,
                        feature_plot_point_size = 1,
                        vln_plot_legend_title = NULL) {
  # if (inherits(assay(sce, "logcounts"), "ResidualMatrix")) {
  #   assay(sce, "logcounts") <- assay(sce, "logcounts") %>% as.matrix()
  # }

  if (is_scalar_character(group)) {
    assert_that_(group %in% colnames(colData(sce)), msg = "Column {.val {group}} not found in {.code colnames(colData(sce))}")
    group_name <- group
  } else {
    sce <- sce_add_colData(sce, df = data.frame(group = group))
    group_name <- "group"
  }

  if (cluster_plot_label) {
    cluster_plot_label <- group_name
  } else {
    cluster_plot_label <- NULL
  }

  if (is_null(vln_plot_legend_title)) {
    vln_plot_legend_title <- group_name
  }

  gene_annotation <- rowData(sce)[gene_ensembl_id, ]
  gene_symbol <- gene_annotation$SYMBOL
  gene_title <- glue("{gene_symbol} ({gene_ensembl_id})")

  p_clusters <- plotReducedDim_mod(
    sce,
    dimred = dimred_name,
    colour_by = group_name,
    text_by = cluster_plot_label,
    title = cluster_plot_title,
    subtitle = cluster_plot_subtitle,
    legend_title = cluster_plot_legend_title,
    point_size = cluster_plot_point_size
  ) +
    theme(
      legend.position = "top",
      legend.justification = "center",
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    )

  p_expression <- plotReducedDim_mod(
    sce,
    dimred = dimred_name,
    colour_by = gene_ensembl_id,
    title = gene_title
  ) +
    guides(color = ggplot2::guide_colorbar(barwidth = 5, title.vjust = 0.9)) +
    labs(color = "log2(expression)") +
    p_clusters$theme

  p_dot <- scater::plotDots(
    sce,
    features = gene_ensembl_id,
    group = group_name,
    color = c("lightgrey", "blue")
  ) +
    guides(color = ggplot2::guide_colorbar(barwidth = 8, title.vjust = 0.9)) +
    labs(title = gene_title, x = NULL) +
    p_clusters$theme +
    theme(
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.line.y = element_blank(),
      legend.justification = "center"
    )

  p_vln <- plot_vln(
    sce,
    ensembl_id = gene_ensembl_id,
    groups = group_name,
    labs = labs(title = NULL, x = vln_plot_legend_title, y = "log2(expression)", fill = vln_plot_legend_title)
  ) +
    guides(color = "none") +
    p_clusters$theme

  p <- (p_clusters | p_expression) / p_dot / p_vln
  p <- p + patchwork::plot_layout(height = c(1, 0.25, 1))

  return(p)
}

#' @title Make plots of top markers.
#' @param sce_dimred A `SingleCellExperiment` object with computed dimreds.
#' @param markers_plots_top A tibble, output from [markers_plots_top()].
#'   See `cluster_markers_plots_top` or `contrasts_plots_top` target for details.
#' @param save A logical scalar: if `TRUE`, save each plot to `markers_top$out_file`.
#' @param dry A logical scalar: if `TRUE`, do not make plots and just create empty files.
#' @param return_type A character scalar:
#'   - `"file"`: return `markers_top$out_file`.
#'   - `"plot"`: return list of marker plots.
#'   - `"tibble"`: return a modified `markers_plots_top` tibble with appended `marker_plot` column (list of marker plots).
#' @return See the `return_type` argument.
#'   See `cluster_markers_plots_files` or `contrasts_plots_files` target for details.
#'
#' @seealso [marker_plot()]
#'
#' @concept sc_markers
#' @export
markers_plots_files <- function(sce_dimred, markers_plots_top, save = TRUE, dry = FALSE, return_type = c("file", "plot", "tibble")) {
  return_type <- arg_match(return_type)

  markers_plots_top <- lapply_rows(markers_plots_top, FUN = function(par) {
    if (!dry) {
      p <- suppressMessages(marker_plot(
        sce_dimred,
        dimred_name = par$plot_dimreds,
        gene_ensembl_id = par$ENSEMBL,
        group = par$groups,
        cluster_plot_title = par$source_column,
        # cluster_plot_legend_title = source_column,
        cluster_plot_legend_title = "",
        # vln_plot_legend_title = source_column
        vln_plot_legend_title = "",
      ))

      par$marker_plot <- p
    } else {
      par$marker_plot <- NA
      # p <- ggplot() +
      #   ggplot2::theme_void() +
      #   ggplot2::geom_text(aes(x = 0, y = 0, label = "Marker plots were disabled."))
    }

    # par$marker_plot <- p

    if (save) {
      out_dir <- fs::path_dir(par$out_file)
      fs::dir_create(out_dir, recurse = TRUE)

      if (dry) {
        fs::file_create(par$out_file)
      } else {
        ggplot2::ggsave(par$out_file, p, device = "pdf", width = 10, height = 10, useDingbats = FALSE)
      }
    }

    return(par)
  })

  names(markers_plots_top$marker_plot) <- markers_plots_top$ENSEMBL

  if (return_type == "file") {
    return(markers_plots_top$out_file)
  } else if (return_type == "plot") {
    return(markers_plots_top$marker_plot)
  } else if (return_type == "tibble") {
    return(markers_plots_top)
  }
}

#' @title Make dimred plots for each marker source defined in `CLUSTER_MARKERS_SOURCES` (`cluster_markers.yaml` config)
#' or `CONTRASTS_SOURCES` (`contrasts.yaml` config).
#' @param sce_final_norm_clustering A `SingleCellExperiment` object with computed dimreds and clusterings.
#' @param markers_dimred_plots_params A tibble. See `cluster_markers_dimred_plot_params` or
#'   `contrasts_dimred_plot_params` for details.
#' @return A modified `markers_dimred_plots_params` tibble with appended `plot` column (list of `ggplot2` objects).
#'   See `cluster_markers_dimred_plots` or `contrasts_dimred_plots` target for details.
#'
#' @concept sc_markers
#' @export
markers_dimred_plots <- function(sce_final_norm_clustering, markers_dimred_plots_params) {
  markers_dimred_plots_params <- lapply_rows(markers_dimred_plots_params, FUN = function(par) {
    p <- plotReducedDim_mod(
      sce_final_norm_clustering,
      dimred = par$plot_dimreds,
      colour_by = par$source_column,
      title = glue("{par$source_column} | {str_to_upper(par$plot_dimreds)}"),
      use_default_ggplot_palette = TRUE,
      legend_title = par$source_column,
      text_by = par$source_column
    ) + theme(legend.position = "bottom", legend.direction = "horizontal")

    par$plot <- list(p)
    return(par)
  })

  names(markers_dimred_plots_params$plot) <- glue(
    "{markers_dimred_plots_params$source_column}_{markers_dimred_plots_params$plot_dimreds}"
  )

  return(markers_dimred_plots_params)
}

#' @title Save dimred plots.
#' @param markers_dimred_plots A tibble. See `cluster_markers_dimred_plots` or `contrasts_dimred_plots` for details.
#' @param markers_dimred_plots_base_out_dir A character scalar: path to base output directory to save dimred plots in.
#' @return A modified `markers_dimred_plots` tibble with appended `out_file` column.
#'   See `cluster_markers_dimred_plots_files` or `contrasts_dimred_plots_files` target for details.
#'
#' @concept sc_markers
#' @export
markers_dimred_plots_files <- function(markers_dimred_plots, markers_dimred_plots_base_out_dir) {
  markers_dimred_plots <- lapply_rows(markers_dimred_plots, FUN = function(par) {
    out_file <- fs::path(
      markers_dimred_plots_base_out_dir,
      par$source_column,
      glue("{par$source_column}_{par$plot_dimreds}.pdf")
    )
    fs::dir_create(fs::path_dir(out_file))
    ggplot2::ggsave(out_file, par$plot, device = "pdf", useDingbats = FALSE)
    par$out_file <- out_file
    return(par)
  })

  return(markers_dimred_plots)
}

#' @title Convert specified columns of a marker table to HTML links.
#' @param df A dataframe-like object.
#' @param links A list of named lists. Each named list has three items:
#'   - `colname_target`: which column in `df` will be converted to link.
#'   - `colname_source`: which column in `df` will be used as data source.
#'   - `template_url`: URL template for link where `{id}` will be replaced by values in `colname_source` column of `df`.
#'     For `ENSEMBL` column is also used the `ensembl_species` parameter (see the default value of `links`).
#' @param ensembl_species A character scalar: ID/name of ENSEMBL species.
#' @return A modified `df`.
#'
#' @concept sc_markers
#' @export
add_marker_table_links <- function(df,
                                   links = list(
                                     list(
                                       colname_target = "ENSEMBL",
                                       colname_source = "ENSEMBL",
                                       template_url = "https://www.ensembl.org/{ensembl_species}/Gene/Summary?g={id}"
                                     ),
                                     list(
                                       colname_target = "ENTREZID",
                                       colname_source = "ENTREZID",
                                       template_url = "https://www.ncbi.nlm.nih.gov/gene/{id}"
                                     ),
                                     list(
                                       colname_target = "SYMBOL",
                                       colname_source = "SYMBOL",
                                       template_url = "https://www.genecards.org/cgi-bin/carddisp.pl?gene={id}"
                                     )
                                   ),
                                   ensembl_species = "Homo_sapiens") {
  # df_orig <- df %>%
  # dplyr::mutate(SYMBOL = dplyr::if_else(is.na(SYMBOL), ENSEMBL, SYMBOL))

  df_orig <- df %>%
    dplyr::mutate(SYMBOL = dplyr::if_else(.data$ENSEMBL == .data$SYMBOL, "", .data$SYMBOL))

  for (link in links) {
    colname_target <- link$colname_target
    colname_source <- link$colname_source

    id <- df_orig[, colname_source] %>%
      stringr::str_remove_all(" ")
    urls <- dplyr::if_else(
      !is.na(id),
      gluec(link$template_url, id = id, ensembl_species = ensembl_species),
      NA_character_
    )
    url_texts <- df_orig[, colname_target]
    df[[colname_target]] <- dplyr::if_else(
      !is.na(url_texts),
      gluec("<a href='{urls}' target='_blank'>{url_texts}</a>"),
      NA_character_
    )
  }

  return(df)
}

#' @title Make a table used in HTML report of marker results.
#' @description It combines marker statistics, parameters, dimred plots, and links to heatmaps.
#' @param markers_out A tibble. See `cluster_markers_out` or `contrasts_out` target for details.
#' @param markers_heatmaps_df A tibble. See `cluster_markers_heatmaps_df` or `contrasts_heatmaps_df` for details.
#' @param markers_plots_top A tibble.
#'   See `cluster_markers_plots_top` or `contrasts_plots_top` for details.
#' @param markers_dimred_plots_files A tibble.
#'   See `cluster_markers_dimred_plots_files` or `contrasts_dimred_plots_files` for details.
#' @param out_dir A character scalar: path to base output directory to save HTML tables in.
#'   Files and subdirectories will be created as:
#'   - `<source_name>/<source_name>_<test_type>_<group_level>.html` for cluster markers.
#'     Example: `markers_cluster_kmeans_k4/markers_cluster_kmeans_k4_t_1.html`
#'   - `<source_name>/<source_name>_<test_type>_<target_level>_vs_<reference_level>.html` for contrasts.
#'     Example: `dea_cluster_louvain_annotated/dea_cluster_louvain_annotated_binom_cl3_vs_cl4.html`
#' @param ensembl_species A character scalar: passed to [add_marker_table_links()].
#' @param ... Currently not used.
#' @return A tibble. See `cluster_markers_for_tables` or `contrasts_for_tables` for details.
#'
#' @concept sc_markers
#' @export
markers_for_tables <- function(markers_out, markers_heatmaps_df, markers_plots_top, markers_dimred_plots_files, out_dir,
                               ensembl_species = "Homo_sapiens",
                               ...) {
  markers_heatmaps_df <- markers_heatmaps_df %>%
    dplyr::select(.data$id, .data$description, .data$test_label, heatmap_file = .data$out_file)

  markers_plots_top <- markers_plots_top %>%
    dplyr::select(.data$source_column, .data$plot_dimreds, .data$ENSEMBL, .data$out_file) %>%
    dplyr::group_by(.data$source_column, .data$ENSEMBL) %>%
    dplyr::summarise(
      source_column = dplyr::first(.data$source_column),
      ENSEMBL = dplyr::first(.data$ENSEMBL),
      plot_dimreds = list(.data$plot_dimreds),
      out_file = list(.data$out_file)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(.data$source_column) %>%
    tidyr::nest(marker_plots = -.data$source_column) %>%
    dplyr::ungroup()

  markers_dimred_plots_files <- markers_dimred_plots_files %>%
    dplyr::group_by(.data$source_column) %>%
    tidyr::nest(dimred_plots = c(.data$plot_dimreds, .data$plot, .data$out_file)) %>%
    dplyr::ungroup()

  res <- dplyr::left_join(markers_out, markers_heatmaps_df, by = "id") %>%
    dplyr::left_join(markers_dimred_plots_files, by = "source_column") %>%
    dplyr::left_join(markers_plots_top, by = "source_column") %>%
    dplyr::mutate(
      markers = purrr::map2(.data$markers, .data$marker_plots, ~ dplyr::left_join(.x, .y, by = "ENSEMBL")),
      out_file = fs::path(out_dir, .data$name, glue("{id}.html"))
    ) %>%
    dplyr::mutate(markers = purrr::map(.data$markers, ~ add_marker_table_links(., ensembl_species = ensembl_species))) %>%
    dplyr::select(-dplyr::any_of(c("test_params_name", "groups", "blocks", "marker_plots")))

  return(res)
}

#' @title Prepare a table with markers and render a HTML report from RMarkdown template.
#' @param markers_for_tables A tibble. File paths in column `out_file` will be used to save rendered HTML files.
#'   See `cluster_markers_for_tables` or `contrasts_for_tables` target for details.
#' @param rmd_template A character scalar: path to RMarkdown file.
#' @param marker_type A character scalar.
#' @param drake_cache_dir A character scalar: path to `drake` cache directory.
#' @return A character vector: `markers_for_tables$out_file`.
#'
#' @concept sc_markers
#' @export
markers_table_files <- function(markers_for_tables,
                                rmd_template,
                                marker_type = c("global", "contrast"),
                                drake_cache_dir = ".drake") {
  marker_type <- arg_match(marker_type)

  markers_for_tables <- lapply_rows(markers_for_tables, FUN = function(par) {
    par$markers <- par$markers %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        plot = purrr::map2_chr(
          .data$out_file, .data$plot_dimreds,
          function(.x, .y) {
            if (is.na(.x)) {
              ""
            } else {
              create_a_link(.x, str_to_upper(.y), href_rel_start = fs::path_dir(par$out_file))
            }
          }
        ) %>%
          str_comma(),
        out_file = NULL,
        plot_dimreds = NULL
      ) %>%
      dplyr::ungroup() %>%
      dplyr::select(.data$ENSEMBL, .data$ENTREZID, .data$SYMBOL, .data$GENENAME, .data$plot, dplyr::everything())

    par$heatmap_file_rel <- purrr::map2_chr(
      par$heatmap_file, par$out_file,
      ~ create_a_link(.x, "Heatmap", href_rel_start = fs::path_dir(.y), class = "btn btn-primary", role = "button")
    )

    if (marker_type == "global") {
      title <- glue("Cluster markers for '{par$source_column}' (level: '{par$group_level}'), {par$test_label}")
    } else {
      title <- glue(
        "Results for contrast '{par$contrast_name}' (factor: '{par$source_column}', ",
        "target: '{par$target}', reference: '{par$reference}'), {par$test_label}"
      )
    }

    if (!is_na(par$block_column)) {
      title <- glue("{title} (blocking on '{par$block_column}')")
    }

    intermediates_dir <- fs::file_temp()
    fs::dir_create(intermediates_dir)

    rmarkdown::render(
      rmd_template,
      output_dir = fs::path_dir(par$out_file),
      output_file = fs::path_file(par$out_file),
      params = list(
        title = title,
        drake_cache_dir = drake_cache_dir,
        other_params = par
      ),
      intermediates_dir = intermediates_dir,
      envir = new.env(),
      quiet = TRUE
    )

    fs::dir_delete(intermediates_dir)

    return(par)
  })

  return(markers_for_tables$out_file)
}

#' @title Generate a table with links to marker results.
#' @description This function is used directly in Rmd files for its side effects (in chunks with `results = "asis"`).
#' @param df A dataframe-like object.
#' @param group_var A character scalar: name of grouping variable for `df`.
#' @param dt_order_by A character scalar: name of column in `df` to order rows by.
#' @param base_out_dir A character scalar: path to base out dir for `cluster_markers` or `contrasts` stage.
#'   Used to determine relative paths from rendered HTML files to PDF files (marker plots, heatmaps).
#' @return Invisibly `NULL`.
#'
#' @concept sc_markers
#' @export
generate_markers_results_section <- function(df, group_var, dt_order_by, base_out_dir) {
  df <- dplyr::group_by(df, !!sym(group_var)) %>%
    dplyr::select(
      .data$name, .data$source_column,
      dplyr::any_of(c("group_level", "contrast_name", "target", "reference")),
      .data$test_type, .data$block_column,
      dplyr::everything()
    ) %>%
    dplyr::mutate(
      heatmap = purrr::map_chr(.data$heatmap_file, ~ create_a_link(., "PDF", href_rel_start = base_out_dir)),
      markers = purrr::map_chr(.data$out_file, ~ create_a_link(., "Link", href_rel_start = base_out_dir))
    ) %>%
    dplyr::select(-dplyr::all_of(c(
      "id", "description", "test_label", "heatmap_file", "dimred_plots", "out_file"
    )))

  df_list <- dplyr::group_split(df) %>%
    set_names(dplyr::group_keys(df) %>% dplyr::pull(!!sym(group_var)))
    
  if (!any(names(df_list) %in% c("binom", "t", "wilcox"))) {
    df_list <- df_list[as.character(order(names(df_list)))]
  }

  z <- lapply(names(df_list), function(name) {
    md_header(name, 4)
    res <- df_list[[name]]
    dt <- DT::datatable(
      res,
      # filter = "top",
      rownames = FALSE,
      escape = FALSE,
      width = "100%",
      class = "display compact",
      options = list(
        pageLength = 100,
        order = list(which(colnames(res) == dt_order_by) - 1, "asc"),
        autoWidth = TRUE,
        lengthMenu = list(list(10, 25, 50, -1), list(10, 25, 50, "All"))
      )
    )

    print(htmltools::tagList(dt))
  })

  invisible(NULL)
}
