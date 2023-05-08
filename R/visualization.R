## -- Common functions related to visualization.

#' @title A wrapper for [scater::plotColData()].
#' @description Can add title, scales and number of each logical level (`TRUE`, `FALSE`) used for coloring.
#' @param ... Parameters passed to [scater::plotColData()].
#' @param title A characater scalr: main title of plot.
#' @param scale_x,scale_y A `ggplot2` scale, e.g. [ggplot2::scale_x_log10()].
#' @param add_logical_counts A logical scalar: if `TRUE`, add number of each logical level
#'   (`TRUE`, `FALSE`) used for coloring.
#' @return A `ggplot2` object.
#'
#' @concept sce_visualization
#' @export
plot_colData <- function(...,
                         title = "",
                         scale_x = NULL,
                         scale_y = NULL,
                         add_logical_counts = TRUE) {
  dots <- list(...)
  colour_by <- dots$colour_by

  p <- scater::plotColData(...) +
    ggtitle(title)

  if (!is_null(scale_x)) {
    p <- p + scale_x
  }

  if (!is_null(scale_y)) {
    p <- p + scale_y
  }

  if (!is_null(add_logical_counts)) {
    logical_col <- colData(dots[[1]])[, colour_by]

    if (is.logical(logical_col)) {
      scale_fill_labels <- janitor::tabyl(logical_col) %>%
        dplyr::mutate(
          logical_col = as.character(.data$logical_col),
          legend_label = gluec("{logical_col} ({n})")
        ) %>%
        dplyr::select(.data$logical_col, .data$legend_label) %>%
        tibble::deframe()

      for (i in seq_along(p$scales)) {
        s <- p$scales$scales[[i]]
        s_name <- s$name

        if (!is_null(s_name) && s_name == colour_by) {
          p$scales$scales[[i]]$labels <- scale_fill_labels
          break
        }
      }
    }
  }

  return(p)
}

#' @title A wrapper around [scater::plotReducedDim()].
#' @param sce A `SingleCellExperiment` object.
#' @param dimred,colour_by,point_size,point_alpha,... Passed to [scater::plotReducedDim()].
#' @param title,subtitle,legend_title A character scalar.
#' @param use_default_ggplot_palette A logical scalar: if `TRUE`, use the default `ggplot2` palette
#'   ([ggplot2::scale_color_hue()]).
#' @param colour_gradient A named list with low and high colors for continuous variables.
#' @param add_cells_per_cluster A logical scalar: if `TRUE` and `colour_by` variable is (coercible to) factor,
#'   add number of cells per each level to color legend labels.
#' @return A `ggplot2` object.
#'
#' @concept sce_visualization
#' @export
plotReducedDim_mod <- function(sce,
                               dimred,
                               colour_by = NULL,
                               point_size = 1,
                               point_alpha = 0.8,
                               title = "",
                               subtitle = "",
                               use_default_ggplot_palette = TRUE,
                               colour_gradient = list(low = "lightgrey", high = "blue"),
                               legend_title = NULL,
                               add_cells_per_cluster = TRUE,
                               ...) {
  assert_that_(is(sce, "SingleCellExperiment"), msg = "First parameter is not a {.var SingleCellExperiment} object.")
  assert_that_(
    dimred %in% reducedDimNames(sce),
    msg = "{.val {dimred}} not found in {.code reducedDimNames({deparse(substitute(sce))})}"
  )

  if (is_null(legend_title)) {
    legend_title <- colour_by
  }

  p <- do.call(
    scater::plotReducedDim,
    args = c(list(object = sce, dimred = dimred, colour_by = colour_by, point_size = point_size, point_alpha = point_alpha), list(...)),
    quote = TRUE
  ) +
    ggtitle(title, subtitle = subtitle)

  if (is_null(colour_by)) {
    return(p)
  }

  if (any(c("character", "factor") %in% class(colData(sce)[[colour_by]]))) {
    if (add_cells_per_cluster) {
      cells_per_cluster <- cells_per_cluster_table(sce[[colour_by]])
      legend_labels <- gluec("{cells_per_cluster$Cluster} ({cells_per_cluster$n})")
    } else {
      legend_labels <- ggplot2::waiver()
    }

    if (use_default_ggplot_palette) {
      p <- suppressMessages(p + ggplot2::scale_color_hue(labels = legend_labels))
    } else {
      p$scales$scales[[1]]$labels <- legend_labels
    }

    p <- p + guides(color = ggplot2::guide_legend(title = legend_title, override.aes = list(size = 3)))
  } else {
    if (use_default_ggplot_palette && is_null(colour_gradient)) {
      p <- suppressMessages(p + ggplot2::scale_color_gradient())
    } else {
      p <- suppressMessages(p + ggplot2::scale_color_gradient(low = colour_gradient$low, high = colour_gradient$high))
    }

    p <- p + guides(color = ggplot2::guide_colorbar(title = legend_title))
  }

  return(p)
}

#' @title Make a combined violin-boxplot plot.
#' @param sce A `SingleCellExperiment` object.
#' @param ensembl_id A character scalar: ENSEMBL ID of feature to plot.
#' @param groups A character scalar: name of a column in `colData(sce)` by which to divide cells.
#'   Violin plot will be created for each level.
#' @param add_jitter A logical scalar: if `TRUE`, add jitter to points.
#' @param jitter_color A logical scalar: if `TRUE`, jittered points will be colored by `groups`.
#' @param jitter_point_size A numeric scalar.
#' @param violin_scale A character scalar: passed to [ggplot2::geom_violin()] (`scale` parameter).
#' @param boxplot_width A numeric scalar: passed to [ggplot2::geom_boxplot()] (`width` parameter).
#' @param labs A [ggplot2::labs()] object.
#' @return A `ggplot2` object.
#'
#' @concept sce_visualization
#' @export
plot_vln <- function(sce,
                     ensembl_id,
                     groups,
                     add_jitter = TRUE,
                     jitter_color = TRUE,
                     jitter_point_size = 0.5,
                     violin_scale = "area",
                     boxplot_width = 0.1,
                     labs = NULL) {
  p <- scater::ggcells(sce, mapping = aes(x = !!sym(groups), y = !!sym(ensembl_id))) +
    ggplot2::geom_violin(aes(fill = !!sym(groups)), scale = violin_scale) +
    ggplot2::geom_boxplot(width = boxplot_width, outlier.shape = NA) +
    ggplot2::theme_bw() +
    theme(legend.position = "top", legend.justification = "left")

  if (add_jitter) {
    if (jitter_color) {
      jitter_aes <- aes(color = !!sym(groups))
    } else {
      jitter_aes <- aes()
    }

    p <- p + ggplot2::geom_jitter(jitter_aes, size = jitter_point_size)
  }

  if (!is_null(labs)) {
    p <- p + labs
  }

  return(p)
}

#' @title Highlight points belonging to certain levels.
#' @description Highlighting means that alpha value of other levels will be decreased.
#' @param p A `ggplot2` object.
#' @param column_name A character scalar: name of a column in `p` from which levels will be read.
#' @param levels A character vector: levels to highlight.
#' @param alpha_val A numeric scalar: alpha value to set for levels other than those in `levels`.
#' @return A `ggplot2` object.
#'
#' @examples
#' p <- ggplot2::ggplot(
#'   mtcars,
#'   ggplot2::aes(x = cyl, y = mpg, color = factor(am))
#' ) +
#'   ggplot2::geom_point()
#' highlight_points(p, "am", "0", alpha_val = 0.25)
#' @concept sce_visualization
#' @export
highlight_points <- function(p, column_name, levels, alpha_val = 0.1) {
  p$data <- dplyr::mutate(p$data, !!sym(column_name) := factor(!!sym(column_name))) %>%
    dplyr::mutate(alpha_ = dplyr::if_else(!!sym(column_name) %in% !!levels, 1, alpha_val))
  p$layers <- lapply(p$layers, function(layer) {
    if (any(names(layer$mapping) %in% c("color", "colour"))) {
      layer$aes_params$alpha <- NULL
    }

    return(layer)
  })
  p <- p + aes(alpha = .data$alpha_) + ggplot2::scale_alpha_identity()

  return(p)
}

#' @title Make a grid of feature plots for selected markers.
#' @param sce A `SingleCellExperiment` object.
#' @param dimred A character scalar: name of dimred to plot.
#' @param selected_markers_df A dataframe-like object with two character columns:
#' - `group`: marker group.
#' - `markers`: gene symbols of markers separated by `":"`.
#'  Such dataframe is created from a CSV file defined in the `SELECTED_MARKERS_FILE` parameter in e.g.
#'  `02_norm_clustering.yaml` config.
#'
#' *Input target*: `selected_markers_df` or `selected_markers_int_df`
#' @param assay A character scalar: name of assay in `sce` to be used.
#' @param low_color,high_color Passed as `list(low = low_color, high = high_color)` to [plotReducedDim_mod()].
#' @param combine A logical scalar: if `TRUE`, combine plots to a single `patchwork` object, otherwise return
#'   list of `ggplot2` objects.
#' @return See the `combine` argument.
#'
#' @concept sce_visualization
#' @export
selected_markers_dimplot <- function(sce,
                                     dimred,
                                     selected_markers_df,
                                     assay = "logcounts",
                                     low_color = "lightgrey",
                                     high_color = "red",
                                     combine = TRUE) {
  markers_per_group_list <- lapply(selected_markers_df[["markers"]], function(markers) {
    stringr::str_split(markers, ":") %>% unlist()
  }) %>%
    set_names(selected_markers_df$group)

  gene_annotation <- rowData(sce) %>% as.data.frame()

  gene_symbol_ensembl <- gene_annotation %>%
    dplyr::select(.data$SYMBOL, .data$ENSEMBL) %>%
    tibble::deframe() %>%
    as.list()

  selected_markers_plots <- lapply(names(markers_per_group_list), function(group) {
    group_markers <- markers_per_group_list[[group]]

    lapply(group_markers, function(marker) {
      gene_ensembl <- gene_symbol_ensembl[[marker]]
      if (!is_null(gene_ensembl) && gene_ensembl %in% rownames(sce)) {
        p <- plotReducedDim_mod(
          sce,
          by_exprs_values = assay,
          dimred = dimred,
          colour_by = gene_ensembl,
          colour_gradient = list(low = low_color, high = high_color),
          legend_title = "log2(expression)"
        ) +
          ggtitle(glue("{group} / {marker}"))

        return(p)
      } else {
        cli_alert_warning("Marker {.val {marker}} not found in {.var sce} object.")
        return(NULL)
      }
    }) %>%
      set_names(group_markers)
  }) %>%
    set_names(names(markers_per_group_list))

  if (combine) {
    plots <- unlist(selected_markers_plots, recursive = FALSE) %>%
      filter_nulls()
    ncol_p <- ceiling(length(plots)^(1 / 2))
    p_grid <- patchwork::wrap_plots(plots, ncol = ncol_p)
    return(p_grid)
  } else {
    return(selected_markers_plots)
  }
}

#' @title Save plots of selected markers.
#' @param selected_markers_plots (*input target*) A tibble: `selected_markers_plots` or `selected_markers_int_plots_df`
#' @param selected_markers_out_dir A character scalar: path to base output directory to save plots in.
#' @param is_integration A logical scalar: `TRUE` is used in the integration plan.
#' @return A character vector of output files.
#'   *Output target*: `selected_markers_plots_files` or `selected_markers_int_plots_files`
#'
#' @concept sce_visualization
#' @export
save_selected_markers_plots_files <- function(selected_markers_plots, selected_markers_out_dir, is_integration) {
  lapply_rows(selected_markers_plots, FUN = function(par) {
    if (is_integration) {
      out_pdf_file <- fs::path(selected_markers_out_dir, glue("selected_markers_{par$int_rmcc_dimred}.pdf"))
    } else {
      out_pdf_file <- fs::path(selected_markers_out_dir, glue("selected_markers_{par$dimred_name}.pdf"))
    }

    cowplot::save_plot(out_pdf_file, par$plot, base_height = 10)
    par$out_pdf_file <- out_pdf_file
    par
  })
}

#' @title Make a dimred plot for each clustering and dimred method.
#' @param sce_dimred A `SingleCellExperiment` object with computed dimreds specified in `dimred_names`.
#' @param dimred_names A character vector: dimred names to use for plotting.
#' @param cluster_df A tibble.
#' @param out_dir A character scalar: output directory in which PDF and PNG files will be saved.
#' @return A tibble. *Output target*: `dimred_plots_clustering`
#'
#' @concept sce_visualization
#' @export
dimred_plots_clustering_fn <- function(sce_dimred,
                                       dimred_names,
                                       cluster_df,
                                       out_dir = NULL) {
  cluster_df <- tidyr::crossing(cluster_df, dimred_name = dimred_names)

  res <- lapply_rows(cluster_df, FUN = function(par) {
    dimred_name <- par$dimred_name
    dimred_name_upper <- str_to_upper(dimred_name)
    cell_data <- tibble::tibble(x = par$cell_membership)
    colnames(cell_data) <- par$sce_column

    p <- plotReducedDim_mod(
      sce_add_colData(sce_dimred, cell_data),
      dimred = dimred_name,
      colour_by = par$sce_column,
      text_by = par$sce_column,
      title = glue("{par$title} | {dimred_name_upper}"),
      subtitle = par$subtitle,
      use_default_ggplot_palette = TRUE,
      legend_title = "Cluster"
    )

    if (is_null(out_dir)) {
      out_pdf_file <- NA_character_
      out_png_file <- NA_character_
    } else {
      out_pdf_file <- fs::path(out_dir, glue("{par$sce_column}_{dimred_name}.pdf"))
      out_png_file <- out_pdf_file
      fs::path_ext(out_png_file) <- "png"

      save_pdf(list(p), out_pdf_file)
      ggplot2::ggsave(
        filename = out_png_file,
        plot = p,
        device = "png",
        dpi = 300
      )
    }

    par$dimred_plot <- list(p)
    par$dimred_plot_out_pdf_file <- out_pdf_file
    par$dimred_plot_out_png_file <- out_png_file

    return(par)
  })

  return(res)
}

#' @title Put clustering dimred plots for different parameters (resolution, `k`) into a single PDF.
#' @param dimred_plots_clustering (*input target*) A tibble.
#' @param out_dir A character scalar: output directory in which PDF files will be later saved.
#' @return A tibble. *Output target*: `dimred_plots_clustering_united_files`
#'
#' @concept sce_visualization
#' @export
dimred_plots_clustering_united_files_fn <- function(dimred_plots_clustering, out_dir = NULL) {
  dimred_plots_clustering <- dimred_plots_clustering[[1]]
  algorithm_category <- dimred_plots_clustering$algorithm_category[1]
  algorithm <- dimred_plots_clustering$algorithm[1]
  dimred_name <- dimred_plots_clustering$dimred_name[1]

  if (is_null(out_dir)) {
    out_pdf_file <- NA_character_
    out_png_file <- NA_character_
  } else {
    if (algorithm_category == algorithm) {
      out_pdf_file <- fs::path(out_dir, glue("cluster_{algorithm_category}_all_{dimred_name}.pdf"))
    } else {
      out_pdf_file <- fs::path(out_dir, glue("cluster_{algorithm_category}_{algorithm}_all_{dimred_name}.pdf"))
    }
    save_pdf(dimred_plots_clustering$dimred_plot, out_pdf_file)
  }

  tibble::tibble(
    algorithm_category = .env$algorithm_category,
    algorithm = .env$algorithm,
    dimred_name = .env$dimred_name,
    dimred_plot_out_pdf_file = .env$out_pdf_file
  )
}

#' @title Make a tibble with parameters for dimred plots of selected variables.
#' @param dimred_names A character vector: names of dimreds.
#' @param dimred_plots_other A named list: see `NORM_CLUSTERING_REPORT_DIMRED_PLOTS_OTHER` parameter in
#'   `02_norm_clustering.yaml` or `02_int_clustering.yaml` config.
#' @param out_dir A character scalar: output directory in which PDF and PNG files will be later saved.
#' @return A tibble. *Output target*: `dimred_plots_other_vars_params`
#'
#' @concept sce_visualization
#' @export
dimred_plots_other_vars_params_df_fn <- function(dimred_names, dimred_plots_other, out_dir = NULL) {
  dimred_plots_other <- replace_list_nulls(dimred_plots_other)
  params <- lists_to_tibble(dimred_plots_other) %>%
    dplyr::rename(source_column = name) %>%
    tidyr::crossing(dimred_name = dimred_names) %>%
    dplyr::mutate(type = "other_vars")

  if (is_null(out_dir)) {
    out_pdf_file <- NA_character_
    out_png_file <- NA_character_
  } else {
    out_pdf_file <- fs::path(
      out_dir,
      glue::glue_data(params, "{source_column}_{dimred_name}.pdf")
    )
    out_png_file <- out_pdf_file
    fs::path_ext(out_png_file) <- "png"
  }

  params <- params %>%
    dplyr::mutate(
      label = stringr::str_wrap(.data$label, width = 60),
      out_pdf_file = .env$out_pdf_file,
      out_png_file = .env$out_png_file
    )

  return(params)
}

#' @title Make a tibble with parameters for dimred plots of cell annotation labels.
#' @param dimred_names A character vector: names of dimreds.
#' @param cell_annotation_params (*input target*) A tibble.
#' @param out_dir A character scalar: output directory in which PDF and PNG files will be later saved.
#' @return A tibble. *Output target*: `dimred_plots_other_vars_params`
#'
#' @concept sce_visualization
#' @export
dimred_plots_cell_annotation_params_df_fn <- function(dimred_names, cell_annotation_params, out_dir = NULL) {
  cell_annotation_params <- tibble::tibble(
    name = .env$cell_annotation_params$name,
    source_column = glue("{name}_labels") %>% as.character(),
    label = .env$cell_annotation_params$description
  ) %>%
    tidyr::crossing(dimred_name = dimred_names)

  if (is_null(out_dir)) {
    out_pdf_file <- NA_character_
    out_png_file <- NA_character_
  } else {
    out_pdf_file <- fs::path(
      out_dir,
      glue::glue_data(cell_annotation_params, "{source_column}_{dimred_name}.pdf")
    )
    out_png_file <- out_pdf_file
    fs::path_ext(out_png_file) <- "png"
  }

  cell_annotation_params %>%
    dplyr::mutate(
      type = "cell_annotation",
      label = stringr::str_wrap(.data$label, width = 60),
      out_pdf_file = .env$out_pdf_file,
      out_png_file = .env$out_png_file
    )
}

#' @title Make dimred plots of selected variables.
#' @param sce_dimred (*input target*) A `SingleCellExperiment` object. Input target in the integration plan is
#'   `sce_int_clustering_final`.
#' @param dimred_plots_params_df (*input target*) A tibble.
#' @return A tibble. *Output target*: `dimred_plots_other_vars`
#'
#' @concept sce_visualization
#' @export
dimred_plots_from_params_df <- function(sce_dimred, dimred_plots_params_df) {
  res <- lapply_rows(dimred_plots_params_df, FUN = function(par) {
    assert_that_(
      par$source_column %in% colnames(colData(sce_dimred)),
      msg = str_space(
        "{.code dimred_plots_params_df$source_column} {.val {par$source_column}} not found in",
        "{.code colData(sce_dimred)}. Check {.field DIMRED_PLOTS_OTHER} in {.file single_sample/02_norm_clustering} or",
        "{.file integration/02_int_clustering} config."
      )
    )

    if (is_na(par$label)) {
      label <- metadata(sce_dimred)$cell_groupings[[par$source_column]]
      par$label <- label$description
    }

    show_cluster_labels <- NULL
    show_cluster_labels <- if (
      is.factor(sce_dimred[[par$source_column]]) || is.character(sce_dimred[[par$source_column]])
    ) {
      par$source_column
    }

    p <- plotReducedDim_mod(
      sce_dimred,
      dimred = par$dimred_name,
      colour_by = par$source_column,
      title = glue("{par$label} | {str_to_upper(par$dimred_name)}"),
      use_default_ggplot_palette = TRUE,
      legend_title = par$source_column,
      text_by = show_cluster_labels
    )

    if (!is_na(par$out_pdf_file) && !is_na(par$out_png_file)) {
      res <- save_pdf(list(p), par$out_pdf_file)
      if (!res$success) {
        p <- res$error_plot
        p$plot_env$label <- glue0c(
          p$plot_env$label,
          str_line(
            "\ndimred_plots_params_df:",
            "source_column: {par$source_column}",
            "label: {par$label}",
            "dimred_name: {par$dimred_name}"
          )
        )
      }

      ggplot2::ggsave(filename = par$out_png_file, plot = p, device = "png", dpi = 150)
    }

    par$plot <- p
    return(par)
  })

  names(res$plot) <- glue("{res$source_column}_{res$dimred_name}")
  return(res)
}

#' @title Plot clustering tree.
#' @description [clustree::clustree()] is used under the hood.
#' @param cluster_list A named list of vectors.
#' @param params A vector of clustering parameters (resolutions, `k`s) in the same order as `cluster_list`.
#' @param prefix,edge_arrow,highlight_core,... Passed to [clustree::clustree()].
#' @param title A character scalar: plot title.
#' @return A `ggplot` object.
#'
#' @concept sce_visualization
#' @export
plot_clustree <- function(cluster_list, params, prefix, title = deparse(substitute(cluster_list)), edge_arrow = FALSE, highlight_core = TRUE, ...) {
  assert_that_(length(cluster_list) == length(params))
  ## -- To fix https://github.com/lazappi/clustree/issues/14
  ## -- It isn't sufficient to import clustree in NAMESPACE
  withr::local_package("clustree")

  clustree_list <- cluster_list %>%
    purrr::map(as.character) %>%
    set_names(glue("{prefix}{params}")) %>%
    purrr::map(as.integer)

  # if (length(params) == 1) {
  #   title <- glue("{title} ({prefix} {params})")
  # } else {
  #   title <- glue("{title} ({prefix} {params[1]}-{params[length(params)]})")
  # }

  clustree(tibble::as_tibble(clustree_list), prefix = prefix, edge_arrow = edge_arrow, highlight_core = highlight_core) +
    ggplot2::ggtitle(title)
}

#' @title Save a clustree plot into PDF.
#' @param p A `ggplot` object.
#' @param out_file A character scalar: output PDF file.
#' @param width,height,... Passed to [ggplot2::ggsave()].
#' @return A character scalar: `out_file`
#'
#' @concept sce_visualization
#' @export
save_clustree <- function(p, out_file, width = 14, height = 10, ...) {
  withr::local_package("clustree")
  ggplot2::ggsave(out_file, p, width = width, height = height, ...)
}
