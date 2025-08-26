#' @title Deconvolution of spatial dataset.
#' @description Deconvulate spots in spatial dataset using CARD or RCTD algorithm.
#' @param spe (*input target*) A normalized `SpatialExperiment` object.
#' @param sce_ref A `SingeCellExperiment` object obtain from config file.
#' @return A modified `spe` object.
#' @concept deconvolution
#' @export
spot_deconvolution_fn <- function(spe,sce_ref,sce_ref_label,deconv_method="CARD") {
  sce_ref <- readRDS(sce_ref)
  if (deconv_method == "CARD") {
    assays_list <- SummarizedExperiment::assays(spe)
    
    # Convert each assay to dgCMatrix if needed
    assays_list <- lapply(assays_list, function(x) {
      if (!inherits(x, "dgCMatrix")) {
        x <- as(x, "dgCMatrix")
      }
      return(x)
    })
    
    # Replace assays with converted sparse matrices
    SummarizedExperiment::assays(spe) <- assays_list
    SpatialExperiment::spatialCoordsNames(spe) <- c("x","y")
    CARD_obj <- CARDspa::CARD_deconvolution(
      spe = spe,
      sce = sce_ref,
      sc_count = NULL,
      sc_meta = NULL,
      spatial_count = NULL,
      spatial_location = NULL,
      ct_varname = sce_ref_label,
      ct_select = NULL,  # use cell types
      sample_varname = NULL,  # use all sce as one ref sample 
      mincountgene = 100,
      mincountspot = 5
    )
    CARD_res <- CARD_obj$Proportion_CARD
    print(head(CARD_res))
    res <- setdiff(spe@colData$Barcode,rownames(CARD_obj$Proportion_CARD))
    barcodes_true <- setdiff(spe@colData$Barcode,res)
    spe <- spe[,spe$Barcode %in% barcodes_true]
    
    spe <- sce_add_metadata(spe,deconv_results = CARD_res)
    CARD_res <- as.data.frame(CARD_res)
    ids <- names(CARD_res)[apply(CARD_res, 1, which.max)]
    spe$Deconvolution_annot <- ids
  } else if (input_type == "RCTD") {
    
    RCTD_data <- spacexr::createRctd(spe, sce_red, cell_type_col = sce_ref_label)
    RCTD_res <- spacexr::runRctd(RCTD_data, max_cores = 4, rctd_mode = "full")
    ws <- assay(RCTD_res)
    ws <- sweep(RCTD_res)
    ws_rctd <- data.frame(t(as.matrix(ws)))
    spe <- sce_add_metadata(spe,deconv_results = ws_res)
    ids <- names(RCTD_res)[apply(ws_rctd, 1, which.max)]
    spe$Deconvolution_annot <- ids
  }
  
  #spe$Deconvolution_annot <- as.factor(spe$Deconvolution_annot)
  return(spe)
}

#' @title Deconvolution annotation plotting.
#' @param spe A `SingleCellAnnotation` object.
#' @concept deconvolution
#' @return List of images.
#' @export
plot_deconv_results_fn <- function(spe, outdir) {
  card_proportions <- as.data.frame(spe@metadata$deconv_results)
  
  plt_xy <- function(vis, ws, col, point_size) {
    xy <- SpatialExperiment::spatialCoords(vis)
    xy <- xy[rownames(ws), ]
    colnames(xy) <- c("x", "y")
    df <- cbind(ws, xy)
    ggplot(df, aes(x = x, y = y, col = .data[[col]])) + 
      ggplot2::coord_equal() + 
      ggplot2::theme_void() + 
      ggplot2::geom_point(size = point_size)
  }
  
  plt_decon <- function(vis, ws) {
    plots <- lapply(names(ws), function(colname) {
      plt_xy(vis, ws, col = colname, point_size = 0.4)
    })
    
    wrap_plots(plots, nrow = 3) & 
      theme(
        legend.key.width = ggplot2::unit(0.5, "lines"),
        legend.key.height = ggplot2::unit(1, "lines")
      ) &
      ggplot2::scale_color_gradientn(colors = pals::jet())
  }
  
  pl <- plt_decon(ws = card_proportions, spe)
  out_pdf_file <- fs::path(outdir, "deconvolution.pdf")
  out_png_file <- out_pdf_file
  fs::path_ext(out_png_file) <- "png"
  # pl <- list(pl)
  pl <- tryCatch(
    {
      scdrake::save_pdf(list(pl), out_pdf_file, stop_on_error = TRUE)
      ggplot2::ggsave(
        filename = out_png_file,
        plot = pl,
        device = "png",
        dpi = 300
      )
      pl
    },
    error = function(e) {
      if (stringr::str_detect(e$message, "Viewport has zero dimension")) {
        cli_alert_warning(
          str_space(
            "Error catched: 'Viewport has zero dimension(s)'.",
            "There are probably too many levels and the legend doesn't fit into the plot.",
            "Removing the legend before saving the plot image."
          )
        )
        pl <- pl + theme(legend.position = "none")
        scdrake::save_pdf(list(pl), out_pdf_file)
        ggplot2::ggsave(
          filename = out_png_file,
          plot = pl,
          device = "png",
          dpi = 150
        )
        pl
      } else {
        cli::cli_abort(e$message)
      }
    })
  
  par <- tibble::tibble(
    title = as.character(glue::glue("deconvolution.pdf")),
    anot_plot = list(pl),
    anot_plot_out_pdf_file = out_pdf_file,
    anot_plot_out_png_file = out_png_file
  )
  
  pl <- ggspavis::plotCoords(spe,annotate = "Deconvolution_annot")
  out_pdf_file <- fs::path(outdir, "deconvolution_result.pdf")
  out_png_file <- out_pdf_file
  fs::path_ext(out_png_file) <- "png"
  # pl <- list(pl)
  pl <- tryCatch(
    {
      scdrake::save_pdf(list(pl), out_pdf_file, stop_on_error = TRUE)
      ggplot2::ggsave(
        filename = out_png_file,
        plot = pl,
        device = "png",
        dpi = 300
      )
      pl
    },
    error = function(e) {
      if (stringr::str_detect(e$message, "Viewport has zero dimension")) {
        cli_alert_warning(
          str_space(
            "Error catched: 'Viewport has zero dimension(s)'.",
            "There are probably too many levels and the legend doesn't fit into the plot.",
            "Removing the legend before saving the plot image."
          )
        )
        pl <- pl + theme(legend.position = "none")
        scdrake::save_pdf(list(pl), out_pdf_file)
        ggplot2::ggsave(
          filename = out_png_file,
          plot = pl,
          device = "png",
          dpi = 150
        )
        pl
      } else {
        cli::cli_abort(e$message)
      }
    })
  
  deconv_par <- tibble::tibble(
    title = as.character(glue::glue("deconvolution_result.pdf")),
    anot_plot = list(pl),
    anot_plot_out_pdf_file = out_pdf_file,
    anot_plot_out_png_file = out_png_file
  )
  par <- rbind(par, deconv_par)
  
  res_quantise <- data.frame("cell_type" = colnames(card_proportions), 
                                     "mean" = colMeans(card_proportions, na.rm=TRUE))
  plot_deconv_proportions <- ggplot(res_quantise, aes(x = "Sample", y = mean, fill = cell_type)) +
    ggplot2::geom_col() +
    ggplot2::geom_text(aes(label = cell_type),
              position = ggplot2::position_stack(vjust = 0.5)) +
    ggplot2::theme_minimal(base_size = 16) +
    ggplot2::ylab("Deconvolution proportions") +
    ggplot2::xlab("")
  
  labels <- spe@colData$Deconvolution_annot
  print(colnames(as.data.frame(table(labels))))
  # Create summary data frame
  label_df <- as.data.frame(table(labels)) %>%
    dplyr::rename(final_label = labels, count = Freq) %>%
    dplyr::mutate(proportion = count / sum(count) * 100)
  
  # 1. Bar plot with absolute counts
  plot_counts <- ggplot(label_df, aes(x = reorder(final_label, -count), y = count, fill = final_label)) +
    ggplot2::geom_bar(stat = "identity", width = 0.7) +
    ggplot2::ylab("Cell Count") +
    ggplot2::xlab("Cell Type") +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none") +
    ggplot2::ggtitle("Absolute Cell Counts")
  
  # 2. Stacked bar plot with proportions (just one bar)
  plot_proportions <- ggplot(label_df, aes(x = "Sample", y = proportion, fill = final_label)) +
    ggplot2::geom_bar(stat = "identity", width = 0.5) +
    ggplot2::ylab("Percentage of Cells") +
    ggplot2::xlab("") +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
    ggplot2::ggtitle("Cell Type Proportions")
  
  # Optional: Combine and save both plots side by side
  pl <- plot_deconv_proportions + plot_counts + plot_proportions + patchwork::plot_layout(ncol=2)
  out_pdf_file <- fs::path(outdir, "barplots.pdf")
  out_png_file <- out_pdf_file
  fs::path_ext(out_png_file) <- "png"
  pl <- tryCatch(
    {
      scdrake::save_pdf(list(pl), out_pdf_file, stop_on_error = TRUE,width = 15,height = 10)
      ggplot2::ggsave(
        filename = out_png_file,
        plot = pl,
        device = "png",
        dpi = 300
      )
      pl
    },
    error = function(e) {
      if (stringr::str_detect(e$message, "Viewport has zero dimension")) {
        cli_alert_warning(
          str_space(
            "Error catched: 'Viewport has zero dimension(s)'.",
            "There are probably too many levels and the legend doesn't fit into the plot.",
            "Removing the legend before saving the plot image."
          )
        )
        pl <- pl + theme(legend.position = "none")
        scdrake::save_pdf(list(pl), out_pdf_file)
        ggplot2::ggsave(
          filename = out_png_file,
          plot = pl,
          device = "png",
          dpi = 150
        )
        pl
      } else {
        cli::cli_abort(e$message)
      }
    })
  
  
  barplot_par <- tibble::tibble(
    title = as.character(glue::glue("barplots.pdf")),
    anot_plot = list(pl),
    anot_plot_out_pdf_file = out_pdf_file,
    anot_plot_out_png_file = out_png_file
  )

  par <- rbind(par, barplot_par)
}
