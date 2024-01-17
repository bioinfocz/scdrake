## -- Common functions related to visualization in spatial space.

#' @title Spot visualization
#' @description Main function for visualization spots as pseudotissue plot  and subsequent information
#' @param sce A `SingleCellExpression` object
#' @param sdimx A label of x coordinates of spots
#' @param sdimy A label of y coordinates of spots
#' @param cell_color_gradient A vector of length three with color names
#' @concept spatial_visualization
#' @export
visualized_spots = function(sce,
                            sdimx = 'Dims_x',
                            sdimy = 'Dims_y',
                            spat_enr_names = NULL,
                            cell_color = NULL,
                            color_as_factor = F,
                            cell_color_code = NULL,
                            cell_color_gradient = c("navy", "lightcyan", 'red'),
                            gradient_midpoint = NULL,
                            gradient_limits = NULL,
                            select_cells = NULL,
                            point_shape = c('border', 'no_border'),
                            point_size = 3,
                            point_alpha = 1,
                            point_border_col = 'black',
                            point_border_stroke = 0.1,
                            show_cluster_center = F,
                            show_center_label = F,
                            center_point_size = 4,
                            center_point_border_col = 'black',
                            center_point_border_stroke = 0.1,
                            label_size = 4,
                            label_fontface = 'bold',
                            show_other_cells = T,
                            other_cell_color = 'lightgrey',
                            other_point_size = 1,
                            other_cells_alpha = 0.1,
                            coord_fix_ratio = NULL,
                            title = NULL,
                            show_legend = T,
                            legend_text = 8,
                            legend_symbol_size = 1,
                            background_color = 'white',
                            axis_text = 8,
                            axis_title = 8
) {





  ## point shape ##
  point_shape = match.arg(point_shape, choices = c('border', 'no_border'))

  ## get spatial cell locations
  cell_locations  = metadata(sce)$spatial_locs


  ## get cell metadata
  cell_metadata = colData(sce)[,c("Barcode",cell_color,sdimx,sdimy)]
  cell_metadata <- as.data.frame(cell_metadata)
  if(nrow(cell_metadata) == 0) {
    cell_locations_metadata = cell_locations
  } else {
    cell_locations_metadata <- cell_metadata
  }

  ## create subsets if needed

  if(!is.null(select_cells)) {
    cat('You have selected individual cell IDs \n')
    cell_locations_metadata_other = cell_locations_metadata[!cell_locations_metadata$Barcode %in% select_cells,]
    cell_locations_metadata_selected = cell_locations_metadata[cell_locations_metadata$Barcode %in% select_cells,]
}


   else if(is.null(select_cells)) {

    cell_locations_metadata_selected = cell_locations_metadata
    cell_locations_metadata_other = NULL

  }

  # data.table and ggplot variables
  sdimx_begin = sdimy_begin = sdimx_end = sdimy_end = x_start = x_end = y_start = y_end = NULL

  ### create 2D plot with ggplot ###
  #cat('create 2D plot with ggplot \n')

  pl <- ggplot2::ggplot()
  pl <- pl + ggplot2::theme_bw()

  ## plot point layer
  if(point_shape == 'border') {
    pl = plot_spat_point_layer_ggplot(ggobject = pl,
                                      sdimx = sdimx,
                                      sdimy = sdimy,
                                      cell_locations_metadata_selected = cell_locations_metadata_selected,
                                      cell_locations_metadata_other = cell_locations_metadata_other,
                                      cell_color = cell_color,
                                      color_as_factor = color_as_factor,
                                      cell_color_code = cell_color_code,
                                      cell_color_gradient = cell_color_gradient,
                                      gradient_midpoint = gradient_midpoint,
                                      gradient_limits = gradient_limits,

                                      select_cells = select_cells,
                                      point_size = point_size,
                                      point_alpha = point_alpha,
                                      point_border_stroke = point_border_stroke,
                                      point_border_col = point_border_col,
                                      show_cluster_center = show_cluster_center,
                                      show_center_label = show_center_label,
                                      center_point_size = center_point_size,
                                      center_point_border_col = center_point_border_col,
                                      center_point_border_stroke = center_point_border_stroke,
                                      label_size = label_size,
                                      label_fontface = label_fontface,
                                      show_other_cells = show_other_cells,
                                      other_cell_color = other_cell_color,
                                      other_point_size = other_point_size,
                                      show_legend = show_legend)
  } else if(point_shape == 'no_border') {
    pl = plot_spat_point_layer_ggplot(ggobject = pl,
                                      sdimx = sdimx,
                                      sdimy = sdimy,
                                      cell_locations_metadata_selected = cell_locations_metadata_selected,
                                      cell_locations_metadata_other = cell_locations_metadata_other,
                                      cell_color = cell_color,
                                      color_as_factor = color_as_factor,
                                      cell_color_code = cell_color_code,
                                      cell_color_gradient = cell_color_gradient,
                                      gradient_midpoint = gradient_midpoint,
                                      gradient_limits = gradient_limits,

                                      select_cells = select_cells,
                                      point_size = point_size,
                                      point_alpha = point_alpha,
                                      show_cluster_center = show_cluster_center,
                                      show_center_label = show_center_label,
                                      center_point_size = center_point_size,
                                      label_size = label_size,
                                      label_fontface = label_fontface,
                                      show_other_cells = show_other_cells,
                                      other_cell_color = other_cell_color,
                                      other_point_size = other_point_size,
                                      show_legend = show_legend)

  }



  ## adjust theme settings
  pl <- pl + ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                            legend.title = ggplot2::element_blank(),
                            legend.text = ggplot2::element_text(size = legend_text),
                            axis.title = ggplot2::element_text(size = axis_title),
                            axis.text = ggplot2::element_text(size = axis_text),
                            panel.grid = ggplot2::element_blank(),
                            panel.background = ggplot2::element_rect(fill = background_color))

  ## change symbol size of legend
  if(color_as_factor == TRUE) {
    if(point_shape == 'border') {
      pl = pl + guides(fill = guide_legend(override.aes = list(size = legend_symbol_size)))
    } else if(point_shape == 'no_border') {
      pl = pl + guides(color = guide_legend(override.aes = list(size = legend_symbol_size)))
    }
  }


  # fix coord ratio
  if(!is.null(coord_fix_ratio)) {
    pl <- pl + ggplot2::coord_fixed(ratio = coord_fix_ratio)
  }

  # provide x, y and plot titles
  if(is.null(title)) title = cell_color
  pl <- pl + ggplot2::labs(x = 'x coordinates', y = 'y coordinates', title = title)


}

#' @title Spatial ggplot point layer
#' @description Rendering of the color layer over base ggplot spatial pseudotissue plot
#' @param ggobject Inheritated from main functions
#' @concept spatial_visualization
plot_spat_point_layer_ggplot = function(ggobject,
                                        sdimx = NULL,
                                        sdimy = NULL,
                                        cell_locations_metadata_selected,
                                        cell_locations_metadata_other,
                                        cell_color = NULL,
                                        color_as_factor = T,
                                        cell_color_code = NULL,
                                        cell_color_gradient = c('yellow', 'white', 'red'),
                                        gradient_midpoint = NULL,
                                        gradient_limits = NULL,

                                        select_cells = NULL,
                                        point_size = 2,
                                        point_alpha = 1,
                                        point_border_col = 'lightgrey',
                                        point_border_stroke = 0.1,
                                        show_cluster_center = F,
                                        show_center_label = T,
                                        center_point_size = 4,
                                        center_point_border_col = 'black',
                                        center_point_border_stroke = 0.1,
                                        label_size = 4,
                                        label_fontface = 'bold',
                                        show_other_cells = T,
                                        other_cell_color = 'lightgrey',
                                        other_point_size = 1,
                                        show_legend = TRUE

) {

  ## specify spatial dimensions first
  if(is.null(sdimx) | is.null(sdimy)) {

    warning("plot_method = ggplot, but spatial dimensions for sdimx and/or sdimy are not specified. \n
            It will default to the 'sdimx' and 'sdimy' ")
    sdimx = 'Dims_x'
    sdimy = 'Dims_y'
  }

  ## ggplot object
  pl = ggobject

  ## first plot other non-selected cells
  if(!is.null(select_cells) & show_other_cells == TRUE) {
    pl <- pl + ggplot2::geom_point(data = cell_locations_metadata_other, ggplot2::aes(x = .data[[sdimx]], y=.data[[sdimy]]),
                                   color = other_cell_color, show.legend = F, size = other_point_size, alpha = point_alpha)
  }


  ## order of color
  # 1. if NULL then default to lightblue
  # 2. if character vector
  # 2.1 if length of cell_color is longer than 1 and has colors
  # 2.2 if not part of metadata then suppose its color
  # 2.3 part of metadata
  # 2.3.1 numerical column
  # 2.3.2 factor column or character to factor


  # cell color default
  if(is.null(cell_color)) {

    cell_color = 'lightblue'
    pl <- pl + ggplot2::geom_point(data = cell_locations_metadata_selected,
                                   ggplot2::aes(x = .data[[sdimx]], y = .data[[sdimy]]),
                                   show.legend = show_legend, shape = 21,
                                   fill = cell_color, size = point_size,
                                   stroke = point_border_stroke, color = point_border_col,
                                   alpha = point_alpha)


  } else if(length(cell_color) > 1) {

    if(is.numeric(cell_color) | is.factor(cell_color)) {
      if(nrow(cell_locations_metadata_selected) != length(cell_color)) stop('\n vector needs to be the same lengths as number of cells \n')
      cell_locations_metadata_selected[['temp_color']] = cell_color

      pl <- pl + ggplot2::geom_point(data = cell_locations_metadata_selected,
                                     ggplot2::aes(x = .data[[sdimx]], y = .data[[sdimy]], fill = 'temp_color'),
                                     show.legend = show_legend, shape = 21,
                                     size = point_size,
                                     color = point_border_col, stroke = point_border_stroke,
                                     alpha = point_alpha)

    } else if(is.character(cell_color)) {
      if(!all(cell_color %in% grDevices::colors())) stop('cell_color is not numeric, a factor or vector of colors \n')
      pl <- pl + ggplot2::geom_point(data = cell_locations_metadata_selected,
                                     ggplot2::aes(x = .data[[sdimx]], y = .data[[sdimy]]),
                                     show.legend = show_legend, shape = 21, fill = cell_color,
                                     size = point_size,
                                     color = point_border_col, stroke = point_border_stroke,
                                     alpha = point_alpha)

    }

  } else if(is.character(cell_color)) {
    if(!cell_color %in% colnames(cell_locations_metadata_selected)) {
      if(!cell_color %in% grDevices::colors()) stop(cell_color,' is not a color or a column name \n')
      pl <- pl + ggplot2::geom_point(data = cell_locations_metadata_selected,
                                     ggplot2::aes(x = .data[[sdimx]], y = .data[[sdimy]]),
                                     show.legend = show_legend, shape = 21, fill = cell_color,
                                     size = point_size,
                                     color = point_border_col, stroke = point_border_stroke,
                                     alpha = point_alpha)

    } else {

      class_cell_color = class(cell_locations_metadata_selected[[cell_color]])

      if((class_cell_color == 'integer' | class_cell_color == 'numeric') & color_as_factor == FALSE) {
        # set upper and lower limits
        if(!is.null(gradient_limits) & is.vector(gradient_limits) & length(gradient_limits) == 2) {
          lower_lim = gradient_limits[[1]]
          upper_lim = gradient_limits[[2]]

          numeric_data = cell_locations_metadata_selected[[cell_color]]
          limit_numeric_data = ifelse(numeric_data > upper_lim, upper_lim,
                                      ifelse(numeric_data < lower_lim, lower_lim, numeric_data))
          cell_locations_metadata_selected[[cell_color]] = limit_numeric_data
        }
        ######tady je problem aes vs aes_string fill = cell_color> the problem is, that we have "" ...

        pl <- pl + ggplot2::geom_point(data = cell_locations_metadata_selected,
                                       ggplot2::aes(x = .data[[sdimx]], y = .data[[sdimy]],
                                                    fill = .data[[cell_color]]),
                                       show.legend = show_legend, shape = 21,
                                       size = point_size,
                                       color = point_border_col,
                                       stroke = point_border_stroke,
                                       alpha = point_alpha)



      } else {

        # convert character or numeric to factor
        if(color_as_factor == TRUE) {
          factor_data = factor(cell_locations_metadata_selected[[cell_color]])
          cell_locations_metadata_selected[[cell_color]] <- factor_data
        }
        cell_locations_metadata_selected2 <- as.data.table(cell_locations_metadata_selected)
        # if you want to show centers or labels then calculate centers 'Dims_x', 'Dims_y' , by = cell_color
        if(show_cluster_center == TRUE | show_center_label == TRUE) {
          annotated_DT_centers = cell_locations_metadata_selected2[, .(center_1 = stats::median(get('Dims_x')),
                                                                       center_2 = stats::median(get('Dims_y'))),by = cell_color]
          factor_center_data = factor(annotated_DT_centers[[cell_color]])
          annotated_DT_centers[[cell_color]] <- factor_center_data
        }

        pl <- pl + ggplot2::geom_point(data = cell_locations_metadata_selected,
                                       ggplot2::aes(x = .data[[sdimx]], y = .data[[sdimy]], fill = .data[[cell_color]]),
                                       show.legend = show_legend, shape = 21, size = point_size,
                                       color = point_border_col, stroke = point_border_stroke,
                                       alpha = point_alpha)


        ## plot centers
        if(show_cluster_center == TRUE & (color_as_factor == TRUE | class_cell_color %in% c('character', 'factor'))) {

          pl <- pl + ggplot2::geom_point(data = annotated_DT_centers,
                                         ggplot2::aes(x = center_1, y = center_2, fill = .data[[cell_color]]),
                                         color = center_point_border_col, stroke = center_point_border_stroke,
                                         size = center_point_size, shape = 21,
                                         alpha = point_alpha)
        }

        ## plot labels
        if(show_center_label == TRUE) {
          pl <- pl + ggrepel::geom_text_repel(data = annotated_DT_centers,
                                              ggplot2::aes(x = center_1, y = center_2, label = .data[[cell_color]]),
                                              size = label_size, fontface = label_fontface)
        }

      }

      ## specificy colors to use
      if(!is.null(cell_color_code)) {

        pl <- pl + ggplot2::scale_fill_manual(values = cell_color_code)

      } else if(color_as_factor == T) {

        number_colors = length(unique(factor_data))
        cell_color_code = getDistinctColors(n = number_colors)
        names(cell_color_code) = unique(factor_data)
        pl <- pl + ggplot2::scale_fill_manual(values = cell_color_code)
        
      } else if(color_as_factor == F){

        if(is.null(gradient_midpoint)) {
          gradient_midpoint = stats::median(cell_locations_metadata_selected[[cell_color]])
        }

        pl <- pl + ggplot2::scale_fill_gradient2(low = cell_color_gradient[[1]],
                                                 mid = cell_color_gradient[[2]],
                                                 high = cell_color_gradient[[3]],
                                                 midpoint = gradient_midpoint)

      }
    }
  }
  pl <- pl + ggplot2::scale_y_reverse()
  return(pl)
}

#' @title Generating distinct colors 
#' @description Generating distinct colors for rendering color layer for factor variable
#' @param n Number of unique factors in data
#' @concept spatial_visualization
#' @export

getDistinctColors <- function(n) {
  qual_col_pals <- RColorBrewer::brewer.pal.info[RColorBrewer::brewer.pal.info$category == 'qual',]
  col_vector <- unique(unlist(mapply(RColorBrewer::brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals))));

  if(n > length(col_vector)) {

    # get all possible colors
    all_colors = grDevices::colors()
    all_colors_no_grey = grep(x = all_colors, pattern = 'grey|gray', value = T, invert = T)
    grey_colors = grep(x = all_colors, pattern = 'grey', value = T, invert = F)
    admitted_grey_colors = grey_colors[seq(1, 110, 10)]
    broad_colors = c(all_colors_no_grey, admitted_grey_colors)

    # if too many colors stop
    if(n > length(broad_colors)) {
      warning('\n not enough unique colors in R, maximum = 444 \n')
      col_vector = sample(x = broad_colors, size = n, replace = T)
    } else {
      col_vector = sample(x = broad_colors, size = n, replace = F)
    }

  } else {

    xxx <- grDevices::col2rgb(col_vector);
    dist_mat <- as.matrix(stats::dist(t(xxx)));
    diag(dist_mat) <- 1e10;
    while (length(col_vector) > n) {
      minv <- apply(dist_mat,1,function(x)min(x));
      idx <- which(minv==min(minv))[1];
      dist_mat <- dist_mat[-idx, -idx];
      col_vector <- col_vector[-idx]
    }

  }
  return(col_vector)
}

#' @title Control quality spatial plot
#' @description Function for rendering of quality control plots of selected features
#' @concept spatial_visualization
plot_spat_visuals <- function(sce) {
  to_plot <- c("detected","sum","subsets_mito_percent","subsets_ribo_percent")
  plist <- list()
  n=1
  for (j in to_plot) {
    plist[[n]] <- visualized_spots(sce=sce, sdimx = "Dims_x",sdimy = "Dims_y",
                                   cell_color = j, point_size = 2, point_shape = "border",
                                   color_as_factor = F, point_alpha = 1, show_legend = T)
    n= n+1
  }

  return(plist)
}

#' @title Spatial gene plot
#' @description Spatial visualization of selected genes within pseudotissue representaiton
#' @param sce A `SingleCellExpression` object
#' @param sdimx A label of x spot coordinates
#' @param sdimy A label of y spot coordinates
#' @param values A label of which values to use, counts of logcounts
#' @param genes A vector of genes to visualization
#' @param cell_color_gradient A vector of lenght three with color names to use as gradient
#' @concept spatial_visualization
#' @export
spatGenePlot2Dsce <- function(sce,

                           sdimx = 'Dims_x',
                           sdimy = 'Dims_y',
                           expression_values = c('counts', 'logcounts'),
                           genes,
                           cell_color_gradient = c('blue', 'white', 'red'),
                           gradient_midpoint = NULL,
                           gradient_limits = NULL,

                           edge_alpha = NULL,

                           #midpoint = 0,
                           scale_alpha_with_expression = FALSE,
                           point_shape = c('border', 'no_border'),
                           point_size = 1,
                           point_alpha = 1,
                           point_border_col = 'black',
                           point_border_stroke = 0.1,
                           show_legend = T,
                           legend_text = 8,
                           background_color = 'white',

                           axis_text = 8,
                           axis_title = 8,
                           cow_n_col = 2,
                           cow_rel_h = 1,
                           cow_rel_w = 1,
                           cow_align = 'h'
                           ) {


  # data.table variables
  Barcode = NULL

  # point shape
  point_shape = match.arg(point_shape, choices = c('border', 'no_border'))

  # expression values
  values = match.arg(expression_values, c('counts', 'logcounts'))
  expr_values = as.matrix(assay(sce, values))
  colnames(expr_values) <- colData(sce)[["Barcode"]]
  # only keep genes that are in the dataset
  selected_genes = genes
  selected_genes = selected_genes[selected_genes %in% rownames(expr_values) ]


  # get selected gene expression values in data.table format
  if(length(selected_genes) == 1) {
    subset_expr_data = expr_values[rownames(expr_values) %in% selected_genes, ]
    t_sub_expr_data_DT = data.table::data.table('selected_gene' = subset_expr_data, 'Barcode' = colnames(expr_values))
    data.table::setnames(t_sub_expr_data_DT, 'selected_gene', selected_genes)
  } else {
    subset_expr_data = expr_values[rownames(expr_values) %in% selected_genes, ]
    t_sub_expr_data = t(subset_expr_data)
    t_sub_expr_data_DT = data.table::as.data.table(t_sub_expr_data)
    t_sub_expr_data_DT[, Barcode := rownames(t_sub_expr_data)]
  }


  ## get spatial cell locations
  cell_locations  = metadata(sce)$spatial_locs


  ## get cell metadata
  cell_metadata = colData(sce)[,c("Barcode",sdimx,sdimy)]
  cell_metadata <- as.data.frame(cell_metadata)
  if(nrow(cell_metadata) == 0) {
    cell_locations_metadata = cell_locations
  } else {
    cell_locations_metadata <- cell_metadata
  }

  cell_locations_metadata_genes <- merge(cell_locations_metadata, t_sub_expr_data_DT, by = 'Barcode')


  ## plotting ##
  savelist <- list()

  for(gene in selected_genes) {

    pl <- ggplot2::ggplot()
    pl <- pl + ggplot2::theme_classic()



    ### plot cells ###

    ## set gradient limits if needed ##
    if(!is.null(gradient_limits) & is.vector(gradient_limits) & length(gradient_limits) == 2) {
      lower_lim = gradient_limits[[1]]
      upper_lim = gradient_limits[[2]]
      numeric_data = cell_locations_metadata_genes[[gene]]
      limit_numeric_data = ifelse(numeric_data > upper_lim, upper_lim,
                                  ifelse(numeric_data < lower_lim, lower_lim, numeric_data))
      cell_locations_metadata_genes[[gene]] = limit_numeric_data
    }



    ## with border ##
    if(point_shape == 'border') {

      if(scale_alpha_with_expression == TRUE) {
        pl <- pl + ggplot2::geom_point(data = cell_locations_metadata_genes, aes_string(x = sdimx,
                                                                                         y = sdimy,
                                                                                         fill = gene,
                                                                                         alpha = gene),
                                       shape = 21,
                                       color = point_border_col, size = point_size, stroke = point_border_stroke,
                                       show.legend = show_legend)
      } else {
        pl <- pl + ggplot2::geom_point(data = cell_locations_metadata_genes,  aes_string(x = sdimx,
                                                                                          y = sdimy,
                                                                                          fill = gene),
                                       shape = 21,
                                       color = point_border_col, size = point_size, stroke = point_border_stroke,
                                       show.legend = show_legend, alpha = point_alpha)
      }


      ## scale and labs ##
      if(is.null(gradient_midpoint)) {
        gradient_midpoint =  stats::median(NA^(cell_locations_metadata_genes[[gene]]==0)*cell_locations_metadata_genes[[gene]], na.rm=TRUE)
      }
      pl <- pl + ggplot2::scale_alpha_continuous(guide = 'none')
      pl <- pl + ggplot2::scale_fill_gradient2(low = cell_color_gradient[[1]],
                                               mid = cell_color_gradient[[2]],
                                               high = cell_color_gradient[[3]],
                                               midpoint = gradient_midpoint,
                                               guide = guide_colorbar(title = ''))
      pl <- pl + ggplot2::labs(x = 'coord x', y = 'coord y', title = gene)


    }



    ## no border ##
    if(point_shape == 'no_border') {

      if(scale_alpha_with_expression == TRUE) {
        pl <- pl + ggplot2::geom_point(data = cell_locations_metadata_genes,  aes_string(x = sdimx,
                                                                                          y = sdimy,
                                                                                          color = gene,
                                                                                          alpha = gene),
                                       shape = 19, size = point_size,  show.legend = show_legend)
      } else {
        pl <- pl + ggplot2::geom_point(data = cell_locations_metadata_genes,  aes_string(x = sdimx,
                                                                                          y = sdimy,
                                                                                          color = gene),
                                       shape = 19, size = point_size, show.legend = show_legend, alpha = point_alpha)
      }


      ## scale and labs ##

      if(is.null(gradient_midpoint)) {
        gradient_midpoint =  stats::median(NA^(cell_locations_metadata_genes[[gene]]==0)*cell_locations_metadata_genes[[gene]], na.rm=TRUE)
      }

      pl <- pl + ggplot2::scale_alpha_continuous(guide = 'none')
      pl <- pl + ggplot2::scale_color_gradient2(low = cell_color_gradient[[1]],
                                                mid = cell_color_gradient[[2]],
                                                high = cell_color_gradient[[3]],
                                                midpoint = gradient_midpoint,
                                                guide = guide_colorbar(title = ''))
      pl <- pl + ggplot2::labs(x = 'coord x', y = 'coord y', title = gene)

    }

    ## theme ##
    pl <- pl + ggplot2::theme(plot.title = element_text(hjust = 0.5),
                              legend.title = element_blank(),
                              legend.text = element_text(size = legend_text),
                              axis.title = element_text(size = axis_title),
                              axis.text = element_text(size = axis_text),
                              panel.grid = element_blank(),
                              panel.background = element_rect(fill = background_color))


    savelist[[gene]] <- pl
  }

  # combine plots with cowplot
  combo_plot <- cowplot::plot_grid(plotlist = savelist,
                                   ncol = cow_n_col,
                                   rel_heights = cow_rel_h, rel_widths = cow_rel_w, align = cow_align)



}
