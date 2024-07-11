## -- Common functions used for manual annotation of spots/cells.
#' @title Create signature matrix from provided file containing names with markers.
#' @param markers_file A csv file containing list of annotation names with selected markers.
#' @export
#' @concept manual_annotation
create_signature_matrix_fn <- function(markers_file){
  markers <- read.csv(markers_file)
  
  #NEW SECTION
  long_df <- markers %>%
    tidyr::pivot_longer(cols = everything(), names_to = "types", values_to = "gene") %>%
    dplyr::filter(gene != "")
  
  # Create a binary indicator for the presence of genes
  
  signature_matrix <- as.data.frame(long_df %>%
    dplyr::mutate(Presence = 1) %>%
    tidyr::pivot_wider(names_from = types, values_from = Presence, values_fill = list(Presence = 0)))
 rownames(signature_matrix) <- signature_matrix$gene
 signature_matrix <- signature_matrix[,-1]
  return(signature_matrix)
}


#' @title Calculate and run PAGE annotation.
#' @param sign_matrix precalculated signature matrix
#' @param sce A `SingleCellExperiment` object
#' @param values A expresion indicating which values use, logcounts as default
#' @export
#' @concept manual_annotation
run_page_man_annotation <- function(sign_matrix,
                                    sce,
                                    values="logcounts",
                                    #clustering,
                                    scale=NULL,
                                    overlap=5,
                                    reverse_log_scale=FALSE,
                                    selected_annotation = NULL,
                                    output_enrichment="zscore") {
  expr_values <- assay(sce,values)
  
  rownames(expr_values) <- rowData(sce)$SYMBOL
  available_ct <- c()
  
  for (i in colnames(sign_matrix)){
    gene_i <- rownames(sign_matrix)[which(sign_matrix[,i]==1)]
    overlap_i <- intersect(gene_i,rownames(expr_values))
    if (length(overlap_i)<=overlap){
      output <- paste0("Warning, ",i," only has ",length(overlap_i)," overlapped genes. Will remove it.")
      print(output)
    } else {
      available_ct <- c(available_ct,i)
    }
  }
  
  if (length(selected_annotation)>0){
    available_ct <- intersect(available_ct, selected_annotation)
    output<-paste0("Warning, continuing only with selected annotation. Available annotation are ",available_ct)
    print(output)
  }
  
  if (length(available_ct)==1){
    
    print(available_ct)
    stop("Only one cell type available. Program will stop")
  }
  if (length(available_ct)<1){
    
    stop("No cell type available for this experiment. Program will stop")
  }
  
  interGene <- intersect(rownames(sign_matrix), rownames(expr_values))
  filterSig <- sign_matrix[interGene, available_ct]
  signames <- rownames(filterSig)[which(filterSig[,1]==1)]
  
  # calculate mean gene expression
  if(reverse_log_scale == TRUE) {
    mean_gene_expr <- log(rowMeans(logbase^expr_values-1, dims = 1)+1)
  } else {
    mean_gene_expr <- Matrix::rowMeans(expr_values)
  }
  geneFold <- expr_values - mean_gene_expr
  
  cellColMean <- apply(geneFold,2,mean)
  cellColSd <- apply(geneFold,2,stats::sd)
  
  # get enrichment scores
  enrichment <- matrix(data=NA,nrow = dim(filterSig)[2],ncol=length(cellColMean))
  for (i in (1:dim(filterSig)[2])){
    signames <- rownames(filterSig)[which(filterSig[,i]==1)]
    sigColMean <- apply(geneFold[signames,],2,mean)
    m <- length(signames)
    vectorX <- NULL
    for (j in(1:length(cellColMean))){
      Sm <- sigColMean[j]
      u <- cellColMean[j]
      sigma <- cellColSd[j]
      zscore <- (Sm - u)* m^(1/2) / sigma
      vectorX <- append(vectorX,zscore)
    }
    enrichment[i,] <- vectorX
  }
  ##
  rownames(enrichment) <- colnames(filterSig)
  colnames(enrichment) <- names(cellColMean)
  enrichment <- t(enrichment)
  
  if(output_enrichment == "zscore") {
    enrichment <- scale(enrichment)
  }
  
  return(enrichment)
}

#' @title Calculate metadata for manual cell/spot annotation for heatmap visualisation.
#' @param sce A `SingleCellExperiment` object
#' @param enrichment precalculated enrichment score for each cell/spot
#' @param clustering A vector of selected clustering used for annotation, inheritated from meta_heatmap plotting
#' @concept manual_annotation
calculate_metadata <- function(sce, enrichment, clustering) {
  cell_types <- colnames(enrichment) 
  sce[[glue::glue("manual_annotation_{clustering}")]] <- colnames(enrichment)[apply(enrichment,1,which.max)]
  cell_metadata <- cbind(enrichment,sce[[clustering]])
  colnames(cell_metadata)[ncol(cell_metadata)] <- clustering
  sce <- scdrake::sce_add_metadata(sce = sce, clustering_enrichment = cell_metadata)
  
  return(sce)
}


#' @title Manual annotation heatmap plotting
#' @param sce A `SingleCellAnnotation` object
#' @param clustering Selected clustering
#' @param spatial Logical vector, if include spot images for each anotation
#' @param make_cell_plot Logical vector, if include pseudotissue images, for spatial extension
#' @concept manual_annotation
#' @export
meta_heatmap_ploting <- function(sce,clus_cor_method="pearson",clus_cluster_method = "complete",
                                 values_cor_method="pearson",values_cluster_method="complete",
                                 clustering,
                                 show_value="value",
                                 #selection(c("value","zscores","zscores_rescaled"))
                                 gradient_midpoint = 0,
                                 gradient_limits = NULL,
                                 x_text_size = 10,
                                 x_text_angle = 45,
                                 y_text_size = 10,
                                 strip_text_size = 8,
                                 low = "blue", mid = "white", high = "red",
                                 spatial=FALSE,
                                 make_cell_plot=FALSE,
                                 out_dir=NULL) {
 
  cell_metadata <- metadata(sce)[["clustering_enrichment"]]
  
  cell_types <- colnames(cell_metadata)[!colnames(cell_metadata) %in% clustering]

  cell_metadata_cols <- colnames(cell_metadata)[-which(colnames(cell_metadata) %in% clustering )]
  
  cell_metadata <- tibble::as_tibble(cell_metadata)
  
  cell_metadata <- cell_metadata %>% 
    dplyr::mutate_at(clustering, factor)
  
  workdt <- cell_metadata %>%
    dplyr::group_by(!!! rlang::syms(clustering)) %>%
    dplyr::summarise(dplyr::across(all_of(cell_metadata_cols), mean, na.rm = TRUE))
  
  page_enrichment <- workdt %>%
    tidyr::pivot_longer(cols = all_of(cell_metadata_cols), names_to = "variable", values_to = "value")
  
  
  ##plotMetaDataCellsHeatmap
  metaDT <- page_enrichment
  
  # Step 1: Calculate Z-Scores
  metaDT <- metaDT %>%
    dplyr::group_by(variable) %>%
    dplyr::mutate(zscores = c(scale(value)))
  
  # Step 2: Rescale Z-Scores to Range [-1, 1]
  metaDT <- metaDT %>%
    dplyr::group_by(variable) %>%
    dplyr::mutate(zscores_rescaled_per_gene = c(scales::rescale(zscores, to = c(-1, 1))))
  #print(head(metaDT))
  #Calculate means
  # testmain <- metaDT %>% 
  #   dplyr::group_by(variable, !!! rlang::syms(main_factor)) %>%
  #   dplyr::summarise(mean_value = mean(value))
  # 
  # # Step 2: Define the dfunction
  # dfunction <- function(d, col_name1, col_name2, value.var) {
  #   d %>%
  #     tidyr::pivot_wider(names_from = {{ col_name2 }}, values_from = {{ value.var }})
  # }
  
  # Step 3: Apply dfunction to testmain
  # testmain_matrix <- dfunction(d = testmain, col_name1 = variable, col_name2 = main_factor, value.var = mean_value)
  # 
  # testmain_mat <- as.matrix(testmain_matrix[,-1]); rownames(testmain_mat) = testmain_matrix$variable
  # # for clusters
  # ## this part is ridiculusely redundant...it is just sorting rows and column based on hierarchic clustering!!!!
  # cormatrix <- stats::cor(x = testmain_mat, method = clus_cor_method)
  # cordist <- stats::as.dist(1 - cormatrix, diag = T, upper = T)
  # corclus <- stats::hclust(d = cordist, method = clus_cluster_method)
  # clus_names <- rownames(cormatrix)
  # names(clus_names) <- 1:length(clus_names)
  # clus_sort_names <- clus_names[corclus$order]
  # 
  # 
  # # for genes
  # 
  # values_cormatrix <- stats::cor(x = t(testmain_mat), method = values_cor_method)
  # values_cordist <- stats::as.dist(1 - values_cormatrix, diag = T, upper = T)
  # values_corclus <- stats::hclust(d = values_cordist, method = values_cluster_method)
  # values_names <- rownames(values_cormatrix)
  # names(values_names) <- 1:length(values_names)
  # values_sort_names <- values_names[values_corclus$order]
  ## -- should it remain?
  
  
  # data.table variables
  #factor_column = variable = NULL
  ##def not necesary part
  # metaDT[, factor_column := factor(get(clustering), levels = clus_sort_names)]
  # metaDT[, variable := factor(get('variable'), levels = values_sort_names)]
  ###new part
  metaDT <- metaDT %>%
    dplyr::mutate(factor_column = factor(!!! rlang::syms(clustering))) #, levels = clus_sort_names))
    
    # Convert variable column to a factor with specified levels
  metaDT <- metaDT %>%
    dplyr::mutate(variable = as.character(variable)) #, levels = values_sort_names))
    ##
    #print(head(metaDT))
    
  pl <- ggplot2::ggplot()
  pl <- pl + ggplot2::geom_tile(data = metaDT, ggplot2::aes(x = factor_column, y = variable, fill =.data[[show_value]]), color = "black")
  pl <- pl + ggplot2::scale_fill_gradient2(low = low, mid = mid, high = high, midpoint = gradient_midpoint)
  pl <- pl + ggplot2::theme_classic()
  pl <- pl + ggplot2::theme(axis.text.x = ggplot2::element_text(size = x_text_size, angle = x_text_angle, hjust = 1, vjust = 1),
                            axis.text.y = ggplot2::element_text(size = y_text_size),
                            legend.title = ggplot2::element_blank())
  pl <- pl + ggplot2::labs(x = clustering, y = "cell types")
  #return(pl)
  
  
  #output pdf
  out_pdf_file <- fs::path(out_dir, glue::glue("manual_annotation_{clustering}.pdf"))
  out_png_file <- out_pdf_file
  fs::path_ext(out_png_file) <- "png"
  #pl <- list(pl)
  pl <- tryCatch({
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
      cli_alert_warning(str_space(
        "Error catched: 'Viewport has zero dimension(s)'.",
        "There are probably too many levels and the legend doesn't fit into the plot.",
        "Removing the legend before saving the plot image."
      ))
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
  }
  )
  
  par <- tibble::tibble(title = as.character(glue::glue("manual_annotation_{clustering}.pdf")), anot_plot = list(pl), anot_plot_out_pdf_file = out_pdf_file,
                        anot_plot_out_png_file = out_png_file)
  
  if (spatial) {
    man_anot_plot <- visualized_spots(sce,
                                    cell_color = glue::glue("manual_annotation_{clustering}"),point_size = 5,color_as_factor = T,
                                    legend_symbol_size = 3,legend_text = 16)
    out_pdf_file <- fs::path(out_dir, "spatmananotplot.pdf")
    scdrake::save_pdf(list(man_anot_plot), out_pdf_file, stop_on_error = TRUE,width = 14,height = 14)
    annot_par <- tibble::tibble(title = "spatmananotplot.pdf", anot_plot = list(man_anot_plot),
                               anot_plot_out_pdf_file = out_pdf_file,anot_plot_out_png_file = NA)
    par = rbind(par,annot_par)
    
    if (make_cell_plot) {
      cell_annotation_values = cell_types
      savelist <- list()
      
      for(annot in cell_annotation_values) {
        enrich_plot <- visualized_spots(scdrake::sce_add_colData(sce,cell_metadata),
                                        cell_color = annot,point_size = 1.5)
        savelist[[annot]] <- enrich_plot
      }
      combo_plot <- cowplot::plot_grid(plotlist = savelist)
      out_pdf_file <- fs::path(out_dir, "spatcellplot.pdf")
      scdrake::save_pdf(list(combo_plot), out_pdf_file, stop_on_error = TRUE,width = 14,height = 14)
      #print(head(par))
      
      cell_par <- tibble::tibble(title = "spatcellplot.pdf", anot_plot = list(combo_plot),
                                 anot_plot_out_pdf_file = out_pdf_file,anot_plot_out_png_file = NA)
      #print(head(cell_par))
      
      par = rbind(par,cell_par)
    }
  }
  par
  
}