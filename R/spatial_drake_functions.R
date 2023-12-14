
## -- create giotto object, add rotated spatial locs
createGiotto_fn = function(sce,annotation = FALSE, selected_clustering = NULL){
  raw_expr = assay(sce, "counts")
  colnames(raw_expr) <- colData(sce)[,"Barcode"]
  norm_expression <- SummarizedExperiment::assay(sce, "logcounts")
  colnames(norm_expression) <- colData(sce)[,"Barcode"]
  ##or select all clustering...
  subset_cell_metadata <- c("Barcode","total","subsets_mito_percent","subsets_ribo_percent",c(selected_clustering))
  cell_metadata <- SingleCellExperiment::colData(sce)[,subset_cell_metadata]
  colnames(cell_metadata)[colnames(cell_metadata) == 'Barcode'] <- 'cell_ID'

  gene_metadata <- as.data.frame(SingleCellExperiment::rowData(sce)[,c("SYMBOL","ENSEMBL")])
  colnames(gene_metadata) <- c("gene_ID","ensembl_ID")
  #rownames(raw_epxr) <- gene_metadata$gene_ID
  if (annotation) {
    rownames(raw_expr) <- c(SingleCellExperiment::rowData(sce)[,"SYMBOL"])
    rownames(norm_expression) <- c(SingleCellExperiment::rowData(sce)[,"SYMBOL"])
  }
  gobj = Giotto::createGiottoObject(raw_exprs = raw_expr,norm_expr = norm_expression,
                            cell_metadata = cell_metadata,
                            spatial_locs = as.data.frame(SingleCellExperiment::colData(sce)[,c("Dims_x","Dims_y")]),
                            gene_metadata = gene_metadata)
  return(gobj)
}
cellProximityBarplot_fn = function(gobject = gobject,
                                   CPscore = CPscore, out_dir = out_dir) {
  pl <- Giotto::cellProximityBarplot(gobject,
                                     CPscore,
                                     min_orig_ints = 3, min_sim_ints = 3,return_plot = TRUE,show_plot = FALSE)
  out_pdf_file <- fs::path(out_dir, "CellProximityBarplot.pdf")
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

  par <- tibble(title = "CellProximityBarplot.pdf", anot_plot = list(pl), anot_plot_out_pdf_file = out_pdf_file,
                anot_plot_out_png_file = out_png_file)
}

# heatmap
cellProximityHeatmap_fn = function(gobject = gobject,
                                   CPscore = CPscore, out_dir=out_dir){

  pl <- Giotto::cellProximityHeatmap(gobject,
                                     CPscore,
                                     order_cell_types = T,scale = T,
                                     color_breaks = c(-1.5, 0, 1.5),
                                     return_plot = TRUE,show_plot = FALSE)

  out_pdf_file <- fs::path(out_dir, "CellProximityHeatmap.pdf")
  out_png_file <- out_pdf_file
  fs::path_ext(out_png_file) <- "png"
  #pl <- list(pl)

  scdrake::save_pdf(list(pl), out_pdf_file, stop_on_error = TRUE)


  par <- tibble(title = "CellProximityHeatmap", anot_plot = list(pl), anot_plot_out_pdf_file = out_pdf_file,
                anot_plot_out_png_file = NULL)
}

## -- receptor ligand analyses
LR_data_fn = function(LR_data=LR_data,gobj=gobj,selected_clustering=selected_clustering){

  LR_data <- data.table::as.data.table(LR_data)
  LR_data[, ligand_det := ifelse(humanLigand %in% gobj@gene_ID, T, F)]
  LR_data[, receptor_det := ifelse(humanReceptor %in% gobj@gene_ID, T, F)]
  LR_data_det = LR_data[ligand_det == T & receptor_det == T]

  select_ligands = LR_data_det$humanLigand

  select_receptors = LR_data_det$humanReceptor
  metadata = Giotto::pDataDT(gobj)
  uniq_clusters = length(unique(metadata[[selected_clustering]]))

  clusters_cell_types = paste0('cluster_', c(1:uniq_clusters))
  names(clusters_cell_types) = c(1:uniq_clusters)
  print(names(clusters_cell_types))

  gobj_anot = Giotto::annotateGiotto(gobject = gobj,
                                            annotation_vector = clusters_cell_types,
                                            cluster_column = selected_clustering,
                                            name = paste0('cell_types_',selected_clustering))

  print(head(Giotto::pDataDT(gobj_anot)[[paste0('cell_types_',selected_clustering)]]))

  spatial_all_scores = Giotto::exprCellCellcom(gobject = gobj_anot,
                                       #spatial_network_name = 'spatial_network',
                                       cluster_column = paste0('cell_types_',selected_clustering),
                                       random_iter = 100,
                                       gene_set_1 = select_ligands,
                                       gene_set_2 = select_receptors,
                                       adjust_method = 'fdr',
                                       # do_parallel = F,
                                       # cores = 6,
                                       verbose = 'a little')
  print(head(spatial_all_scores))
  #spatial_all_scores <- spatial_all_scores[complete.cases(spatial_all_scores), ]
  #spatial_all_scores[spatial_all_scores$rand_expr==0] <- NA
  return(spatial_all_scores)
}

## get statistical significance of gene pair expression changes upon cell-cell interaction
#Spatial Cell-Cell communication scores based on spatial expression of interacting cells


selected_spat_fn = function(spatial_all_scores){
  spatial_all_scores <- data.table::as.data.table(spatial_all_scores)
  selected_spat = spatial_all_scores[p.adj > 0 & p.adj <= 0.5 & abs(log2fc) > 0.1 & lig_nr >= 2 & rec_nr >= 2]
  data.table::setorder(selected_spat, -PI)
  return(selected_spat)
}

top_LR_ints_fn_new = function(selected_spat){
  top_LR_ints <- unique(selected_spat[order(-abs(PI))]$LR_comb)[1:33]
  return(top_LR_ints)
}

top_LR_cell_ints_fn = function(selected_spat){
  top_LR_cell_ints <- unique(selected_spat[order(-abs(PI))]$LR_cell_comb)[1:33]
  return(top_LR_cell_ints)
}

plotCCcomHeatmap_fn = function(gobject = gobject,
                               comScores = comScores,
                               selected_LR = selected_LR,
                               selected_cell_LR = selected_cell_LR,
                               out_dir = out_dir) {

  pl <- Giotto::plotCCcomHeatmap(gobject,
                           comScores,
                           selected_LR,
                           selected_cell_LR,
                           show = 'PI',return_plot = TRUE,show_plot = FALSE)
  out_pdf_file <- fs::path(out_dir, "CCcomHeatmap.pdf")
  out_png_file <- out_pdf_file
  fs::path_ext(out_png_file) <- "png"
  #pl <- list(pl)

  #pl <- list(pl)

  scdrake::save_pdf(list(pl), out_pdf_file, stop_on_error = TRUE)


  par <- tibble(title = "CCcomHeatmap", anot_plot = list(pl), anot_plot_out_pdf_file = out_pdf_file,
                anot_plot_out_png_file = NULL)
}

plotCCcomDotplot_fn = function(gobject = gobject,
                                comScores = comScores,
                                selected_LR = selected_LR,
                                selected_cell_LR = selected_cell_LR, out_dir = out_dir){

  pl <- Giotto::plotCCcomDotplot(gobject ,
                                          comScores ,
                                          selected_LR ,
                                          selected_cell_LR,
                                          cluster_on = 'LR_expr',return_plot = TRUE,show_plot = FALSE)
  out_pdf_file <- fs::path(out_dir, "CCcomDotplot.pdf")
  out_png_file <- out_pdf_file
  fs::path_ext(out_png_file) <- "png"
  #pl <- list(pl)

    scdrake::save_pdf(list(pl), out_pdf_file, stop_on_error = TRUE)

  par <- tibble(title = "CCcomDotplot.pdf", anot_plot = list(pl), anot_plot_out_pdf_file = out_pdf_file,
                anot_plot_out_png_file = out_png_file)
}

