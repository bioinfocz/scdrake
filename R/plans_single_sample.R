## -- Functions to build drake plans for single-sample analysis.

#' @title Get a `drake` plan for a stage of single-sample pipeline.
#' @param cfg A list of parameters for this stage
#'   (loaded from single-sample config directory, e.g. `01_input_qc.yaml`, etc.).
#' @inheritParams cfg_pipeline_param
#' @param cfg_main A list of general parameters
#'   (loaded from `00_main.yaml` from single-sample config directory).
#' @return [drake::drake_plan()]
#'
#' @concept get_subplan_single_sample
#' @name get_subplan_single_sample
NULL

#' @description A plan for 01_input_qc stage.
#' @rdname get_subplan_single_sample
#' @export
get_input_qc_subplan <- function(cfg, cfg_pipeline, cfg_main) {
  drake::drake_plan(
    ## -- Save config.
    config_input_qc = !!cfg,

    ## -- Read raw Cell Ranger files.
    sce_raw = sce_raw_fn(!!cfg$INPUT_DATA, input_data_subset = !!cfg$INPUT_DATA_SUBSET),
    sce_raw_info = save_object_info(sce_raw),

    ## -- Calculate barcode ranks (for knee plot).
    barcode_ranks = counts(sce_raw) %>% DropletUtils::barcodeRanks(),

    ## -- Mark empty droplets.
    empty_droplets = empty_droplets_fn(
      sce_raw,
      empty_droplets_lower = !!cfg$EMPTY_DROPLETS_LOWER,
      empty_droplets_enabled = !!cfg$EMPTY_DROPLETS_ENABLED,
      BPPARAM = ignore(BiocParallel::bpparam())
    ),

    ## -- Remove empty droplets from sce.
    sce_valid_cells = sce_valid_cells_fn(
      sce_raw = sce_raw, empty_droplets = empty_droplets, empty_droplets_fdr_threshold = !!cfg$EMPTY_DROPLETS_FDR_THRESHOLD
    ),
    sce_valid_cells_info = save_object_info(sce_valid_cells),

    ## -- Mark mitochondrial and ribosomal genes.
    mito_genes = which_genes_regex(sce = sce_valid_cells, regex = !!cfg$MITO_REGEX, colname = "Symbol", ignore_case = TRUE),
    ribo_genes = which_genes_regex(sce = sce_valid_cells, regex = !!cfg$RIBO_REGEX, colname = "Symbol", ignore_case = TRUE),

    ## -- Calculate cell QC metrics and add it.
    cell_qc = scater::perCellQCMetrics(
      sce_valid_cells,
      subsets = list(mito = mito_genes, ribo = ribo_genes), BPPARAM = ignore(BiocParallel::bpparam())
    ),

    ## -- Dataset-sensitive filters.
    qc_filters_raw = list(
      qc_lib = scater::isOutlier(cell_qc$total, log = TRUE, nmads = !!cfg$MAD_THRESHOLD, type = "lower"),
      qc_nexprs = scater::isOutlier(cell_qc$detected, nmads = !!cfg$MAD_THRESHOLD, log = TRUE, type = "lower"),
      qc_mito = scater::isOutlier(cell_qc$subsets_mito_percent, nmads = !!cfg$MAD_THRESHOLD, type = "higher")
      # qc_ribo = isOutlier(cell_qc$subsets_ribo_percent, nmads = !!cfg$MAD_THRESHOLD, type = "higher")
    ),
    qc_filters = purrr::map(qc_filters_raw, ~ as.logical(.) %>% tidyr::replace_na(replace = FALSE)),
    ## -- Join filters by OR operator.
    qc_filter = Reduce("|", qc_filters),

    ## -- Custom filters.
    custom_filters_raw = list(
      low_count = cell_qc$total <= !!cfg$MIN_UMI_CF,
      high_count = cell_qc$total >= !!cfg$MAX_UMI_CF,
      low_expression = cell_qc$detected <= !!cfg$MIN_FEATURES,
      high_mito = cell_qc$subsets_mito_percent >= !!cfg$MAX_MITO_RATIO * 100
      # low_ribo = cell_qc$subsets_ribo_percent <= !!cfg$MIN_RIBO_RATIO * 100
    ),
    custom_filters = purrr::map(custom_filters_raw, ~ as.logical(.) %>% tidyr::replace_na(replace = FALSE)),
    custom_filter = Reduce("|", custom_filters),

    ## -- Add filters to sce and create Seurat object.
    sce_unfiltered = sce_add_colData(
      sce_valid_cells,
      df = data.frame(cell_qc, discard_qc = qc_filter, discard_custom = custom_filter)
    ),
    sce_unfiltered_info = save_object_info(sce_unfiltered),
    sce_unfiltered_plotlist = list(
      plot_colData(sce_unfiltered, y = "total", colour_by = "discard_qc", title = "Total UMI count", scale_y = ggplot2::scale_y_log10()),
      plot_colData(sce_unfiltered, y = "detected", colour_by = "discard_qc", title = "Detected features", scale_y = ggplot2::scale_y_log10()),
      plot_colData(sce_unfiltered, y = "subsets_mito_percent", colour_by = "discard_qc", title = "Percentage of expressed mitochondrial genes"),
      plot_colData(sce_unfiltered, y = "subsets_ribo_percent", colour_by = "discard_qc", title = "Percentage of expressed ribosomal genes"),
      plot_colData(sce_unfiltered, x = "total", y = "subsets_mito_percent", colour_by = "discard_qc", scale_x = ggplot2::scale_x_log10()),
      plot_colData(sce_unfiltered, x = "total", y = "subsets_ribo_percent", colour_by = "discard_qc", scale_x = ggplot2::scale_x_log10()),
      plot_colData(sce_unfiltered, x = "total", y = "detected", colour_by = "discard_qc", scale_x = ggplot2::scale_x_log10())
    ),

    ## -- sce filtered by dataset-sensitive filters
    sce_qc_filter = sce_unfiltered[, !sce_unfiltered$discard_qc],
    sce_qc_filter_rowSums = counts(sce_qc_filter) %>% rowSums(),
    sce_qc_gene_filter = get_gene_filter(sce_qc_filter, min_ratio_cells = !!cfg$MIN_RATIO_CELLS, min_umi = !!cfg$MIN_UMI),
    sce_qc_filter_genes = sce_qc_filter[!sce_qc_gene_filter, ],
    sce_qc_filter_genes_info = save_object_info(sce_qc_filter_genes),

    ## -- sce filtered by custom filters
    sce_custom_filter = sce_unfiltered[, !sce_unfiltered$discard_custom],
    sce_custom_filter_rowSums = counts(sce_custom_filter) %>% rowSums(),
    sce_custom_gene_filter = get_gene_filter(sce_custom_filter, min_ratio_cells = !!cfg$MIN_RATIO_CELLS, min_umi = !!cfg$MIN_UMI),
    sce_custom_filter_genes = sce_custom_filter[!sce_custom_gene_filter, ],
    sce_custom_filter_genes_info = save_object_info(sce_custom_filter_genes),

    ## -- Create a history of cell and gene filtering.
    sce_history = sce_history_fn(sce_unfiltered, sce_qc_filter_genes, sce_custom_filter_genes),
    sce_history_plot = sce_history_plot_fn(sce_history),

    ## -- Create plots of filters.
    sce_qc_filter_genes_plotlist = list(
      plot_colData(sce_qc_filter_genes, y = "total", colour_by = "discard_custom", title = "Total UMI count", scale_y = ggplot2::scale_y_log10()),
      plot_colData(sce_qc_filter_genes, y = "detected", colour_by = "discard_custom", title = "Detected features", scale_y = ggplot2::scale_y_log10()),
      plot_colData(sce_qc_filter_genes, y = "subsets_mito_percent", colour_by = "discard_custom", title = "Percentage of expressed mitochondrial genes"),
      plot_colData(sce_qc_filter_genes, y = "subsets_ribo_percent", colour_by = "discard_custom", title = "Percentage of expressed ribosomal genes"),
      plot_colData(sce_qc_filter_genes, x = "total", y = "subsets_mito_percent", colour_by = "discard_custom", scale_x = ggplot2::scale_x_log10()),
      plot_colData(sce_qc_filter_genes, x = "total", y = "subsets_ribo_percent", colour_by = "discard_custom", scale_x = ggplot2::scale_x_log10()),
      plot_colData(sce_qc_filter_genes, x = "total", y = "detected", colour_by = "discard_custom", scale_x = ggplot2::scale_x_log10())
    ),
    sce_custom_filter_genes_plotlist = list(
      plot_colData(sce_custom_filter_genes, y = "total", colour_by = "discard_qc", title = "Total UMI count", scale_y = ggplot2::scale_y_log10()),
      plot_colData(sce_custom_filter_genes, y = "detected", colour_by = "discard_qc", title = "Detected features", scale_y = ggplot2::scale_y_log10()),
      plot_colData(sce_custom_filter_genes, y = "subsets_mito_percent", colour_by = "discard_qc", title = "Percentage of expressed mitochondrial genes"),
      plot_colData(sce_custom_filter_genes, y = "subsets_ribo_percent", colour_by = "discard_qc", title = "Percentage of expressed ribosomal genes"),
      plot_colData(sce_custom_filter_genes, x = "total", y = "subsets_mito_percent", colour_by = "discard_qc", scale_x = ggplot2::scale_x_log10()),
      plot_colData(sce_custom_filter_genes, x = "total", y = "subsets_ribo_percent", colour_by = "discard_qc", scale_x = ggplot2::scale_x_log10()),
      plot_colData(sce_custom_filter_genes, x = "total", y = "detected", colour_by = "discard_qc", scale_x = ggplot2::scale_x_log10())
    ),

    ## -- Final sce object.
    sce_selected = sce_selected_fn(
      sce_qc_filter_genes = sce_qc_filter_genes,
      sce_custom_filter_genes = sce_custom_filter_genes,
      save_dataset_sensitive_filtering = !!cfg$SAVE_DATASET_SENSITIVE_FILTERING
    ),

    ## -- If a single ENSEMBL ID has multiple symbols, gene descriptions, or ENTREZ IDs, they are collapsed by comma (`,`).
    ## -- ENSEMBL ID is used as a symbol for ENSEMBL IDs with unknown symbols.
    ## -- ENSEMBL ID is appended to symbols having multiple ENSEMBL IDs
    ## -- (e.g. TBCE has both ENSG00000285053 and ENSG00000284770 ENSEMBL IDs assigned ->
    ## -- its symbol is changed to TBCE_ENSG00000285053 and TBCE_ENSG00000284770).
    gene_annotation = make_gene_annotation(sce_selected, annotation_db_file = !!cfg_main$ANNOTATION_DB_FILE),

    ## -- Final object: cells and genes filtered, annotated genes.
    sce_final_input_qc = sce_final_input_qc_fn(sce_selected, gene_annotation = gene_annotation),
    sce_final_input_qc_info = save_object_info(sce_final_input_qc),

    ## -- HTML report
    report_input_qc = target(
      generate_stage_report(
        rmd_file = knitr_in(!!cfg$INPUT_QC_REPORT_RMD_FILE),
        out_html_file_name = file_out(!!cfg$INPUT_QC_REPORT_HTML_FILE),
        css_file = file_in(!!cfg_main$CSS_FILE),
        message = !!cfg$INPUT_QC_KNITR_MESSAGE,
        warning = !!cfg$INPUT_QC_KNITR_WARNING,
        echo = !!cfg$INPUT_QC_KNITR_ECHO,
        other_deps = list(
          file_in(!!here("Rmd/common/_header.Rmd")),
          file_in(!!here("Rmd/common/_footer.Rmd")),
          file_in(!!here("Rmd/single_sample/01_input_qc_children/empty_droplets.Rmd")),
          file_in(!!here("Rmd/single_sample/01_input_qc_children/cell_filtering_qc.Rmd")),
          file_in(!!here("Rmd/single_sample/01_input_qc_children/cell_filtering_custom.Rmd")),
          file_in(!!here("Rmd/single_sample/01_input_qc_children/gene_filtering_qc.Rmd")),
          file_in(!!here("Rmd/single_sample/01_input_qc_children/gene_filtering_custom.Rmd"))
        ),
        drake_cache_dir = !!cfg_pipeline$DRAKE_CACHE_DIR
      ),
      format = "file"
    )
  )
}

#' @description  A plan for 02_norm_clustering stage. The following subplans are included:
#'
#' - [get_clustering_graph_subplan()], [get_clustering_kmeans_subplan()], [get_clustering_sc3_subplan()]
#'   - Bound together with [get_clustering_subplan()]
#' - [get_cell_annotation_subplan()]
#' - [get_dimred_plots_other_vars_subplan()]
#' @rdname get_subplan_single_sample
#' @export
get_norm_clustering_subplan <- function(cfg, cfg_pipeline, cfg_main) {
  any_clustering_enabled <- any(
    cfg$CLUSTER_GRAPH_LOUVAIN_ENABLED, cfg$CLUSTER_GRAPH_WALKTRAP_ENABLED, cfg$CLUSTER_GRAPH_LEIDEN_ENABLED,
    cfg$CLUSTER_KMEANS_K_ENABLED, cfg$CLUSTER_KMEANS_KBEST_ENABLED,
    cfg$CLUSTER_SC3_ENABLED
  )

  plan <- drake::drake_plan(
    ## -- Save config.
    config_norm_clustering = !!cfg,

    barcodes = colnames(sce_final_input_qc),

    ## -- Prepare cell cycle genes.
    cc_genes = cc_genes_fn(
      sce_final_input_qc,
      organism = !!cfg_main$ORGANISM, annotation_db_file = !!cfg_main$ANNOTATION_DB_FILE
    ),

    ## -- Calculate cell cycle score.
    sce_cc = sce_cc_fn(sce_final_input_qc, cc_genes = cc_genes, data = NULL),

    ## -- Normalization will be dispatched according to NORMALIZATION_TYPE:
    ## -- "scran": scran_normalization()
    ## -- "sctransform": sctransform_normalization()
    ## -- "none": skip normalization. This is assuming you have performed the normalization before and in the
    ## --         01_input_qc you are importing a SingleCellExperiment object.
    sce_norm = sce_norm_fn(
      sce_cc,
      norm_type = !!cfg$NORMALIZATION_TYPE,

      ## -- scran parameters
      use_quickcluster = !!cfg$SCRAN_USE_QUICKCLUSTER,
      quickcluster_method = !!cfg$SCRAN_QUICKCLUSTER_METHOD,
      BPPARAM = ignore(BiocParallel::bpparam()),

      ## -- sctransform parameters
      vars_to_regress = !!cfg$SCT_VARS_TO_REGRESS,
      n_hvg = !!cfg$SCT_N_HVG,
      seed = !!cfg_pipeline$SEED
    ),

    ## -- Find highly variable genes (HVGs) and assign them to SCE object.
    ## -- If specified, remove cell-cycle related genes prior to HVG selection.
    sce_norm_hvg = target(
      sce_norm_hvg_fn(
        sce_norm,
        hvg_selection_value = !!cfg$HVG_SELECTION_VALUE,
        hvg_metric = !!cfg$HVG_METRIC,
        hvg_selection = !!cfg$HVG_SELECTION,
        hvg_rm_cc_genes = !!cfg$HVG_RM_CC_GENES,
        hvg_cc_genes_var_expl_threshold = !!cfg$HVG_CC_GENES_VAR_EXPL_THRESHOLD,
        BPPARAM = ignore(BiocParallel::bpparam())
      ),

      ## -- For some reason, qs format throws error for this target.
      format = "rds"
    ),
    sce_norm_hvg_info = save_object_info(sce_norm_hvg),
    hvg_plot = plot_hvg(sce_norm_hvg),
    variance_explained = scater::getVarianceExplained(sce_norm_hvg, variables = c("s_score", "g2m_score", "cc_difference", "total")),

    ## -- Identify and remove cell doublets.
    ## -- Deprecated function: scran::doubletCells()
    doublet_density = scDblFinder::computeDoubletDensity(sce_norm_hvg),
    sce_rm_doublets = sce_rm_doublets_fn(sce_norm_hvg, doublet_density, max_doublet_score = !!cfg$MAX_DOUBLET_SCORE),

    ## -- Compute PCA.
    sce_pca = sce_calc_pca(sce_rm_doublets, name = "pca", BPPARAM = ignore(BiocParallel::bpparam())),

    ## -- PCA plots.
    pca_phase_plots = pca_phase_plots_fn(sce_pca),
    pca_doublet_plot = plotReducedDim_mod(sce_pca, "pca", colour_by = "doublet_score", title = "Doublet Score"),
    pca_total_plot = plotReducedDim_mod(sce_pca, "pca", colour_by = "total", title = "Total number of UMI"),

    ## -- PCs selection.
    pca_percent_var = attr(reducedDim(sce_pca, "pca"), "percentVar"),
    pca_elbow_pcs = PCAtools::findElbowPoint(pca_percent_var),
    pca_gene_var_pcs = get_pca_gene_var_pcs(sce_pca, BPPARAM = ignore(BiocParallel::bpparam())),
    pca_selected_pcs_plot = make_pca_selected_pcs_plot(
      pca_percent_var, pca_elbow_pcs, pca_gene_var_pcs,
      pca_forced_pcs = !!cfg$PCA_FORCED_PCS
    ),
    sce_pca_selected_pcs = get_pca_selected_pcs(
      sce_pca,
      pca_elbow_pcs = pca_elbow_pcs,
      pca_gene_var_pcs = pca_gene_var_pcs,
      pca_selection_method = !!cfg$PCA_SELECTION_METHOD,
      pca_forced_pcs = !!cfg$PCA_FORCED_PCS
    ),

    ## -- Compute dimreds.
    sce_dimred = sce_compute_dimreds(
      sce_pca_selected_pcs,
      tsne_perp = !!cfg$TSNE_PERP,
      tsne_max_iter = !!cfg$TSNE_MAX_ITER,
      BPPARAM = ignore(BiocParallel::bpparam())
    ),

    additional_cell_data = additional_cell_data_fn(file_in(!!cfg$ADDITIONAL_CELL_DATA_FILE)),
    ## -- Save colData. No new column is added between sce_rm_doublets and sce_dimred and so we can use
    ## -- this colData in cluster markers/contrasts stages without unnecessary dependencies.
    cell_data = cell_data_fn(
      col_data = colData(sce_rm_doublets) %>% as.data.frame(),
      clusters_all = clusters_all,
      cell_annotation_labels = cell_annotation_labels,
      cell_groupings = !!cfg$CELL_GROUPINGS,
      additional_cell_data = additional_cell_data,
      pipeline_type = "single_sample"
    ),

    ## -- Final SCE object.
    sce_final_norm_clustering = sce_add_cell_data(sce_dimred, cell_data),

    ## -- HTML report
    report_norm_clustering = target(
      generate_stage_report(
        rmd_file = knitr_in(!!cfg$NORM_CLUSTERING_REPORT_RMD_FILE),
        out_html_file_name = file_out(!!cfg$NORM_CLUSTERING_REPORT_HTML_FILE),
        css_file = file_in(!!cfg_main$CSS_FILE),
        message = !!cfg$NORM_CLUSTERING_KNITR_MESSAGE,
        warning = !!cfg$NORM_CLUSTERING_KNITR_WARNING,
        echo = !!cfg$NORM_CLUSTERING_KNITR_ECHO,
        other_deps = list(
          file_in(!!here("Rmd/common/_header.Rmd")),
          file_in(!!here("Rmd/common/_footer.Rmd")),
          file_in(!!fs::dir_ls(here("Rmd/common/clustering"), fail = FALSE))
        ),
        drake_cache_dir = !!cfg_pipeline$DRAKE_CACHE_DIR
      ),
      format = "file"
    ),
    report_norm_clustering_simple = target(
      generate_stage_report(
        rmd_file = knitr_in(!!cfg$NORM_CLUSTERING_REPORT_SIMPLE_RMD_FILE),
        out_html_file_name = file_out(!!cfg$NORM_CLUSTERING_REPORT_SIMPLE_HTML_FILE),
        css_file = file_in(!!cfg_main$CSS_FILE),
        message = FALSE,
        warning = FALSE,
        echo = FALSE,
        other_deps = list(
          file_in(!!here("Rmd/common/_header.Rmd")),
          file_in(!!here("Rmd/common/_footer.Rmd")),
          file_in(!!fs::dir_ls(here("Rmd/common/clustering"), fail = FALSE))
        ),
        drake_cache_dir = !!cfg_pipeline$DRAKE_CACHE_DIR
      ),
      format = "file"
    )
  )

  plan_clustering <- get_clustering_subplan(
    cfg,
    sce_clustering_target_name = "sce_pca_selected_pcs",
    sce_dimred_plots_target_name = "sce_dimred",
    dimred = "pca",
    report_dimred_names = cfg$NORM_CLUSTERING_REPORT_DIMRED_NAMES,
    dimred_plots_out_dir = cfg$NORM_CLUSTERING_DIMRED_PLOTS_OUT_DIR,
    other_plots_out_dir = cfg$NORM_CLUSTERING_OTHER_PLOTS_OUT_DIR,
    is_integration = FALSE,
    seed = cfg_pipeline$SEED
  )

  plan_cell_annotation <- get_cell_annotation_subplan(
    sce_target_name = "sce_rm_doublets",
    sce_dimred_plots_target_name = "sce_final_norm_clustering",
    cell_annotation_sources = cfg$CELL_ANNOTATION_SOURCES,
    cell_annotation_out_dir = cfg$NORM_CLUSTERING_CELL_ANNOTATION_OUT_DIR,
    report_dimred_names = cfg$NORM_CLUSTERING_REPORT_DIMRED_NAMES,
    dimred_plots_out_dir = cfg$NORM_CLUSTERING_DIMRED_PLOTS_OUT_DIR,
    do_heatmaps_ = any_clustering_enabled
  )

  plan_dimred_plots_other_vars <- get_dimred_plots_other_vars_subplan(
    sce_target_name = "sce_final_norm_clustering",
    report_dimred_plots_other = cfg$NORM_CLUSTERING_REPORT_DIMRED_PLOTS_OTHER,
    report_dimred_names = cfg$NORM_CLUSTERING_REPORT_DIMRED_NAMES,
    dimred_plots_out_dir = cfg$NORM_CLUSTERING_DIMRED_PLOTS_OUT_DIR
  )

  ## -- Selected markers plots.
  if (!is_null(cfg$SELECTED_MARKERS_FILE)) {
    plan_selected_markers <- drake::drake_plan(
      selected_markers_df = target(
        readr::read_csv(file_in(!!cfg$SELECTED_MARKERS_FILE), col_names = c("group", "markers"), col_types = "cc") %>%
          tidyr::crossing(dimred_name = !!cfg$NORM_CLUSTERING_REPORT_DIMRED_NAMES)
      ),
      selected_markers_plots_by = selected_markers_df$dimred_name,
      selected_markers_plots = target(
        selected_markers_plots_fn(
          sce_final_norm_clustering,
          selected_markers_df = selected_markers_df
        ),
        dynamic = group(selected_markers_df, .by = selected_markers_plots_by)
      ),
      selected_markers_plots_files = target(
        save_selected_markers_plots_files(
          selected_markers_plots,
          selected_markers_out_dir = !!cfg$NORM_CLUSTERING_SELECTED_MARKERS_OUT_DIR,
          is_integration = FALSE
        ),

        dynamic = map(selected_markers_plots)
      ),
      selected_markers_plots_files_out = target(
        selected_markers_plots_files$out_pdf_file,

        format = "file"
      )
    )
  } else {
    plan_selected_markers <- drake::drake_plan(
      selected_markers_df = NULL,
      selected_markers_plots_by = NULL,
      selected_markers_plots = NULL,
      selected_markers_plots_files = NULL
    )
  }

  drake::bind_plans(plan, plan_clustering, plan_cell_annotation, plan_dimred_plots_other_vars, plan_selected_markers)
}
