## -- Functions to build common drake plans.

#' @title Get a `drake` subplan common to all pipelines.
#' @param cfg A list of parameters for this stage
#'   (from single-sample or integration config directory, loaded from e.g. `cluster_markers.yaml`, etc.).
#' @inheritParams cfg_pipeline_param
#' @param cfg_main A list of general parameters
#'   (loaded from `00_main.yaml` from single-sample or integration config directory).
#' @return [drake::drake_plan()]
#'
#' @details The `pipeline_type` target is defined in the plan returned from [get_common_subplan()].
#'
#' @concept get_subplan_common
#' @name get_subplan_common
NULL

#' @description A subplan for storing configs and runtime information.
#'
#' @param pipeline_type_ A character scalar indicating the type of pipeline.
#' Based on the pipeline type, some targets depend on different targets.
#'
#' @rdname get_subplan_common
#' @export
get_common_subplan <- function(cfg_pipeline, cfg_main, pipeline_type_ = c("single_sample", "integration")) {
  pipeline_type_ <- arg_match(pipeline_type_)

  drake::drake_plan(
    config_pipeline = !!cfg_pipeline,
    config_main = !!cfg_main,
    pipeline_type = !!pipeline_type_,
    sessioninfo_pretty = sessioninfo::session_info(),
    sessioninfo_base = utils::sessionInfo(),
    biocmanager_version = BiocManager::version(),
    external_libs = extSoftVersion(),
    datetime = Sys.time()
  )
}

#' @title Get a subplan for dimensionality reduction plots of selected variables.
#' @param sce_target_name A character scalar: name of target that stores SCE object, which will be used for plotting.
#' @param report_dimred_plots_other A character vector: variables in `colData(<sce_target_name>)` to plot.
#' @param report_dimred_names A character vector: dimred names for plotting (`"pca"`, `"tsne"`, `"umap"`).
#' @param dimred_plots_out_dir A character scalar: output directory to save PDFs into.
#' @return [drake::drake_plan()]
#'
#' @concept get_subplan_common
#' @export
get_dimred_plots_other_vars_subplan <- function(sce_target_name,
                                                report_dimred_plots_other,
                                                report_dimred_names,
                                                dimred_plots_out_dir) {
  if (!is.null(report_dimred_plots_other)) {
    plan_dimred_plots_other_vars <- drake::drake_plan(
      dimred_plots_other_vars_params = dimred_plots_other_vars_params_df_fn(
        dimred_names = !!report_dimred_names,
        dimred_plots_other = !!report_dimred_plots_other,
        out_dir = !!dimred_plots_out_dir
      ),
      dimred_plots_other_vars = target(
        dimred_plots_from_params_df(
          !!sym(sce_target_name),
          dimred_plots_params_df = dimred_plots_other_vars_params
        ),
        dynamic = map(dimred_plots_other_vars_params)
      ),
      dimred_plots_other_vars_files = dplyr::select(dimred_plots_other_vars, -plot),
      dimred_plots_other_vars_files_out = target(
        c(dimred_plots_other_vars_files$out_pdf_file, dimred_plots_other_vars_files$out_png_file),
        format = "file"
      )
    )
  } else {
    plan_dimred_plots_other_vars <- drake::drake_plan(
      dimred_plots_other_vars_params = NULL,
      dimred_plots_other_vars = NULL,
      dimred_plots_other_vars_files = NULL,
      dimred_plots_other_vars_files_out = NULL
    )
  }

  plan_dimred_plots_other_vars
}

#' @description A subplan for cluster_markers stage.
#' @rdname get_subplan_common
#' @export
get_cluster_markers_subplan <- function(cfg, cfg_pipeline, cfg_main) {
  drake::drake_plan(
    ## -- Save config.
    config_cluster_markers = !!cfg,

    ## -- Get SCE object acccording to the pipeline type.
    sce_dimred_cluster_markers = if (pipeline_type == "single_sample") sce_dimred else sce_int_clustering_final,
    sce_final_cluster_markers = if (pipeline_type == "single_sample") sce_final_norm_clustering else sce_int_clustering_final,
    sce_cluster_markers = if (pipeline_type == "single_sample") sce_rm_doublets else sce_int_clustering_final,
    cluster_markers_params = cluster_markers_params_fn(!!cfg$CLUSTER_MARKERS_SOURCES, cell_data),
    cluster_markers_test_params = cluster_markers_test_params_fn(cluster_markers_params),
    cluster_markers_heatmap_params = cluster_markers_heatmap_params_fn(cluster_markers_params),
    cluster_markers_plot_params = cluster_markers_plot_params_fn(cluster_markers_params),
    cluster_markers_dimred_plot_params = cluster_markers_plot_params %>% dplyr::distinct(source_column, plot_dimreds),
    cluster_markers_raw = target(
      scran_markers(sce_cluster_markers, params = cluster_markers_test_params, markers_type = "global"),
      dynamic = map(cluster_markers_test_params)
    ),

    ## -- Add summary logFC from t-test to Wilcox test results.
    cluster_markers = cluster_markers_fn(cluster_markers_raw),

    ## -- Replace nested DataFrames with pairwise results by only their LFCs or AUCs.
    cluster_markers_processed = cluster_markers_processed_fn(cluster_markers),
    cluster_markers_out = cluster_markers_out_fn(cluster_markers_processed),

    ## -- Row z-score in assays$RNA@scale.data
    seu_for_cluster_markers_heatmaps = create_seu_for_heatmaps(sce_dimred_cluster_markers, calc_zscore = TRUE),

    ## -- Cluster markers heatmaps.
    cluster_markers_heatmaps_df = cluster_markers_heatmaps_df_fn(
      cluster_markers_processed, cluster_markers_heatmap_params, !!cfg$CLUSTER_MARKERS_HEATMAPS_OUT_DIR
    ),
    cluster_markers_heatmaps_files = target(
      marker_heatmaps_wrapper(
        seu = seu_for_cluster_markers_heatmaps,
        params = cluster_markers_heatmaps_df,
        marker_type = "global"
      ),
      dynamic = map(cluster_markers_heatmaps_df)
    ),

    ## -- Cluster markers plots.
    cluster_markers_plots_top = markers_plots_top(
      cluster_markers_processed, cluster_markers_plot_params,
      out_dir = !!cfg$CLUSTER_MARKERS_PLOTS_BASE_OUT_DIR
    ),
    cluster_markers_plots_files = target(
      markers_plots_files(sce_dimred_cluster_markers, cluster_markers_plots_top, dry = !!(!cfg$MAKE_CLUSTER_MARKERS_PLOTS)),
      dynamic = map(cluster_markers_plots_top),
      format = "file"
    ),

    ## -- Dimred plots of source columns.
    cluster_markers_dimred_plots = target(
      markers_dimred_plots(sce_final_cluster_markers, cluster_markers_dimred_plot_params),
      dynamic = map(cluster_markers_dimred_plot_params)
    ),
    cluster_markers_dimred_plots_files = target(
      markers_dimred_plots_files(cluster_markers_dimred_plots, !!cfg$CLUSTER_MARKERS_DIMRED_PLOTS_BASE_OUT_DIR),
      dynamic = map(cluster_markers_dimred_plots)
    ),

    ## -- Cluster markers HTML tables.
    cluster_markers_for_tables = markers_for_tables(
      cluster_markers_out, cluster_markers_heatmaps_df, cluster_markers_plots_top, cluster_markers_dimred_plots_files,
      out_dir = !!cfg$CLUSTER_MARKERS_TABLES_OUT_DIR, ensembl_species = !!cfg_main$ENSEMBL_SPECIES,
      cluster_markers_plots_files, cluster_markers_heatmaps_files
    ),
    cluster_markers_table_template_file = file_in(!!cfg$CLUSTER_MARKERS_TABLE_TEMPLATE_RMD_FILE),
    cluster_markers_table_files = target(
      markers_table_files(
        cluster_markers_for_tables,
        cluster_markers_table_template_file,
        marker_type = "global",
        drake_cache_dir = !!cfg_pipeline$DRAKE_CACHE_DIR
      ),
      format = "file",
      dynamic = map(cluster_markers_for_tables)
    ),
    report_cluster_markers = target(
      generate_stage_report(
        rmd_file = knitr_in(!!cfg$CLUSTER_MARKERS_REPORT_RMD_FILE),
        out_html_file_name = file_out(!!cfg$CLUSTER_MARKERS_REPORT_HTML_FILE),
        css_file = file_in(!!cfg_main$CSS_FILE),
        message = FALSE,
        warning = FALSE,
        echo = FALSE,
        other_deps = list(
          file_in(!!here("Rmd/common/_header.Rmd")),
          file_in(!!here("Rmd/common/_footer.Rmd"))
        ),
        drake_cache_dir = !!cfg_pipeline$DRAKE_CACHE_DIR
      ),
      format = "file"
    )
  )
}

#' @description A subplan for contrasts stage (differential expression).
#' @rdname get_subplan_common
#' @export
get_contrasts_subplan <- function(cfg, cfg_pipeline, cfg_main) {
  drake::drake_plan(
    ## -- Save config.
    config_contrasts = !!cfg,

    ## -- Get SCE object acccording to the pipeline type.
    sce_dimred_contrasts = if (pipeline_type == "single_sample") sce_dimred else sce_int_clustering_final,
    sce_final_contrasts = if (pipeline_type == "single_sample") sce_final_norm_clustering else sce_int_clustering_final,
    sce_contrasts = if (pipeline_type == "single_sample") sce_rm_doublets else sce_int_clustering_final,
    contrasts_params = contrasts_params_fn(!!cfg$CONTRASTS_SOURCES, cell_data),
    contrasts_test_params = contrasts_params %>%
      dplyr::distinct(name, test_type, source_column, block_column, test_type, groups, blocks, lfc_test, std_lfc),
    contrasts_heatmap_params = contrasts_heatmap_params_fn(contrasts_params, contrasts_test_params),
    contrasts_plot_params = contrasts_plot_params_fn(contrasts_params),
    contrasts_dimred_plot_params = contrasts_plot_params %>% dplyr::distinct(source_column, plot_dimreds),
    contrasts_raw = target(
      scran_markers(sce_contrasts, params = contrasts_test_params, markers_type = "contrast"),
      dynamic = map(contrasts_test_params)
    ),
    contrasts = contrasts_fn(contrasts_raw, contrasts_params),
    contrasts_out = contrasts_out_fn(contrasts),

    ## -- Contrast heatmaps.
    seu_for_contrasts_heatmaps = create_seu_for_heatmaps(sce_dimred_contrasts, calc_zscore = FALSE),
    contrasts_heatmaps_df = contrasts_heatmaps_df_fn(contrasts, contrasts_heatmap_params, !!cfg$CONTRASTS_HEATMAPS_OUT_DIR),
    contrasts_heatmaps_files = target(
      marker_heatmaps_wrapper(
        seu = seu_for_contrasts_heatmaps,
        params = contrasts_heatmaps_df,
        marker_type = "contrast"
      ),
      dynamic = map(contrasts_heatmaps_df)
    ),

    ## -- Contrast marker plots.
    contrasts_plots_top = markers_plots_top(
      contrasts, contrasts_plot_params,
      out_dir = !!cfg$CONTRASTS_PLOTS_BASE_OUT_DIR, marker_type = "contrast"
    ),
    contrasts_plots_files = target(
      markers_plots_files(sce_dimred_contrasts, contrasts_plots_top, dry = !!(!cfg$MAKE_CONTRASTS_PLOTS)),
      dynamic = map(contrasts_plots_top),
      format = "file"
    ),

    ## -- Dimred plots of source columns.
    contrasts_dimred_plots = target(
      markers_dimred_plots(sce_final_contrasts, contrasts_dimred_plot_params),
      dynamic = map(contrasts_dimred_plot_params)
    ),
    contrasts_dimred_plots_files = target(
      markers_dimred_plots_files(contrasts_dimred_plots, !!cfg$CONTRASTS_DIMRED_PLOTS_BASE_OUT_DIR),
      dynamic = map(contrasts_dimred_plots)
    ),

    ## -- Contrasts HTML tables.
    contrasts_for_tables = markers_for_tables(
      contrasts_out, contrasts_heatmaps_df, contrasts_plots_top, contrasts_dimred_plots_files,
      out_dir = !!cfg$CONTRASTS_TABLES_OUT_DIR, ensembl_species = !!cfg_main$ENSEMBL_SPECIES,
      contrasts_plots_files, contrasts_heatmaps_files
    ),
    contrasts_table_template_file = file_in(!!cfg$CONTRASTS_TABLE_TEMPLATE_RMD_FILE),
    contrasts_table_files = target(
      markers_table_files(
        contrasts_for_tables,
        contrasts_table_template_file,
        marker_type = "contrast",
        drake_cache_dir = !!cfg_pipeline$DRAKE_CACHE_DIR
      ),
      format = "file",
      dynamic = map(contrasts_for_tables)
    ),
    report_contrasts = target(
      generate_stage_report(
        rmd_file = knitr_in(!!cfg$CONTRASTS_REPORT_RMD_FILE),
        out_html_file_name = file_out(!!cfg$CONTRASTS_REPORT_HTML_FILE),
        css_file = file_in(!!cfg_main$CSS_FILE),
        message = FALSE,
        warning = FALSE,
        echo = FALSE,
        other_deps = list(
          file_in(!!here("Rmd/common/_header.Rmd")),
          file_in(!!here("Rmd/common/_footer.Rmd"))
        ),
        drake_cache_dir = !!cfg_pipeline$DRAKE_CACHE_DIR
      ),
      format = "file"
    )
  )
}
