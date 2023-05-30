## -- Functions to build drake plans for integration analysis.

#' @title Get a `drake` plan for a stage of integration pipeline.
#'
#' @param cfg A list of parameters for this stage
#'   (from integration config directory, e.g. loaded from `01_integration.yaml`, etc.).
#' @inheritParams cfg_pipeline_param
#' @param cfg_main A list of general parameters
#'   (loaded from `00_main.yaml` from integration config directory).
#'
#' @return [drake::drake_plan()]
#'
#' @concept get_subplan_integration
#' @name get_subplan_integration
NULL

#' @description A plan for 01_integration stage.
#' @rdname get_subplan_integration
#' @export
get_integration_subplan <- function(cfg, cfg_pipeline, cfg_main) {
  plan <- drake::drake_plan(
    ## -- Save config.
    config_integration = !!cfg,

    ## -- Read all single samples from their drake caches.
    ## -- We cannot easily observe if a particular target from a drake cache has changed,
    ## -- so we need to always load all sce objects before pipeline run.
    ## -- Also, some constraints are checked (common normalization method and HVG metrics).
    sce_int_import = target(
      sce_int_import_fn(!!cfg$INTEGRATION_SOURCES, hvg_combination = !!cfg$HVG_COMBINATION_INT),

      trigger = trigger(condition = TRUE)
    ),

    ## -- Compute fast SNN clustering for each single-sample, used for integration diagnostics.
    sce_int_raw_snn_clustering = sce_int_raw_snn_clustering_fn(
      sce_int_import,
      snn_k = !!cfg$INTEGRATION_SNN_K,
      snn_type = !!cfg$INTEGRATION_SNN_TYPE,
      snn_clustering_method = !!cfg$INTEGRATION_SNN_CLUSTERING_METHOD,
      BPPARAM = ignore(BiocParallel::bpparam())
    ),

    ## -- Subset to common colData, and features (rows) and their corresponding metadata (hvg_ids, hvg_metric_fit).
    sce_int_processed = sce_int_processed_fn(sce_int_import),

    ## -- List of sce objects normalized for inter-batch sequencing depth.
    ## -- This is the final normalization for "uncorrected" integration, that will just be a sce object merged from these ones.
    sce_int_multibatchnorm = batchelor::multiBatchNorm(sce_int_processed, BPPARAM = ignore(BiocParallel::bpparam())),

    ## -- HVGs.
    hvg_int_list = hvg_int_list_fn(
      sce_int_multibatchnorm,
      hvg_combination = !!cfg$HVG_COMBINATION_INT,
      hvg_selection_value = !!cfg$HVG_SELECTION_VALUE_INT,
      hvg_selection = !!cfg$HVG_SELECTION_INT
    ),

    ## -- Without cell-cycle related genes, if requested.
    hvg_int = hvg_int_list$hvg_int,
    ## -- With all genes, could be NULL.
    hvg_int_with_cc = hvg_int_list$hvg_int_with_cc,

    ## -- Load integration parameters.
    integration_methods_df = integration_methods_df_fn(!!cfg$INTEGRATION_METHODS, hvg_int, hvg_int_with_cc),
    integration_params_df = dplyr::select(integration_methods_df, name, hvg_rm_cc_genes, hvg_int, integration_params),

    ## -- Perform the integrations.
    sce_int_df = target(
      sce_int_df_fn(sce_int_multibatchnorm, integration_params_df, BPPARAM = ignore(BiocParallel::bpparam())),

      dynamic = map(integration_params_df)
    ),

    ## -- Save uncorrected sce separately.
    ## -- This tibble will have one or two rows, based on parameter for removal of CC related genes.
    sce_int_uncorrected_df = dplyr::filter(sce_int_df, name == "uncorrected"),
    gene_annotation = rowData(sce_int_uncorrected_df[1, ]$sce_int[[1]])[, c("ENSEMBL", "SYMBOL", "ENTREZID", "GENENAME")],
    single_samples_df = metadata(sce_int_uncorrected_df$sce_int[[1]])$single_samples_df,
    hvg_plots_int_df = dplyr::mutate(sce_int_uncorrected_df, hvg_plot = lapply(sce_int, hvg_plot_int_fn)) %>%
      dplyr::select(-integration_params, -sce_int),
    common_features_count = nrow(sce_int_uncorrected_df$sce_int[[1]]),

    ## -- Compute PCA for all integration methods.
    pca_params_df = dplyr::select(sce_int_df, name, hvg_rm_cc_genes, sce_int) %>%
      dplyr::left_join(
        dplyr::select(integration_methods_df, name, hvg_rm_cc_genes, pca_selection_method, pca_forced_pcs),
        by = c("name", "hvg_rm_cc_genes")
      ),
    sce_int_pca_df = target(
      sce_int_pca_df_fn(pca_params_df, BPPARAM = ignore(BiocParallel::bpparam())),

      dynamic = map(pca_params_df)
    ),
    sce_int_pca_df_for_report = dplyr::select(sce_int_pca_df, -sce_pca),

    ## -- MNN clustering for integration diagnostics.
    sce_int_clustering_df = target(
      sce_int_clustering_df_fn(
        sce_int_pca_df,
        snn_k = !!cfg$INTEGRATION_SNN_K,
        snn_type = !!cfg$INTEGRATION_SNN_TYPE,
        snn_clustering_method = !!cfg$INTEGRATION_SNN_CLUSTERING_METHOD,
        BPPARAM = ignore(BiocParallel::bpparam())
      ),

      dynamic = map(sce_int_pca_df)
    ),
    int_diagnostics_df = target(
      int_diagnostics_df_fn(sce_int_clustering_df, sce_int_raw_snn_clustering),

      dynamic = map(sce_int_clustering_df)
    ),

    ## -- Compute t-SNE and UMAP.
    dimred_params_df = dplyr::select(sce_int_pca_df, name, hvg_rm_cc_genes, sce_pca) %>%
      dplyr::left_join(
        dplyr::select(integration_methods_df, name, hvg_rm_cc_genes, tsne_perp, tsne_max_iter),
        by = c("name", "hvg_rm_cc_genes")
      ),
    sce_int_dimred_df = target(
      sce_int_dimred_df_fn(dimred_params_df, BPPARAM = ignore(BiocParallel::bpparam())),

      dynamic = map(dimred_params_df)
    ),

    ## -- Dimred plots colored by batch and CC phase.
    dimred_plots_params_df = dplyr::select(sce_int_dimred_df, name, hvg_rm_cc_genes, sce_dimred) %>%
      tidyr::expand_grid(dimred_name = c("pca", "umap", "tsne"), colour_by = c("batch", "phase")) %>%
      dplyr::mutate(dimred_name = dplyr::if_else(name == "harmony" & dimred_name == "pca", "harmony", dimred_name)),
    sce_int_dimred_plots_df = target(
      sce_int_dimred_plots_df_fn(dimred_plots_params_df),

      dynamic = map(dimred_plots_params_df)
    ),

    ## -- HTML report
    report_integration = target(
      generate_stage_report(
        rmd_file = knitr_in(!!cfg$INTEGRATION_REPORT_RMD_FILE),
        out_html_file_name = file_out(!!cfg$INTEGRATION_REPORT_HTML_FILE),
        css_file = file_in(!!cfg_main$CSS_FILE),
        message = !!cfg$INTEGRATION_KNITR_MESSAGE,
        warning = !!cfg$INTEGRATION_KNITR_WARNING,
        echo = !!cfg$INTEGRATION_KNITR_ECHO,
        other_deps = list(
          file_in(!!here("Rmd/common/_header.Rmd")),
          file_in(!!here("Rmd/common/_footer.Rmd"))
        ),
        drake_cache_dir = !!cfg_pipeline$DRAKE_CACHE_DIR
      ),

      format = "file"
    )
  )

  ## -- Selected markers plots.
  if (!is_null(cfg$SELECTED_MARKERS_FILE)) {
    plan_selected_markers <- drake::drake_plan(
      selected_markers_int_df = selected_markers_int_df_fn(file_in(!!cfg$SELECTED_MARKERS_FILE), sce_int_dimred_df),
      selected_markers_int_plots_by = selected_markers_int_df$int_rmcc_dimred,
      selected_markers_int_plots_df = target(
        selected_markers_int_plots_df_fn(
          selected_markers_int_df,
          sce_int_dimred_df
        ),

        dynamic = group(selected_markers_int_df, .by = selected_markers_int_plots_by)
      ),
      selected_markers_int_plots_files = target(
        save_selected_markers_plots_files(
          selected_markers_int_plots_df,
          selected_markers_out_dir = !!cfg$INTEGRATION_SELECTED_MARKERS_OUT_DIR,
          is_integration = TRUE
        ),

        dynamic = map(selected_markers_int_plots_df)
      ),
      selected_markers_int_plots_files_out = target(
        selected_markers_int_plots_files$out_pdf_file,

        format = "file"
      )
    )
  } else {
    plan_selected_markers <- drake::drake_plan(
      selected_markers_int_df = NULL,
      selected_markers_int_plots_by = NULL,
      selected_markers_int_plots_df = NULL,
      selected_markers_int_plots_files = NULL
    )
  }

  return(drake::bind_plans(plan, plan_selected_markers))
}

#' @description A plan for 02_int_clustering stage. The following subplans are included:
#'
#' - [get_clustering_graph_subplan()], [get_clustering_kmeans_subplan()], [get_clustering_sc3_subplan()]
#'   - Bound together with [get_clustering_subplan()]
#' - [get_cell_annotation_subplan()]
#' - [get_dimred_plots_other_vars_subplan()]
#' @rdname get_subplan_integration
#' @export
get_int_clustering_subplan <- function(cfg, cfg_pipeline, cfg_main) {
  any_clustering_enabled <- any(
    cfg$CLUSTER_GRAPH_LOUVAIN_ENABLED, cfg$CLUSTER_GRAPH_WALKTRAP_ENABLED, cfg$CLUSTER_GRAPH_LEIDEN_ENABLED,
    cfg$CLUSTER_KMEANS_K_ENABLED, cfg$CLUSTER_KMEANS_KBEST_ENABLED,
    cfg$CLUSTER_SC3_ENABLED
  )

  plan <- drake::drake_plan(
    ## -- Save config.
    config_int_clustering = !!cfg,
    sce_int_uncorrected = dplyr::filter(sce_int_uncorrected_df, hvg_rm_cc_genes == !!cfg$INTEGRATION_FINAL_METHOD_RM_CC) %>%
      dplyr::pull(sce_int) %>%
      .[[1]],

    ## -- Select the integration method result.
    sce_int_final = dplyr::filter(
      sce_int_dimred_df,
      name == !!cfg$INTEGRATION_FINAL_METHOD,
      hvg_rm_cc_genes == !!cfg$INTEGRATION_FINAL_METHOD_RM_CC
    ) %>%
      dplyr::pull(sce_dimred) %>%
      .[[1]],
    sce_int_final_info = save_object_info(sce_int_final),

    additional_cell_data = additional_cell_data_fn(file_in(!!cfg$ADDITIONAL_CELL_DATA_FILE)),
    cell_data = cell_data_fn(
      col_data = colData(sce_int_final) %>% as.data.frame(),
      clusters_all = clusters_all,
      cell_annotation_labels = cell_annotation_labels,
      cell_groupings = !!cfg$CELL_GROUPINGS,
      additional_cell_data = additional_cell_data,
      pipeline_type = "integration"
    ),

    sce_int_clustering_final = sce_add_cell_data(sce_int_final, cell_data),

    ## -- Selected markers plots for the chosen integration method.
    selected_markers_int_plots_final = if (is_null(selected_markers_int_plots_files)) {
      NULL
    } else {
      dplyr::filter(
        selected_markers_int_plots_files,
        name == !!cfg$INTEGRATION_FINAL_METHOD,
        hvg_rm_cc_genes == !!cfg$INTEGRATION_FINAL_METHOD_RM_CC
      )
    },

    ## -- HTML report
    report_int_clustering = target(
      generate_stage_report(
        rmd_file = knitr_in(!!cfg$INT_CLUSTERING_REPORT_RMD_FILE),
        out_html_file_name = file_out(!!cfg$INT_CLUSTERING_REPORT_HTML_FILE),
        css_file = file_in(!!cfg_main$CSS_FILE),
        message = !!cfg$INT_CLUSTERING_KNITR_MESSAGE,
        warning = !!cfg$INT_CLUSTERING_KNITR_WARNING,
        echo = !!cfg$INT_CLUSTERING_KNITR_ECHO,
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

  if (cfg$INTEGRATION_FINAL_METHOD == "harmony") {
    dimred <- "harmony"
  } else {
    dimred <- "pca"
  }

  plan_clustering <- get_clustering_subplan(
    cfg,
    sce_clustering_target_name = "sce_int_final",
    sce_dimred_plots_target_name = "sce_int_final",
    dimred = dimred,
    report_dimred_names = cfg$INT_CLUSTERING_REPORT_DIMRED_NAMES,
    dimred_plots_out_dir = cfg$INT_CLUSTERING_DIMRED_PLOTS_OUT_DIR,
    other_plots_out_dir = cfg$INT_CLUSTERING_OTHER_PLOTS_OUT_DIR,
    is_integration = TRUE,
    seed = cfg_pipeline$SEED
  )

  plan_cell_annotation <- get_cell_annotation_subplan(
    sce_target_name = "sce_int_final",
    sce_dimred_plots_target_name = "sce_int_clustering_final",
    cell_annotation_sources = cfg$CELL_ANNOTATION_SOURCES,
    cell_annotation_out_dir = cfg$INT_CLUSTERING_CELL_ANNOTATION_OUT_DIR,
    report_dimred_names = cfg$INT_CLUSTERING_REPORT_DIMRED_NAMES,
    dimred_plots_out_dir = cfg$INT_CLUSTERING_DIMRED_PLOTS_OUT_DIR,
    do_heatmaps_ = any_clustering_enabled
  )

  plan_dimred_plots_other_vars <- get_dimred_plots_other_vars_subplan(
    sce_target_name = "sce_int_clustering_final",
    report_dimred_plots_other = cfg$INT_CLUSTERING_REPORT_DIMRED_PLOTS_OTHER,
    report_dimred_names = cfg$INT_CLUSTERING_REPORT_DIMRED_NAMES,
    dimred_plots_out_dir = cfg$INT_CLUSTERING_DIMRED_PLOTS_OUT_DIR
  )

  drake::bind_plans(plan, plan_clustering, plan_cell_annotation, plan_dimred_plots_other_vars)
}
