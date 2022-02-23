.create_dirs <- function(dirs) {
  lapply(dirs, fs::dir_create, recurse = TRUE)
  invisible(NULL)
}

.create_common_dirs <- function(cfg) {
  dirs <- c(
    "BASE_OUT_DIR",
    "CLUSTER_MARKERS_BASE_OUT_DIR", "CLUSTER_MARKERS_HEATMAPS_OUT_DIR", "CLUSTER_MARKERS_PLOTS_BASE_OUT_DIR",
    "CLUSTER_MARKERS_DIMRED_PLOTS_BASE_OUT_DIR", "CLUSTER_MARKERS_TABLES_OUT_DIR",
    "CONTRASTS_BASE_OUT_DIR", "CONTRASTS_HEATMAPS_OUT_DIR", "CONTRASTS_PLOTS_BASE_OUT_DIR",
    "CONTRASTS_DIMRED_PLOTS_BASE_OUT_DIR", "CONTRASTS_TABLES_OUT_DIR"
  )
  .create_dirs(cfg[dirs])
  invisible(NULL)
}

.create_single_sample_dirs <- function(cfg) {
  dirs <- c(
    "INPUT_QC_BASE_OUT_DIR", "NORM_CLUSTERING_BASE_OUT_DIR", "NORM_CLUSTERING_SELECTED_MARKERS_OUT_DIR",
    "NORM_CLUSTERING_CELL_ANNOTATION_OUT_DIR", "NORM_CLUSTERING_DIMRED_PLOTS_OUT_DIR"
  )
  .create_dirs(cfg[dirs])
  invisible(NULL)
}

.create_integration_dirs <- function(cfg) {
  dirs <- c(
    "INTEGRATION_BASE_OUT_DIR", "INTEGRATION_SELECTED_MARKERS_OUT_DIR",
    "INT_CLUSTERING_BASE_OUT_DIR", "INT_CLUSTERING_CELL_ANNOTATION_OUT_DIR",
    "INT_CLUSTERING_DIMRED_PLOTS_OUT_DIR"
  )
  .create_dirs(cfg[dirs])
  invisible(NULL)
}

#' @title Create a basic directory structure based on paths in config.
#' @param cfg A `scdrake_list`. Obtained from [load_single_sample_configs()] or [load_integration_configs()].
#' @return Invisibly `NULL`.
#'
#' @concept create_dirs
#' @rdname create_dirs
#' @export
create_single_sample_dirs <- function(cfg) {
  .create_common_dirs(Reduce(c, cfg[c("main", "cluster_markers", "contrasts")]))
  .create_single_sample_dirs(Reduce(c, cfg[c("input_qc", "norm_clustering")]))
  invisible(NULL)
}

#' @rdname create_dirs
#' @export
create_integration_dirs <- function(cfg) {
  .create_common_dirs(Reduce(c, cfg[c("main", "cluster_markers", "contrasts")]))
  .create_integration_dirs(Reduce(c, cfg[c("integration", "int_clustering")]))
  invisible(NULL)
}

#' @title A shortcut to run the `scdrake` pipeline in the current R session.
#' @description This is a shortcut for:
#' - Loading configs: [load_pipeline_config()], and [load_single_sample_configs()] or [load_integration_configs()].
#' - Obtaining the pipeline plan: [get_single_sample_plan()] or [get_integration_plan()].
#' - Creating a basic directory structure (based on paths loaded from config files): [create_single_sample_dirs()] or
#'   [create_integration_dirs()].
#' - Running the pipeline: [scdrake_make()].
#'
#' Note that this is not a recommended way to run the pipeline. Instead, you should run [drake::r_make()] with
#' `source` parameter of `"_drake_single_sample.R"` or `"_drake_integration.R"`. See [scdrake_r_make()] for more details.
#' @inheritParams cfg_pipeline_param
#' @inheritParams pipeline_config_dir
#' @inheritParams single_sample_config_dir
#' @param do_update_configs A logical scalar: if `TRUE`, run [update_single_sample_configs()] or
#'   [update_integration_configs()] before loading configs.
#' @inheritParams verbose
#' @param .dry A logical scalar: if `TRUE`, omit the last step in the description and just return `TRUE`.
#' @param ... Passed to [scdrake_make()].
#' @return `TRUE` if pipeline finishes successfully.
#'
#' @concept run_pipeline
#' @rdname run_pipeline
#' @export
run_single_sample <- function(pipeline_config_dir = getOption("scdrake_pipeline_config_dir"),
                              single_sample_config_dir = getOption("scdrake_single_sample_config_dir"),
                              do_update_configs = TRUE,
                              verbose = getOption("scdrake_verbose"),
                              .dry = FALSE,
                              ...) {
  if (do_update_configs) {
    update_pipeline_config(pipeline_config_dir, verbose = verbose)
    update_single_sample_configs(single_sample_config_dir, verbose = verbose)
  }

  cfg_pipeline <- load_pipeline_config(pipeline_config_dir, verbose = verbose)
  cfg <- load_single_sample_configs(cfg_pipeline, dir = single_sample_config_dir, verbose = verbose)
  plan <- get_single_sample_plan(cfg, cfg_pipeline)

  create_single_sample_dirs(cfg)

  if (.dry) {
    return(TRUE)
  } else {
    scdrake_make(plan, cfg_pipeline = cfg_pipeline, cfg_main = cfg$main, verbose = verbose, ...)
  }
}

#' @inheritParams integration_config_dir
#'
#' @rdname run_pipeline
run_integration <- function(pipeline_config_dir = getOption("scdrake_pipeline_config_dir"),
                            integration_config_dir = getOption("scdrake_integration_config_dir"),
                            do_update_configs = TRUE,
                            verbose = getOption("scdrake_verbose"),
                            .dry = FALSE,
                            ...) {
  if (do_update_configs) {
    update_pipeline_config(pipeline_config_dir, verbose = verbose)
    update_integration_configs(integration_config_dir, verbose = verbose)
  }

  cfg_pipeline <- load_pipeline_config(pipeline_config_dir, verbose = verbose)
  cfg <- load_integration_configs(cfg_pipeline, dir = integration_config_dir, verbose = verbose)
  plan <- get_integration_plan(cfg, cfg_pipeline)

  create_integration_dirs(cfg)

  if (.dry) {
    return(TRUE)
  } else {
    scdrake_make(plan, cfg_pipeline = cfg_pipeline, cfg_main = cfg$main, verbose = verbose, ...)
  }
}
