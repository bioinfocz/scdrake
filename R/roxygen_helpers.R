## -- Just some Roxygen documentation helpers.

## -- A common verbose parameter for functions using options for the default value.
#' @param verbose A logical scalar: if `TRUE`, be verbose.
#'   The default value is obtained from `getOption("scdrake_verbose")`.
#' @name verbose1_param
NULL

#' @param verbose A logical scalar: if `TRUE`, be verbose.
#' @name verbose2_param
NULL

#' @param cfg_pipeline A `scdrake_list` object: pipeline config (see [load_pipeline_config()])
#'   obtained from `pipeline.yaml` file located in pipeline config directory.
#'   If `NULL`, it will be loaded using the path defined in `scdrake_pipeline_config_dir` option.
#' @name cfg_pipeline_param
NULL

#' @param pipeline_config_dir A character scalar: path to directory with pipeline config file (`pipeline.yaml`).
#' @name pipeline_config_dir_param
NULL

#' @param single_sample_config_dir A character scalar: path to directory with `00_main.yaml`, `01_input_qc.yaml`,
#'   `02_norm_clustering.yaml`, `cluster_markers.yaml`, and `contrasts.yaml` files.
#' @name single_sample_config_dir_param
NULL

#' @param integration_config_dir A character scalar: path to directory with `00_main.yaml`, `01_integration.yaml`,
#'   `02_int_clustering.yaml`, `cluster_markers.yaml`, and `contrasts.yaml` files.
#' @name integration_config_dir_param
NULL

#' @param BSPARAM A [BiocSingular::BiocSingularParam-class] object.
#' @name bsparam_param
NULL

#' @param BPPARAM A [BiocParallel::BiocParallelParam-class] object.
#' @name bpparam_param
NULL
