#' @title Get a `drake` plan for a specific analysis type.
#' @description These functions are internally loading and binding a smaller plans (subplans) into full plans.
#' All of them are using [get_common_subplan()], [get_cluster_markers_subplan()], and [get_contrasts_subplan()].
#'
#' [get_single_sample_plan()] uses [get_input_qc_subplan()] and [get_norm_clustering_subplan()].
#'
#' [get_integration_plan()] uses [get_integration_subplan()] and [get_int_clustering_subplan()].
#'
#' @param cfg
#' - For `get_single_sample_plan()`: A list of config lists returned by [load_single_sample_configs()].
#' - For `get_integration_plan()`: A list of config lists returned by [load_integration_configs()].
#' @inheritParams cfg_pipeline_param
#' @return [drake::drake_plan()]
#'
#' @name get_plan
NULL

#' @rdname get_plan
#' @export
get_single_sample_plan <- function(cfg, cfg_pipeline = NULL) {
  if (is_null(cfg_pipeline)) {
    cfg_pipeline <- load_pipeline_config()
  }

  common_plan <- get_common_subplan(cfg_pipeline, cfg$main, pipeline_type_ = "single_sample")
  input_qc_plan <- get_input_qc_subplan(cfg$input_qc, cfg_pipeline, cfg$main)
  norm_clustering_plan <- get_norm_clustering_subplan(cfg$norm_clustering, cfg_pipeline, cfg$main)
  cluster_markers_plan <- get_cluster_markers_subplan(cfg$cluster_markers, cfg_pipeline, cfg$main)
  contrasts_plan <- get_contrasts_subplan(cfg$contrasts, cfg_pipeline, cfg$main)
  drake::bind_plans(common_plan, input_qc_plan, norm_clustering_plan, cluster_markers_plan, contrasts_plan)
}

#' @rdname get_plan
#' @export
get_integration_plan <- function(cfg, cfg_pipeline = NULL) {
  if (is_null(cfg_pipeline)) {
    cfg_pipeline <- load_pipeline_config()
  }

  common_plan <- get_common_subplan(cfg_pipeline, cfg$main, pipeline_type_ = "integration")
  integration_plan <- get_integration_subplan(cfg$integration, cfg_pipeline, cfg$main)
  int_clustering_plan <- get_int_clustering_subplan(cfg$int_clustering, cfg_pipeline, cfg$main)
  cluster_markers_plan <- get_cluster_markers_subplan(cfg$cluster_markers, cfg_pipeline, cfg$main)
  contrasts_plan <- get_contrasts_subplan(cfg$contrasts, cfg_pipeline, cfg$main)
  drake::bind_plans(common_plan, integration_plan, int_clustering_plan, cluster_markers_plan, contrasts_plan)
}
