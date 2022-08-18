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

#' @title Source a file returning a custom [drake] plan.
#' @description This function is used in [drake]s init scripts `_drake_single_sample.R` and `_drake_integration.R`.
#' It sources an R script (`plan_custom.R` by default) which must return a [drake::drake_plan()] object.
#' In the R script, all variables defined in the init script are available, mainly `cfg` and `cfg_pipeline` lists
#' holding pipeline parameters. At the same time, all those variables are locked and cannot be modified in `file` script.
#'
#' @param file A character scalar: path to R script returning a [drake::drake_plan()].
#' @param envir An environment in which the R script will be sourced (defaults to caller env).
#' @return A [drake::drake_plan()] object (`tibble`).
#'
#' @export
load_custom_plan <- function(file = getOption("scdrake_plan_custom_file"), envir = parent.frame()) {
  if (!fs::file_exists(file)) {
    cli_alert_warning("The script file {.file {file}} with custom plan not found.")
    return(NULL)
  }

  ## -- To protect the caller env from modifications.
  purrr::map(ls(envir = envir), ~ lockBinding(., envir))
  res <- source(file, local = envir)
  purrr::map(ls(envir = envir), ~ unlockBinding(., envir))
  val <- res$value
  if (!is_null(val)) {
    assert_that_(
      inherits(val, "data.frame"),
      msg = "The sourced script {.file {file}} didn't return a {.code data.frame} object."
    )
    assert_that_(
      all(c("target", "command") %in% colnames(val)),
      msg = "The sourced script {.file {file}} didn't return a {.code data.frame} with {.field 'target'} and {.field 'command'} columns."
    )
  }

  return(val)
}
