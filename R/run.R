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
    "NORM_CLUSTERING_CELL_ANNOTATION_OUT_DIR", "NORM_CLUSTERING_DIMRED_PLOTS_OUT_DIR", "NORM_CLUSTERING_OTHER_PLOTS_OUT_DIR"
  )
  .create_dirs(cfg[dirs])
  invisible(NULL)
}

.create_integration_dirs <- function(cfg) {
  dirs <- c(
    "INTEGRATION_BASE_OUT_DIR", "INTEGRATION_SELECTED_MARKERS_OUT_DIR",
    "INT_CLUSTERING_BASE_OUT_DIR", "INT_CLUSTERING_CELL_ANNOTATION_OUT_DIR",
    "INT_CLUSTERING_DIMRED_PLOTS_OUT_DIR", "INT_CLUSTERING_OTHER_PLOTS_OUT_DIR"
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

#' @title Run the `scdrake` pipeline.
#' @description There are two ways how to run `scdrake` pipeline:
#'
#' The first, reproducible way is via [run_single_sample_r()] or [run_integration_r()]. In the end, [drake::r_make()]
#' is called and sources `"_drake_single_sample.R"` or `"_drake_integration.R"` scripts in a fresh new R session.
#' Those scripts are entry points for `drake`. See [scdrake_r_make()] for more details.
#'
#' (Soft-deprecated since `scdrake` 1.4.0)
#' The second way is to run pipeline in the current R session via [run_single_sample()] or [run_integration()],
#' which is a shortcut for:
#'
#' - Loading configs: [load_pipeline_config()], and [load_single_sample_configs()] or [load_integration_configs()].
#' - Obtaining the pipeline plan: [get_single_sample_plan()] or [get_integration_plan()].
#' - Creating a basic directory structure (based on paths loaded from config files): [create_single_sample_dirs()] or
#'   [create_integration_dirs()].
#' - Running the pipeline: [scdrake_make()].
#'
#' This way is considered as nonreproducible, because the pipeline is run in the current session and there might be
#' interfering variables or loaded packages. Internally, the [run_single_sample()] and [run_integration()] functions
#' are basically replicating the code that is in `"_drake_single_sample.R"` and `"_drake_integration.R"` scripts,
#' respectively, while protecting from side effects (e.g. setting `options()` etc.) on your current session
#' (i.e. all environment modifications are limited to function's scope).
#'
#' @inheritParams cfg_pipeline_param
#' @inheritParams pipeline_config_dir_param
#' @inheritParams single_sample_config_dir_param
#' @param project_root A character scalar: path to directory in which the pipeline will be run,
#'   i.e. a new working directory. The working directory will be used only temporarily. Default value of the
#'   `scdrake_project_root` options is the current working directory.
#' @param do_update_configs A logical scalar: if `TRUE`, run [update_single_sample_configs()] or
#'   [update_integration_configs()] before loading configs.
#' @inheritParams verbose1_param
#' @param .dry A logical scalar: if `TRUE`, omit the last step in the description and just return `TRUE`.
#' @param ... Passed to [scdrake_make()] or [scdrake_r_make()].
#' @return `TRUE` if pipeline finishes successfully.
#'
#' @concept run_pipeline
#' @rdname run_pipeline
#' @export
run_single_sample <- function(pipeline_config_dir = getOption("scdrake_pipeline_config_dir"),
                              single_sample_config_dir = getOption("scdrake_single_sample_config_dir"),
                              project_root = getOption("scdrake_project_root"),
                              do_update_configs = TRUE,
                              verbose = getOption("scdrake_verbose"),
                              .dry = FALSE,
                              ...) {
  run_pipeline(
    selected_pipeline_config_dir = single_sample_config_dir,
    pipeline_type = "single_sample",
    pipeline_config_dir = pipeline_config_dir,
    project_root = project_root,
    do_update_configs = do_update_configs,
    verbose = verbose,
    .dry = .dry,
    ...
  )
}

#' @inheritParams integration_config_dir_param
#'
#' @rdname run_pipeline
#' @export
run_integration <- function(pipeline_config_dir = getOption("scdrake_pipeline_config_dir"),
                            integration_config_dir = getOption("scdrake_integration_config_dir"),
                            project_root = getOption("scdrake_project_root"),
                            do_update_configs = TRUE,
                            verbose = getOption("scdrake_verbose"),
                            .dry = FALSE,
                            ...) {
  run_pipeline(
    selected_pipeline_config_dir = integration_config_dir,
    pipeline_type = "integration",
    pipeline_config_dir = pipeline_config_dir,
    project_root = project_root,
    do_update_configs = do_update_configs,
    verbose = verbose,
    .dry = .dry,
    ...
  )
}

run_pipeline <- function(selected_pipeline_config_dir,
                         pipeline_type = c("single_sample", "integration"),
                         pipeline_config_dir = getOption("scdrake_pipeline_config_dir"),
                         project_root = getOption("scdrake_project_root"),
                         do_update_configs = TRUE,
                         verbose = getOption("scdrake_verbose"),
                         .dry = FALSE,
                         ...) {
  cli_alert_warning(str_line(
    "{.code run_single_sample()} and {.code run_integration()} are soft-deprecated since {.pkg scdrake} 1.4.0. ",
    "Use, please, {.code run_single_sample_r()} and {.code run_integration_r()} instead."
  ))

  pipeline_type <- arg_match(pipeline_type)

  ## -- Temporarily change the working directory if different from project_root
  if (getwd() != fs::path_abs(project_root)) {
    cli_alert_info("Setting temporarily the working directory to {.file {project_root}}")
    orig_wd <- getwd()
    dir_rel <- fs::path_rel(orig_wd, start = project_root)
    setwd(project_root)
    here::i_am(".here")

    ## -- This will be evaluated on function's exit.
    withr::defer({
      setwd(orig_wd)
      here_exists <- fs::file_exists(".here")

      if (!here_exists) {
        fs::file_touch(".here")
      }
      here::i_am(".here")
      if (!here_exists) {
        fs::file_delete(".here")
      }
    })
  }

  if (do_update_configs) {
    update_pipeline_config(pipeline_config_dir, verbose = verbose)
    if (pipeline_type == "single_sample") {
      update_single_sample_configs(selected_pipeline_config_dir, verbose = verbose)
    } else {
      update_integration_configs(selected_pipeline_config_dir, verbose = verbose)
    }
  }

  cfg_pipeline <- load_pipeline_config(pipeline_config_dir, verbose = verbose)
  if (pipeline_type == "single_sample") {
    cfg <- load_single_sample_configs(cfg_pipeline, dir = selected_pipeline_config_dir, verbose = verbose)
    plan <- get_single_sample_plan(cfg, cfg_pipeline)
    create_single_sample_dirs(cfg)
  } else {
    cfg <- load_integration_configs(cfg_pipeline, dir = selected_pipeline_config_dir, verbose = verbose)
    plan <- get_integration_plan(cfg, cfg_pipeline)
    create_integration_dirs(cfg)
  }

  if (.dry) {
    return(TRUE)
  } else {
    cli_alert_info("Running the {pipeline_type} pipeline.")
    scdrake_make(plan, cfg_pipeline = cfg_pipeline, cfg_main = cfg$main, verbose = verbose, ...)
  }
}

run_pipeline_r <- function(drake_file, project_root = getOption("scdrake_project_root"), ...) {
  ## -- Temporarily change the working directory if different from project_root
  if (getwd() != fs::path_abs(project_root)) {
    cli_alert_info("Setting temporarily the working directory to {.file {project_root}}")
    withr::with_dir(
      project_root,
      scdrake_r_make(drake_file = drake_file, ...)
    )
  } else {
    scdrake_r_make(drake_file = drake_file, ...)
  }
}

#' @param drake_file A character scalar: path to entry script for [drake].
#'
#' @rdname run_pipeline
#' @export
run_single_sample_r <- function(drake_file = "_drake_single_sample.R",
                                project_root = getOption("scdrake_project_root"),
                                ...) {
  run_pipeline_r(drake_file, project_root = project_root, ...)
}

#' @rdname run_pipeline
#' @export
run_integration_r <- function(drake_file = "_drake_integration.R",
                              project_root = getOption("scdrake_project_root"),
                              ...) {
  run_pipeline_r(drake_file = drake_file, project_root = project_root, ...)
}
