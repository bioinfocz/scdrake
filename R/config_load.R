#' @param dir A character scalar:
#' - For `load_pipeline_config()`: a path to directory with `pipeline.yaml` file.
#' - For `load_single_sample_config()`: a path to directory with `00_main.yaml`,
#'   `01_input_qc.yaml`, `02_norm_clustering.yaml`,
#'   `cluster_markers.yaml`, and `contrasts.yaml` files.
#' - For `load_integration_config()`: a path to directory with `00_main.yaml`,
#'   `01_integration.yaml`, `02_int_clustering.yaml`,
#'   `cluster_markers.yaml`, and `contrasts.yaml` files.
#' @inheritParams cfg_pipeline_param
#' @param process A logical scalar: if `TRUE`, internally process the loaded config.
#'   This is always needed prior to sending the config to functions generating a `drake` plan,
#'   but in some cases could be useful to see the raw config.
#' @param ... Passed to `load_config()`.
#'
#' @details Except `load_pipeline_config()`, you can refer to variables from `pipeline.yaml` and `00_main.yaml` configs
#' inside individual configs. For example, you can use `CLUSTER_SC3_N_CORES: !code DRAKE_N_JOBS` in `02_norm_clustering.yaml`.
#'
#' @name load_config
NULL

#' @title Load a YAML config file as `scdrake_list`.
#' @description
#' `load_config()` loads a single YAML file as a [scdrake_list()].
#'
#' `load_pipeline_config()`, `load_single_samples_configs()`, and `load_integration_configs()`
#' load a specific config group (pipeline, single-sample, integration) and return a named list of `scdrake_list`s.
#' @param yaml_file A character scalar: path to YAML file.
#' @param other_variables A list of variables in whose environment the `yaml_file` will be evaluated, see details below.
#' @param eval_code A logical scalar: if `TRUE`, evaluate code for parameters in the `yaml_file` whose value starts with `!code `.
#' @inheritParams verbose1_param
#' @return A [scdrake_list()] of parameters loaded from the YAML file, or a named list of such lists.
#'
#' @section Evaluating code inside YAML files:
#' All parameters in the `yaml_file` whose value starts with `!code` will be evaluated as R code.
#' Those can reference other parameters in the `yaml_file`, or variables in the individual lists of the `other_variables` list.
#' Note that you cannot reference other `!code` parameters as their are not already evaluated during the runtime.
#'
#' An example YAML file:
#'
#' ```yaml
#' VAR_1: 5
#' VAR_2: !code VAR_1 + 5
#' VAR_3: "apple"
#' VAR_4:
#'   VAR_4_1: !code str_upper(VAR_3)
#' VAR_5:
#'   - VAR_5_1:
#'       a: !code VAR_1 + 1
#'     VAR_5_2:
#'       b: !code 1:10
#' VAR_6: !code VAR_2 + 10
#' ````
#'
#' - `VAR_2` will be `10`.
#' - `VAR_4_1` will be `"APPLE"`.
#' - `VAR_5_1` will be `6`.
#' - `VAR_5_2` will be a numeric vector.
#' - `VAR_6` will throw error, because `VAR_2` will still be unevaluated, so R will try to add `10` to the character scalar.
#'
#' Note that `VAR_5` contains a single named list, and this structure is used quite often in `scdrake` configs.
#' You have to keep an eye on the proper indentation :)
#'
#' @examples
#' # If a scdrake project is in the current working directory.
#' \dontrun{
#' cfg_1 <- load_config("config/pipeline.yaml")
#' cfg_2 <- load_config(
#'   "config/single_sample/00_main.yaml",
#'   other_variables = cfg_1
#' )
#' }
#'
#' @concept load_config
#' @rdname load_config
#' @export
load_config <- function(yaml_file, other_variables = NULL, eval_code = TRUE, verbose = getOption("scdrake_verbose")) {
  assert_that_(fs::file_exists(yaml_file), msg = "YAML file {.file {yaml_file}} does not exist.")
  verbose %&&% cli_alert_info("Loading config {.file {yaml_file}}")

  withr::with_options(list(yaml.eval.expr = TRUE), {
    cfg <- yaml::yaml.load_file(yaml_file, handlers = list(code = function(x) str_space("!code", x)))
  })

  yaml_env <- list2env(cfg, envir = new.env())

  if (!is_null(other_variables)) {
    assert_that_(is_list(other_variables), msg = "{.var other_variables} must be a list")
    yaml_env <- list2env(other_variables, envir = yaml_env)
  }

  ## -- Evaluate parameters with values starting with "!code".
  if (eval_code) {
    cfg <- rapply(cfg, how = "replace", f = function(val) {
      if (is_scalar_character(val)) {
        val_trim <- stringr::str_trim(val)
        if (stringr::str_starts(val_trim, stringr::fixed("!code "))) {
          code <- stringr::str_remove(val_trim, "^!code\\s")
          tryCatch(
            val <- eval(parse(text = code), envir = yaml_env),
            error = function(e) {
              cli_alert_danger("Error evaluating the following code:\n{.code {val}}")
              cli_alert_info("Error message:\n{e}")
              stop(e)
            }
          )
        }
      }

      return(val)
    })
  }

  return(scdrake_list(cfg))
}

#' @examples
#' \dontrun{
#' pipeline_config <- load_pipeline_config()
#' pipeline_config_other <- load_pipeline_config("some/other/dir/config.yaml")
#' }
#'
#' @rdname load_config
#' @export
load_pipeline_config <- function(dir = getOption("scdrake_pipeline_config_dir"),
                                 process = TRUE,
                                 ...) {
  yaml_file <- fs::path(dir, "pipeline.yaml")
  cfg <- load_config(yaml_file, ...)

  if (process) {
    cfg <- .process_pipeline_config(cfg)
  }

  return(cfg)
}

#' @title Load a list of YAML config files.
#' @param dir A path to directory with YAML config files.
#' @param cfg_defs A list of lists of length three.
#'   In each list, the first item is a name of YAML file in `dir`, the second one is a name in the resulting named list,
#'   and the third one is a processing function.
#' @param other_variables Passed to `load_config()`.
#' @param process A logical scalar: if `TRUE`, apply a processing function on its associated config list.
#' @param ... Currently not used.
#' @return A named list of lists of loaded YAML configs.
#'
#' @concept internal
.load_configs <- function(dir, cfg_defs, other_variables = NULL, process = TRUE, ...) {
  lapply(cfg_defs, function(cfg_def) {
    cfg <- load_config(fs::path(dir, cfg_def[[1]]), other_variables = other_variables)
    if (process) {
      cfg <- cfg_def[[3]](cfg, other_variables = other_variables)
    }
    return(cfg)
  }) %>%
    set_names(purrr::map_chr(cfg_defs, 2))
}

#' @title Load a group of YAML config files.
#' @param dir A character scalar: path to directory with YAML config files.
#' @param cfg_pipeline One of:
#' - A `scdrake_list` object: pipeline config (see [load_pipeline_config()]) obtained from `pipeline.yaml` file located
#'   in a pipeline config directory
#' - `NULL`: the config will be loaded using the path defined in the `scdrake_pipeline_config_dir` option
#' - A scalar character or `fs_path` object: the config is loaded from this path
#' @inheritParams cfg_pipeline_param
#' @inheritParams .load_configs
#' @param ... Passed to [load_pipeline_config()], [load_config()], and [.load_configs()].
#' @return A named `scdrake_list` of `scdrake_list`s for each entry in `cfg_defs` list.
#'
#' @concept internal
.load_config_group <- function(dir, cfg_defs, cfg_pipeline = NULL, process = TRUE, ...) {
  if (!is(cfg_pipeline, "scdrake_list")) {
    if (is_null(cfg_pipeline)) {
      cfg_pipeline_dir <- getOption("scdrake_pipeline_config_dir")
    } else if (is_scalar_character(cfg_pipeline) || is(cfg_pipeline, "fs_path")) {
      cfg_pipeline_dir <- cfg_pipeline
    } else {
      cli_abort("{.var cfg_pipeline} must be {.code NULL} or scalar character or scalar {.var fs_path} object")
    }

    cfg_pipeline <- load_pipeline_config(dir = cfg_pipeline_dir, process = process, ...)
  }

  cfg_main <- load_config(fs::path(dir, "00_main.yaml"), ...)

  if (process) {
    cfg_main <- .process_main_config(cfg_main)
  }

  other_variables <- c(Reduce(c, cfg_main), cfg_pipeline) %>% scdrake_list()

  cfg_others <- .load_configs(dir, cfg_defs, other_variables = other_variables, process = process, ...)
  cfgs <- c(cfg_main, cfg_others)

  return(scdrake_list(cfgs))
}

#' @examples
#' \dontrun{
#' single_sample_config <- load_single_sample_configs()
#' single_sample_config_other <- load_single_sample_configs("some/other/dir")
#' }
#'
#' @rdname load_config
#' @export
load_single_sample_configs <- function(dir = getOption("scdrake_single_sample_config_dir"),
                                       cfg_pipeline = NULL,
                                       process = TRUE,
                                       ...) {
  cfg_defs <- list(
    list("01_input_qc.yaml", "input_qc", .process_input_qc_config),
    list("02_norm_clustering.yaml", "norm_clustering", .process_norm_clustering_config),
    list("cluster_markers.yaml", "cluster_markers", .process_cluster_markers_config),
    list("contrasts.yaml", "contrasts", .process_contrasts_config)
  )

  .load_config_group(dir = dir, cfg_defs = cfg_defs, cfg_pipeline = cfg_pipeline, process = process, ...)
}

#' @examples
#' \dontrun{
#' integration_config <- load_integration_configs()
#' single_sample_config_other <- load_integration_configs("some/other/dir")
#' }
#'
#' @rdname load_config
#' @export
load_integration_configs <- function(dir = getOption("scdrake_integration_config_dir"),
                                     cfg_pipeline = NULL,
                                     process = TRUE,
                                     ...) {
  cfg_defs <- list(
    list("01_integration.yaml", "integration", .process_integration_config),
    list("02_int_clustering.yaml", "int_clustering", .process_int_clustering_config),
    list("cluster_markers.yaml", "cluster_markers", .process_cluster_markers_config),
    list("contrasts.yaml", "contrasts", .process_contrasts_config)
  )

  .load_config_group(dir = dir, cfg_defs = cfg_defs, cfg_pipeline = cfg_pipeline, process = process, ...)
}
