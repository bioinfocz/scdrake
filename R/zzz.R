#' @title Options used by `scdrake`.
#' @description `get_scdrake_default_options()` returns a list of default `scdrake` options passed to
#' `options()` during the package load.
#' @inheritParams verbose_
#'
#' @details
#' Most of the options are obtained from environment variables named in UPPERCASE,
#' e.g. the value of `scdrake_verbose` is taken from `SCDRAKE_VERBOSE`.
#' The environment variables are coerced to atomic types (character, logical, integer, double).
#'
#' The following options are used by `scdrake`:
#'
#' - `scdrake_yq_binary`
#'   (character, default: `Sys.which("yq")`)
#'   - A path to `yq` tool's binary.
#'   - If the default value is `""`, it will be set to `get_yq_default_path()`.
#' - `scdrake_verbose`
#'   (logical, env: `SCDRAKE_VERBOSE`, default: `TRUE`)
#'   - If `TRUE`, `scdrake` will be verbose.
#' - `scdrake_cache_dir`
#'   (character, env: `SCDRAKE_CACHE_DIR`, default: `".drake"`)
#'   - A path to `drake` cache directory.
#'     The `drake` package also contains an RStudio addin, which has a shortcut for loading a target under cursor
#'     (see `Tools -> Modify Keyboard Shortcuts -> "loadd target under cursor"`).
#'     To know which cache directory to use for loading, `drake` is using the `rstudio_drake_cache` option.
#'     On its load or attach, `scdrake` will automatically set this option to `scdrake_cache_dir`.
#'     Just keep in mind that `rstudio_drake_cache` option will be overwritten if you load the `drake` package.
#' - `scdrake_pipeline_config_dir`
#'   (character, env: `SCDRAKE_PIPELINE_CONFIG_DIR`, default: `"config"`)
#'   - A path to directory containing `pipeline.default.yaml` and/or `pipeline.yaml` files.
#' - `scdrake_single_sample_config_dir`
#'   (character, env: `SCDRAKE_SINGLE_SAMPLE_CONFIG_DIR`, default: `"config/single_sample"`)
#'   - A path to directory containing files for single-sample analysis,
#'     see the `dir` parameter in [load_single_sample_configs()].
#' - `scdrake_integration_config_dir`
#'   (character, env: `SCDRAKE_INTEGRATION_CONFIG_DIR`, default: `"config/integration"`)
#'   - A path to directory containing files for integration analysis,
#'     see the `dir` parameter in [load_integration_configs()].
#' - `scdrake_plan_custom_file`
#'   (character, env: `SCDRAKE_PLAN_CUSTOM_FILE`, default: `"plan_custom.R"`)
#'   - A path to file with custom `drake` plan.
#' - `scdrake_project_root`
#'   (character, env: `SCDRAKE_PROJECT_ROOT`, default: `.`)
#'   - A path to `scdrake` project root. If different from the current working directory (`.`), `scdrake` will first
#'     change the working directory before pipeline is run in e.g. `run_single_sample_r()` or `run_single_sample()`.
#'     That means other path-specifying options (e.g. `scdrake_pipeline_config_dir`) and config parameters will become
#'     relative to `scdrake_project_root` - **use at your own risk**!
#'
#' @return A named list of options.
#'
#' @concept scdrake_options
#' @rdname scdrake_options
#' @export
get_scdrake_default_options <- function(verbose = TRUE) {
  withr::local_options(scdrake_verbose = verbose)
  scdrake_yq_binary <- Sys.which("yq")
  scdrake_yq_binary <- dplyr::if_else(scdrake_yq_binary == "", get_yq_default_path(), scdrake_yq_binary)

  list(
    scdrake_yq_binary = scdrake_yq_binary,
    scdrake_verbose = get_sys_env("SCDRAKE_VERBOSE", default = TRUE, type = "logical"),
    scdrake_cache_dir = get_sys_env("SCDRAKE_CACHE_DIR", default = ".drake"),
    scdrake_pipeline_config_dir = get_sys_env(
      "SCDRAKE_PIPELINE_CONFIG_DIR",
      default = "config"
    ),
    scdrake_single_sample_config_dir = get_sys_env(
      "SCDRAKE_SINGLE_SAMPLE_CONFIG_DIR",
      default = "config/single_sample"
    ),
    scdrake_integration_config_dir = get_sys_env(
      "SCDRAKE_INTEGRATION_CONFIG_DIR",
      default = "config/integration"
    ),
    scdrake_plan_custom_file = get_sys_env(
      "SCDRAKE_PLAN_CUSTOM_FILE",
      default = "plan_custom.R"
    ),
    scdrake_project_root = get_sys_env(
      "SCDRAKE_PROJECT_ROOT",
      default = "."
    )
  )
}

#' @description `get_scdrake_options()` returns a list of options currently used by `scdrake`
#' (their names begin with `"scdrake_"`).
#'
#' @concept scdrake_options
#' @rdname scdrake_options
#' @export
get_scdrake_options <- function() {
  options()[stringr::str_detect(names(options()), "^scdrake_")]
}

.get_verbosity <- function() {
  Sys.getenv("SCDRAKE_VERBOSE") %in% c("", "TRUE")
}

.onLoad <- function(libname, pkgname) {
  options(get_scdrake_default_options(verbose = .get_verbosity()))
  set_rstudio_drake_cache(getOption("scdrake_cache_dir"), verbose = FALSE)
  Sys.setenv(`_R_CHECK_LENGTH_1_CONDITION_` = "true")
}

.onAttach <- function(libname, pkgname) {
  if (.get_verbosity()) {
    packageStartupMessage(cli(cli::cli_h1("Welcome to {.pkg scdrake}!")))
  }
}

## -- We need this to fix R CMD CHECK notes on "no visible binding for global variable" caused by functions generating
## -- a drake plan.
utils::globalVariables(c(
  ## -- drake DSL functions
  c("map", "group", "cross"),

  ## -- Common plans.
  codetools::findGlobals(get_common_subplan, merge = FALSE)$variables,
  codetools::findGlobals(get_cluster_markers_subplan, merge = FALSE)$variables,
  codetools::findGlobals(get_contrasts_subplan, merge = FALSE)$variables,

  ## -- Single-sample plans.
  codetools::findGlobals(get_input_qc_subplan, merge = FALSE)$variables,
  codetools::findGlobals(get_norm_clustering_subplan, merge = FALSE)$variables,

  ## -- Integration plans.
  codetools::findGlobals(get_integration_subplan, merge = FALSE)$variables,
  codetools::findGlobals(get_int_clustering_subplan, merge = FALSE)$variables
))
