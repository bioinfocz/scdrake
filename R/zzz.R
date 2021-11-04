#' @title Options used by `scdrake`.
#' @description `get_scdrake_default_options()` returns a list of default `scdrake` options passed to
#' `options()` during the package load.
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
#'
#' Some of the options are internal, used for unit tests:
#'
#' - `scdrake_test_download_yq`
#'   (logical, env: `SCDRAKE_TEST_DOWNLOAD_YQ`, default: `FALSE`)
#'   - If `TRUE`, run tests for `yq` tool (`test-yq.R`).
#' - `scdrake_test_run_pipeline`
#'   (logical, env: `SCDRAKE_TEST_RUN_PIPELINE`, default: `FALSE`)
#'   - If `TRUE`, run pipeline tests (`test-run_pipeline.R`).
#'     Note that configs will be slightly adjusted compared to the default ones to speedup the computations.
#' - `scdrake_test_run_pipeline_base_out_dir`
#'   (character, env: `SCDRAKE_TEST_RUN_PIPELINE_BASE_OUT_DIR`,
#'   default: `fs::file_temp("scdrake_test_project_") %>% fs::path_abs() %>% as.character()`):
#'   - Base output directory for pipeline tests.
#' - `scdrake_test_run_pipeline_single_sample_full`
#'   (logical, env: `SCDRAKE_TEST_RUN_PIPELINE_SINGLE_SAMPLE_FULL`, default: `FALSE`)
#' - `scdrake_test_run_pipeline_integration`
#'   - If `TRUE`, run all targets of the single-sample pipeline for PBMC 1k dataset.
#'   (logical, env: `SCDRAKE_TEST_RUN_PIPELINE_INTEGRATION`, default: `FALSE`):
#'   - If `TRUE`, run a shortened single-sample pipeline (target `sce_final_norm_clustering`)
#'     for a second sample (PBMC 3k), followed by the integration pipeline.
#' - `scdrake_test_run_pipeline_keep_files`
#'   (logical, env: `SCDRAKE_TEST_RUN_PIPELINE_KEEP_FILES`, default: `TRUE`):
#'   - If `TRUE`, keep files created by pipeline tests.
#'     This is useful for reporting output of the current `scdrake` version.
#'
#' @return A named list of options.
#'
#' @concept scdrake_options
#' @rdname scdrake_options
#' @export
get_scdrake_default_options <- function() {
  withr::local_options(scdrake_verbose = TRUE)

  list(
    scdrake_yq_binary = Sys.which("yq"),
    scdrake_verbose = get_sys_env("SCDRAKE_VERBOSE", default = TRUE, type = "logical"),
    scdrake_cache_dir = get_sys_env("SCDRAKE_CACHE_DIR", default = ".drake"),
    scdrake_pipeline_config_dir = get_sys_env(
      "SCDRAKE_PIPELINE_CONFIG_DIR", default = "config"
    ),
    scdrake_single_sample_config_dir = get_sys_env(
      "SCDRAKE_SINGLE_SAMPLE_CONFIG_DIR", default = "config/single_sample"
    ),
    scdrake_integration_config_dir = get_sys_env(
      "SCDRAKE_INTEGRATION_CONFIG_DIR", default = "config/integration"
    ),

    ## -- Internal options, mostly for unit tests.
    scdrake_test_download_yq = get_sys_env(
      "SCDRAKE_TEST_DOWNLOAD_YQ", default = FALSE, type = "logical", verbose = FALSE
    ),
    scdrake_test_run_pipeline = get_sys_env(
      "SCDRAKE_TEST_RUN_PIPELINE", default = FALSE, type = "logical", verbose = FALSE
    ),
    scdrake_test_run_pipeline_base_out_dir = get_sys_env(
      "SCDRAKE_TEST_RUN_PIPELINE_BASE_OUT_DIR",
      default = fs::file_temp("scdrake_test_run_pipeline_output") %>% fs::path_abs() %>% as.character(),
      type = "character",
      verbose = FALSE
    ),
    scdrake_test_run_pipeline_single_sample_full = get_sys_env(
      "SCDRAKE_TEST_RUN_PIPELINE_SINGLE_SAMPLE_FULL",
      default = FALSE,
      type = "logical",
      verbose = FALSE
    ),
    scdrake_test_run_pipeline_integration = get_sys_env(
      "SCDRAKE_TEST_RUN_PIPELINE_INTEGRATION", default = FALSE, type = "logical", verbose = FALSE
    ),
    scdrake_test_run_pipeline_keep_files = get_sys_env(
      "SCDRAKE_TEST_RUN_PIPELINE_KEEP_FILES", default = TRUE, type = "logical", verbose = FALSE
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

.onLoad <- function(libname, pkgname) {
  options(get_scdrake_default_options())
  set_rstudio_drake_cache(getOption("scdrake_cache_dir"))
  Sys.setenv(`_R_CHECK_LENGTH_1_CONDITION_` = "true")
  check_qs_installed()
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(cli(cli::cli_h1("Welcome to {.pkg scdrake}!")))
}

##-- We need this to fix R CMD CHECK notes on "no visible binding for global variable" caused by functions generating
##-- a drake plan.
globalVariables(c(
  ##-- drake DSL functions
  c("map", "group", "cross"),

  ##-- Common plans.
  codetools::findGlobals(get_common_subplan, merge = FALSE)$variables,
  codetools::findGlobals(get_cluster_markers_subplan, merge = FALSE)$variables,
  codetools::findGlobals(get_contrasts_subplan, merge = FALSE)$variables,

  ##-- Single-sample plans.
  codetools::findGlobals(get_input_qc_subplan, merge = FALSE)$variables,
  codetools::findGlobals(get_norm_clustering_subplan, merge = FALSE)$variables,

  ##-- Integration plans.
  codetools::findGlobals(get_integration_subplan, merge = FALSE)$variables,
  codetools::findGlobals(get_int_clustering_subplan, merge = FALSE)$variables
))
