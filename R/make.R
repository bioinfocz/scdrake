#' @title Execute a `scdrake`'s pipeline plan.
#' @description This is a wrapper around [drake::make()] with some sensible defaults.
#' Before a plan is executed, this function sets *locally* some options, environment variables and other things.
#' That means no side effects are left. Note that this is a less reproducible way to run
#' the `scdrake` pipeline since it is executed in the current R session.
#' Also note that [create_single_sample_dirs()] or [create_integration_dirs()] must be run before.
#' @param plan A `drake` plan.
#' @inheritParams cfg_pipeline_param
#' @param cfg_main A `scdrake_list` object: main config (see [load_config])
#'   obtained from `00_main.yaml` file located in single-sample or integration config directory.
#' @inheritParams verbose
#' @param memory_strategy,caching,prework,log_build_times,log_worker,... Passed to [drake::make()]
#'   or [drake::r_make()]. `prework` must be a character scalar.
#' @param options A list: additional [base::options()] to be set (locally).
#' @return Invisibly `TRUE` if plan execution finishes without errors.
#'
#' @concept scdrake_make
#' @rdname scdrake_make
#' @export
scdrake_make <- function(plan,
                         cfg_pipeline = NULL,
                         cfg_main = NULL,
                         verbose = getOption("scdrake_verbose"),
                         memory_strategy = "speed",
                         caching = "worker",
                         prework = "",
                         log_build_times = FALSE,
                         log_worker = TRUE,
                         options = list(),
                         ...) {
  if (is_null(cfg_pipeline)) {
    cfg_pipeline <- load_pipeline_config()
  }

  withr::local_seed(cfg_pipeline$SEED)

  if (cfg_pipeline$DRAKE_PARALLELISM == "future") {
    future_plan_orig <- future::plan()
    withr::defer(future::plan(future_plan_orig))

    if (parallelly::supportsMulticore()) {
      future::plan(future::multicore, workers = cfg_pipeline$DRAKE_N_JOBS)
    } else {
      future::plan(future.callr::callr, workers = cfg_pipeline$DRAKE_N_JOBS)
    }
  }

  ## -- Within-target parallelism can be reliably used only if targets are processed sequentially.
  BPPARAM_ORIG <- BiocParallel::bpparam()
  withr::defer(BiocParallel::register(BPPARAM_ORIG))

  if (cfg_pipeline$DRAKE_PARALLELISM == "loop" && is_true(cfg_pipeline$WITHIN_TARGET_PARALLELISM)) {
    BPPARAM <- BiocParallel::SnowParam(workers = cfg_pipeline$N_CORES, type = "SOCK", RNGseed = cfg_pipeline$SEED)
    verbose %&&% cli_alert_info("Starting R parallel worker sessions (n = {cfg_pipeline$N_CORES})...")
    BiocParallel::bpstart(BPPARAM)
    verbose %&&% cli_alert_info("Done.")
  } else {
    BPPARAM <- BiocParallel::SerialParam()
  }

  BiocParallel::register(BPPARAM)

  ## -- Prevent issues with BLAS operations when a massive target parallelism is used.
  if (!is_null(cfg_pipeline$BLAS_N_THREADS)) {
    blas_threads_orig <- RhpcBLASctl::blas_get_num_procs()
    RhpcBLASctl::blas_set_num_threads(cfg_pipeline$BLAS_N_THREADS)
    withr::defer(RhpcBLASctl::blas_set_num_threads(blas_threads_orig))
  }

  # if (!is_null(cfg_pipeline$RSTUDIO_PANDOC) && get_sys_env("RSTUDIO_PANDOC", type = "character", verbose = FALSE) != "") {
  if (!is_null(cfg_pipeline$RSTUDIO_PANDOC)) {
    withr::local_envvar(RSTUDIO_PANDOC = cfg_pipeline$RSTUDIO_PANDOC)
  }

  ## -- To fix https://github.com/rstudio/rmarkdown/issues/1632
  prework <- str_line(
    'assignInNamespace("clean_tmpfiles", function() {invisible(NULL)}, ns = "rmarkdown")',
    as.character(prework)
  )

  drake_cache_object <- drake::new_cache(path = cfg_pipeline$DRAKE_CACHE_DIR)

  if (cfg_pipeline$DRAKE_UNLOCK_CACHE && !is_null(drake_cache_object)) {
    cli_alert_info("Unlocking {.pkg drake} cache in {.file {cfg_pipeline$DRAKE_CACHE_DIR}}")
    drake_cache_object$unlock()
  }

  withr::local_options(
    .new = options,
    future.globals.maxSize = 200000 * 1024^2,
    clustermq.scheduler = cfg_pipeline$DRAKE_CLUSTERMQ_SCHEDULER,
    yaml.eval.expr = TRUE,
    knitr.table.format = "html",
    ## -- To fix a bug with the latest RSQLite and AnnotationDbi packages.
    connectionObserver = NULL,
    rstudio_drake_cache = drake_cache_object
  )

  verbose %&&% cli::cli_h2("Running the single-sample pipeline")
  verbose %&&% cli({
    if (!is_null(cfg_main)) {
      cli_alert_info("BASE OUTPUT DIRECTORY: {cfg_main$BASE_OUT_DIR}")
    }

    if (is_null(cfg_pipeline$DRAKE_TARGETS)) {
      cli_alert_info("TARGETS: NULL (= all)")
    } else {
      cli_alert_info("TARGETS:")
      cli::cli_ul(cfg_pipeline$DRAKE_TARGETS)
    }

    cli_alert_info("OUTDATED TARGETS:")
    cli::cli_ul(drake::outdated(plan = plan, targets = cfg_pipeline$DRAKE_TARGETS, cache = drake_cache_object))
  })

  drake::make(
    plan,
    targets = cfg_pipeline$DRAKE_TARGETS,
    envir = getNamespace("scdrake"),
    verbose = cfg_pipeline$DRAKE_VERBOSITY,
    cache = drake_cache_object,
    parallelism = cfg_pipeline$DRAKE_PARALLELISM,
    jobs = cfg_pipeline$DRAKE_N_JOBS,
    jobs_preprocess = cfg_pipeline$DRAKE_N_JOBS_PREPROCESS,
    packages = c("HDF5Array", rev(.packages())),
    prework = prework,
    seed = cfg_pipeline$SEED,
    caching = caching,
    keep_going = cfg_pipeline$DRAKE_KEEP_GOING,
    memory_strategy = memory_strategy,
    lock_envir = cfg_pipeline$DRAKE_LOCK_ENVIR,
    log_build_times = log_build_times,
    format = cfg_pipeline$DRAKE_FORMAT,
    log_worker = log_worker,
    ...
  )

  return(invisible(TRUE))
}

#' @description The preferred way is calling the [scdrake_r_make()] function,
#' which is a wrapper around [drake::r_make()]
#' ([details](https://books.ropensci.org/drake/projects.html#safer-interactivity)).
#' It will start a fresh new R session, source `_drake.R` (or a similar file) and execute the plan.
#' The current `_drake.R`-like files copied when a new `scdrake` project is initialized are:
#' - `_drake_single_sample.R` for the single-sample pipeline
#' - `_drake_integration.R` for the integration pipeline
#'
#' Internally, `_drake_single_sample.R` and `_drake_integration.R` do almost the same as code in [scdrake_make()],
#' except there is no need for isolation of side effects. Also, by default, it relies on environment variables,
#' such as paths to config directories, e.g. `SCDRAKE_SINGLE_SAMPLE_CONFIG_DIR` whose value is used as the default for
#' option used in `load_single_sample_configs(dir = getOption("scdrake_single_sample_config_dir"))`.
#' @param drake_file A character scalar: path to `_drake.R` (default for `NULL`) or similar file
#' ([details](https://books.ropensci.org/drake/projects.html#safer-interactivity)).
#'
#' @rdname scdrake_make
#' @export
scdrake_r_make <- function(drake_file = NULL, ...) {
  drake::r_make(drake_file, ...)
  return(invisible(TRUE))
}
