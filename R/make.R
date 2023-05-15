#' @title Execute a `scdrake`'s pipeline plan.
#' @description This is a wrapper around [drake::make()] with some sensible defaults.
#' Before a plan is executed, this function sets *locally* some options, environment variables and other things.
#' That means no side effects are left. Note that this is a less reproducible way to run
#' the `scdrake` pipeline since it is executed in the current R session.
#' Also note that [create_single_sample_dirs()] or [create_integration_dirs()] must be run before.
#' @param plan A `drake` plan.
#' @inheritParams cfg_pipeline_param2
#' @param cfg_main A `scdrake_list` object: main config (see [load_config])
#'   obtained from `00_main.yaml` file located in single-sample or integration config directory.
#' @inheritParams verbose1_param
#' @param prework,log_worker,... Passed to [drake::make()]. `prework` must be a character scalar.
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
                         prework = "",
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
      cli_alert_info("Using {.code future::multicore} with {cfg_pipeline$DRAKE_N_JOBS} workers.")
    } else {
      future::plan(future.callr::callr, workers = cfg_pipeline$DRAKE_N_JOBS)
      cli_alert_info("Using {.code future.callr::callr} (multisession) with {cfg_pipeline$DRAKE_N_JOBS} workers.")
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

  drake_rebuild <- cfg_pipeline$DRAKE_REBUILD
  drake_trigger <- drake::trigger()

  if (is_scalar_character(drake_rebuild)) {
    ## -- NULL means all targets, so rebuild them all.
    if (is_null(cfg_pipeline$DRAKE_TARGETS)) {
      drake_rebuild <- "all"
    }

    if (drake_rebuild == "all") {
      str_space(
        "{.field DRAKE_REBUILD} is {.val 'all'} ->",
        "the pipeline will be run from scratch."
      ) %>% cli_alert_info()
      drake_trigger <- drake::trigger(condition = TRUE)
    } else if (drake_rebuild == "current") {
      str_space(
        "{.field DRAKE_REBUILD} is {.val 'current'} ->",
        "calling {.code drake::clean({dput(cfg_pipeline$DRAKE_TARGETS)}, path = {DRAKE_CACHE_DIR})}"
      ) %>%
        cli_alert_info()
      drake::clean(list = cfg_pipeline$DRAKE_TARGETS, path = cfg_pipeline$DRAKE_CACHE_DIR)
    } else {
      cli_alert_warning("Unknown value for {.field DRAKE_REBUILD}: {.val {drake_rebuild}}")
    }
  }

  withr::local_options(
    .new = options,
    future.globals.maxSize = 200000 * 1024^2,
    clustermq.scheduler = cfg_pipeline$DRAKE_CLUSTERMQ_SCHEDULER,
    yaml.eval.expr = TRUE,
    knitr.table.format = "html",
    ## -- To fix a bug with the latest RSQLite and AnnotationDbi packages.
    connectionObserver = NULL,
    rstudio_drake_cache = drake_cache_object,
    DT.warn.size = FALSE
  )

  check_scdrake()

  packages <- c("HDF5Array", "ensembldb", rev(.packages()))

  cli_alert_info("WORKING DIRECTORY: {.file {getwd()}}")
  if (!is_null(cfg_main)) {
    cli_alert_info("BASE OUTPUT DIRECTORY: {.file {cfg_main$BASE_OUT_DIR}}")
  }

  if (is_null(cfg_pipeline$DRAKE_TARGETS)) {
    cli_alert_info("TARGETS: NULL (= all)")
  } else {
    cli_alert_info("TARGETS:")
    cli::cli_ul(cfg_pipeline$DRAKE_TARGETS)
  }

  cli_alert_info("OUTDATED TARGETS:")
  cli::cli_ul(drake::outdated(plan = plan, targets = cfg_pipeline$DRAKE_TARGETS, cache = drake_cache_object))

  cli::cli_h2("Running the single-sample pipeline")

  drake::make(
    plan,
    targets = cfg_pipeline$DRAKE_TARGETS,
    envir = getNamespace("scdrake"),
    verbose = cfg_pipeline$DRAKE_VERBOSITY,
    cache = drake_cache_object,
    parallelism = cfg_pipeline$DRAKE_PARALLELISM,
    jobs = cfg_pipeline$DRAKE_N_JOBS,
    jobs_preprocess = cfg_pipeline$DRAKE_N_JOBS_PREPROCESS,
    packages = packages[packages != "scdrake"],
    prework = prework,
    trigger = drake_trigger,
    seed = cfg_pipeline$SEED,
    caching = cfg_pipeline$DRAKE_CACHING,
    keep_going = cfg_pipeline$DRAKE_KEEP_GOING,
    memory_strategy = cfg_pipeline$DRAKE_MEMORY_STRATEGY,
    lock_envir = cfg_pipeline$DRAKE_LOCK_ENVIR,
    log_build_times = cfg_pipeline$DRAKE_LOG_BUILD_TIMES,
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
  cli_alert_info(
    "Calling {.code drake::r_make()} using the entry script {.file {if (is_null(drake_file)) '_drake.R' else drake_file}}"
  )
  drake::r_make(drake_file, ...)
  return(invisible(TRUE))
}
