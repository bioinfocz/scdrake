cli::cli_h1("Begin {.file _drake_integration.R}")

library(conflicted)
library(rlang)
conflict_prefer("is_null", "rlang")
library(cli)
library(here)
library(magrittr)
conflict_prefer("not", "magrittr")

cli_alert_info("WORKING DIRECTORY: {.file {getwd()}}")
cli_alert_info("R LIBRARY PATHS ({.code .libPaths()}): {.file {paste0(.libPaths(), collapse = ':')}}")

scdrake_pkg_dir <- Sys.getenv("SCDRAKE_PKG_DIR", NA)

if (!is.na(scdrake_pkg_dir) && scdrake_pkg_dir != "") {
  cli_alert_info("Loading {.pkg scdrake} from {.file {scdrake_pkg_dir}} (as set by the {.envvar SCDRAKE_PKG_DIR} environment variable).")
  devtools::load_all(scdrake_pkg_dir, export_all = FALSE)
} else {
  library(scdrake)
}

conflict_prefer("is_true", "rlang")

here::i_am(".here")
verbose <- getOption("scdrake_verbose")

update_pipeline_config()
update_integration_configs()

cfg_pipeline <- load_pipeline_config()
cfg <- load_integration_configs(cfg_pipeline = cfg_pipeline)

set.seed(cfg_pipeline$SEED)

if (cfg_pipeline$DRAKE_PARALLELISM == "future") {
  if (parallelly::supportsMulticore()) {
    future::plan(future::multicore, workers = cfg_pipeline$DRAKE_N_JOBS)
    cli_alert_info("Using {.code future::multicore} with {cfg_pipeline$DRAKE_N_JOBS} workers.")
  } else {
    future::plan(future.callr::callr, workers = cfg_pipeline$DRAKE_N_JOBS)
    cli_alert_info("Using {.code future.callr::callr} (multisession) with {cfg_pipeline$DRAKE_N_JOBS} workers.")
  }
}

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
if (!is.null(cfg_pipeline$BLAS_N_THREADS)) {
  RhpcBLASctl::blas_set_num_threads(cfg_pipeline$BLAS_N_THREADS)
}

if (!is_null(cfg_pipeline$RSTUDIO_PANDOC)) {
  Sys.setenv(RSTUDIO_PANDOC = cfg_pipeline$RSTUDIO_PANDOC)
}

## -- To fix https://github.com/rstudio/rmarkdown/issues/1632
prework <- 'assignInNamespace("clean_tmpfiles", function() {invisible(NULL)}, ns = "rmarkdown")'

drake_cache_object <- drake::new_cache(path = cfg_pipeline$DRAKE_CACHE_DIR)

if (cfg_pipeline$DRAKE_UNLOCK_CACHE && !is.null(drake_cache_object)) {
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

options(
  future.globals.maxSize = 200000 * 1024^2,
  clustermq.scheduler = cfg_pipeline$DRAKE_CLUSTERMQ_SCHEDULER,
  yaml.eval.expr = TRUE,
  knitr.table.format = "html",
  ## -- To fix a bug with the latest RSQLite and AnnotationDbi packages.
  connectionObserver = NULL,
  rstudio_drake_cache = drake_cache_object,
  DT.warn.size = FALSE
)

create_integration_dirs(cfg)
plan <- get_integration_plan(cfg, cfg_pipeline)

plan_custom_file <- getOption("scdrake_plan_custom_file")
plan_custom <- load_custom_plan(plan_custom_file)
if (!rlang::is_null(plan_custom)) {
  cli_alert_info("Extending the plan with a custom one defined in {.file {plan_custom_file}}")
  plan <- drake::bind_plans(plan, plan_custom)
}

check_scdrake()

cli::cli_h2("Running the integration pipeline")
cli_alert_info("BASE OUTPUT DIRECTORY: {.file {cfg$main$BASE_OUT_DIR}}")
if (is.null(cfg_pipeline$DRAKE_TARGETS)) {
  cli_alert_info("TARGETS: NULL (= all)")
} else {
  cli_alert_info("TARGETS:")
  cli::cli_ul(cfg_pipeline$DRAKE_TARGETS)
}

packages <- c("HDF5Array", "ensembldb", rev(.packages()))

drake::drake_config(
  plan,
  targets = cfg_pipeline$DRAKE_TARGETS,
  envir = getNamespace("scdrake"),
  verbose = as.integer(cfg_pipeline$DRAKE_VERBOSITY),
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
  log_worker = TRUE
)
