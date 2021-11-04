library(conflicted)
library(rlang)
conflict_prefer("is_null", "rlang")
library(cli)
library(here)

if (Sys.getenv("SCDRAKE_TEST_RUN_PIPELINE") == "TRUE") {
  cli_alert_info("Running in test mode ({.envvar SCDRAKE_TEST_RUN_PIPELINE} is {.code TRUE}).")
  scdrake_pkg_dir <- Sys.getenv("SCDRAKE_PKG_DIR")
  devtools::load_all(scdrake_pkg_dir, export_all = FALSE)
  cli_alert_success("Loaded {.pkg scdrake} package from {.file {scdrake_pkg_dir}}")
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
  } else {
    future::plan(future.callr::callr, workers = cfg_pipeline$DRAKE_N_JOBS)
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

##-- Prevent issues with BLAS operations when a massive target parallelism is used.
if (!is.null(cfg_pipeline$BLAS_N_THREADS)) {
  RhpcBLASctl::blas_set_num_threads(cfg_pipeline$BLAS_N_THREADS)
}

# if (!is.null(cfg_pipeline$RSTUDIO_PANDOC) && get_sys_env("RSTUDIO_PANDOC", type = "character", verbose = FALSE) != "") {
if (!is_null(cfg_pipeline$RSTUDIO_PANDOC)) {
  Sys.setenv(RSTUDIO_PANDOC = cfg_pipeline$RSTUDIO_PANDOC)
}

##-- To fix https://github.com/rstudio/rmarkdown/issues/1632
prework <- 'assignInNamespace("clean_tmpfiles", function() {invisible(NULL)}, ns = "rmarkdown")'

drake_cache_object <- drake::new_cache(path = cfg_pipeline$DRAKE_CACHE_DIR)

if (cfg_pipeline$DRAKE_UNLOCK_CACHE && !is.null(drake_cache_object)) {
  cli_alert_info("Unlocking {.pkg drake} cache in {.file {cfg_pipeline$DRAKE_CACHE_DIR}}")
  drake_cache_object$unlock()
}

options(
  future.globals.maxSize = 200000 * 1024 ^ 2,
  clustermq.scheduler = cfg_pipeline$DRAKE_CLUSTERMQ_SCHEDULER,
  yaml.eval.expr = TRUE,
  knitr.table.format = "html",
  ##-- To fix a bug with the latest RSQLite and AnnotationDbi packages.
  connectionObserver = NULL,
  rstudio_drake_cache = drake_cache_object
)

create_integration_dirs(cfg)
plan <- get_integration_plan(cfg, cfg_pipeline)

verbose %&&% cli::cli_h2("Running the integration pipeline")
verbose %&&% cli({
  cli_alert_info("BASE OUTPUT DIRECTORY: {cfg$main$BASE_OUT_DIR}")

  if (is.null(cfg_pipeline$DRAKE_TARGETS)) {
    cli_alert_info("TARGETS: NULL (= all)")
  } else {
    cli_alert_info("TARGETS:")
    cli::cli_ul(cfg_pipeline$DRAKE_TARGETS)
  }
})

drake::drake_config(
  plan,
  targets = cfg_pipeline$DRAKE_TARGETS,
  envir = getNamespace("scdrake"),
  verbose = as.integer(cfg_pipeline$DRAKE_VERBOSITY),
  cache = drake_cache_object,
  parallelism = cfg_pipeline$DRAKE_PARALLELISM,
  jobs = cfg_pipeline$DRAKE_N_JOBS,
  jobs_preprocess = cfg_pipeline$DRAKE_N_JOBS_PREPROCESS,
  packages = c("HDF5Array", rev(.packages())),
  prework = prework,
  seed = cfg_pipeline$SEED,
  caching = "worker",
  keep_going = cfg_pipeline$DRAKE_KEEP_GOING,
  memory_strategy = "speed",
  lock_envir = cfg_pipeline$DRAKE_LOCK_ENVIR,
  log_build_times = FALSE,
  format = cfg_pipeline$DRAKE_FORMAT,
  log_worker = TRUE
)
