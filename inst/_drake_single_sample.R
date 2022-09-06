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

check_scdrake_packages()

update_pipeline_config()
update_single_sample_configs()

cfg_pipeline <- load_pipeline_config()
cfg <- load_single_sample_configs(cfg_pipeline = cfg_pipeline)

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
    str_line(
      "{.field DRAKE_REBUILD} is {.val 'all'} -> ",
      "the pipeline will be run from scratch."
    ) %>% cli_alert_info()
    drake_trigger <- drake::trigger(condition = TRUE)
  } else if (drake_rebuild == "current") {
    str_line(
      "{.field DRAKE_REBUILD} is {.val 'current'} -> ",
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
  rstudio_drake_cache = drake_cache_object
)

create_single_sample_dirs(cfg)
plan <- get_single_sample_plan(cfg, cfg_pipeline)

plan_custom_file <- getOption("scdrake_plan_custom_file")
plan_custom <- load_custom_plan(plan_custom_file)
if (!rlang::is_null(plan_custom)) {
  cli_alert_info("Extending the plan with a custom one defined in {.file {plan_custom_file}}")
  plan <- drake::bind_plans(plan, plan_custom)
}

verbose %&&% cli::cli_h2("Running the single-sample pipeline")
verbose %&&% cli({
  cli_alert_info("BASE OUTPUT DIRECTORY: {cfg$main$BASE_OUT_DIR}")

  if (is.null(cfg_pipeline$DRAKE_TARGETS)) {
    cli_alert_info("TARGETS: NULL (= all)")
  } else {
    cli_alert_info("TARGETS:")
    cli::cli_ul(cfg_pipeline$DRAKE_TARGETS)
  }
})

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
  caching = "worker",
  keep_going = cfg_pipeline$DRAKE_KEEP_GOING,
  memory_strategy = "speed",
  lock_envir = cfg_pipeline$DRAKE_LOCK_ENVIR,
  log_build_times = FALSE,
  format = cfg_pipeline$DRAKE_FORMAT,
  log_worker = TRUE
)
