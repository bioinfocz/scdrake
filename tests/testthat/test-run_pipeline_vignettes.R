## -- These are tests for example PBMC data given in the instructions in Get started vignette.
skip_if(!test_env_vars$SCDRAKE_TEST_RUN_PIPELINE_VIGNETTES)

cli::cli_h1("{.file tests/testthat/test-run_pipeline_vignettes.R}")

if (test_env_vars$SCDRAKE_TEST_RUN_PIPELINE_CLEAR_CONFIG_PATCHES) {
  ## -- This will delete all files in tests/testthat/run_pipeline_vignette_config_patches not matching the "*.default.yaml" pattern.
  cli_alert_info("Removing local config patches in {.file tests/testthat/run_pipeline_vignette_config_patches/}")
  fs::dir_ls(fs::path(test_path_abs, "run_pipeline_vignette_config_patches/"), glob = "*.default.yaml", recurse = TRUE, invert = TRUE, type = "file") %>%
    fs::file_delete()
}

root_work_dir <- test_env_vars$SCDRAKE_TEST_RUN_PIPELINE_BASE_OUT_DIR %>%
  fs::path_abs() %>%
  fs::path("vignettes") %>%
  as.character()

cli::cat_line()

if (!test_env_vars$SCDRAKE_TEST_RUN_PIPELINE_KEEP_FILES) {
  cli_alert_info("Test files located in {.file {root_work_dir}} will be removed after tests finish.")
  withr::defer(fs::dir_delete(root_work_dir), teardown_env())
} else {
  cli_alert_info("Test files located in {.file {root_work_dir}} will be kept after tests finish.")
}

withr::local_options(list(
  scdrake_verbose = FALSE,
  scdrake_pipeline_config_dir = "config",
  scdrake_single_sample_config_dir = "config/single_sample",
  scdrake_integration_config_dir = "config/integration"
))

withr::local_envvar(
  SCDRAKE_VERBOSE = "FALSE",
  SCDRAKE_PIPELINE_CONFIG_DIR = "config",
  SCDRAKE_SINGLE_SAMPLE_CONFIG_DIR = "config/single_sample",
  SCDRAKE_INTEGRATION_CONFIG_DIR = "config/integration"
)

if (!test_env_vars$R_BUILD_CHECK) {
  ## -- Reset here() to scdrake package root.
  withr::defer(withr::with_dir(test_env_vars$SCDRAKE_PKG_DIR, here::i_am("DESCRIPTION")), teardown_env())
}

pbmc1k_dir <- fs::path(root_work_dir, "pbmc1k")
try(fs::dir_delete(fs::path(pbmc1k_dir, "config")), silent = TRUE)
init_project(pbmc1k_dir, use_rstudio = FALSE, ask = FALSE, download_example_data = TRUE)
withr::local_dir(pbmc1k_dir)
cli_alert_info("Working directory: {.file {getwd()}}")
here::i_am(".here")

test_that("the full single-sample pipeline from the Get started vignette finishes", {
  cli::cli_h2("TEST: PBMC 1k full (Get Started vignette)")

  .apply_config_patches(
    patches_dir = fs::path(test_path_abs, "run_pipeline_vignette_config_patches/single_sample_pbmc1k"),
    analysis_config_dir = getOption("scdrake_single_sample_config_dir")
  )

  expect_true(run_single_sample_r())

  sce <- drake::readd(sce_final_input_qc)
  expect_s4_class(sce, "SingleCellExperiment")
  p <- scater::plotExpression(sce, "NOC2L", exprs_values = "counts", swap_rownames = "SYMBOL")
  expect_s3_class(p, "ggplot")
  cli::cli_h2("Done!")
})

## -- This is replicating the vignette part "Modifying parameters and rerunning the pipeline"
test_that("the modified single-sample pipeline from the Get started vignette finishes", {
  cli::cli_h2("TEST: PBMC 1k modified")

  .apply_config_patches(
    patches_dir = fs::path(test_path_abs, "run_pipeline_vignette_config_patches/single_sample_pbmc1k_modified"),
    analysis_config_dir = getOption("scdrake_single_sample_config_dir")
  )

  expect_true(run_single_sample_r())
  cli::cli_h2("Done!")
})

skip_if(!test_env_vars$SCDRAKE_TEST_RUN_PIPELINE_VIGNETTE_INTEGRATION)

## -- This is a second dataset used for integration. We only need the "sce_final_norm_clustering" target
## -- (this is specified in single_sample_pbmc3k/pipeline.default.yaml patch).
pbmc3k_dir <- fs::path(root_work_dir, "pbmc3k")
try(fs::dir_delete(fs::path(pbmc3k_dir, "config")), silent = TRUE)
init_project(pbmc3k_dir, use_rstudio = FALSE, ask = FALSE)
withr::local_dir(pbmc3k_dir)
cli_alert_info("Working directory: {.file {getwd()}}")
here::i_am(".here")

test_that("'sce_final_norm_clustering' target in single-sample pipeline for PBMC 3k dataset in the Integration guide vignette finishes", {
  cli::cli_h2("TEST: PBMC 3k sce_final_norm_clustering (Integration vignette)")

  .apply_config_patches(
    patches_dir = fs::path(test_path_abs, "run_pipeline_vignette_config_patches/single_sample_pbmc3k"),
    analysis_config_dir = getOption("scdrake_single_sample_config_dir")
  )

  expect_true(run_single_sample_r())
  cli::cli_h2("Done!")
})

integration_dir <- fs::path(root_work_dir, "integration")
try(fs::dir_delete(fs::path(integration_dir, "config")), silent = TRUE)
init_project(integration_dir, use_rstudio = FALSE, ask = FALSE)
withr::local_dir(integration_dir)
cli_alert_info("Working directory: {.file {getwd()}}")
here::i_am(".here")

test_that("integration pipeline finishes", {
  cli::cli_h2("TEST: integration (Integration vignette)")

  .apply_config_patches(
    patches_dir = fs::path(test_path_abs, "run_pipeline_vignette_config_patches/integration"),
    analysis_config_dir = getOption("scdrake_integration_config_dir")
  )

  expect_true(run_integration())
  cli::cli_h2("Done!")
})
