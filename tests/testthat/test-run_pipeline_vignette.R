## -- These are tests for example PBMC data given in the instructions in Get started vignette.
skip_if(is_false(getOption("scdrake_test_run_pipeline_vignette_get_started")))

if (getOption("scdrake_test_run_pipeline_clear_config_patches")) {
  ## -- This will delete all files in tests/testthat/run_pipeline_vignette_config_patches not matching the "*.default.yaml" pattern.
  cli_alert_info("Removing local config patches in {.file tests/testthat/run_pipeline_vignette_config_patches/}")
  fs::dir_ls(fs::path(test_path_abs, "run_pipeline_vignette_config_patches/"), glob = "*.default.yaml", recurse = TRUE, invert = TRUE, type = "file") %>%
    fs::file_delete()
}

root_work_dir <- getOption("scdrake_test_run_pipeline_base_out_dir") %>%
  fs::path_abs() %>%
  as.character()

cli::cat_line()

if (is_false(getOption("scdrake_test_run_pipeline_keep_files"))) {
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
  SCDRAKE_PIPELINE_CONFIG_DIR = "config",
  SCDRAKE_SINGLE_SAMPLE_CONFIG_DIR = "config/single_sample",
  SCDRAKE_INTEGRATION_CONFIG_DIR = "config/integration",
  SCDRAKE_TEST_RUN_PIPELINE = "TRUE",
  SCDRAKE_PKG_DIR = fs::path_abs("../..")
)

if (!is_r_build_check) {
  ## -- Reset here() to scdrake package root.
  withr::defer(withr::with_dir(package_root, here::i_am("DESCRIPTION")), teardown_env())
}

pbmc1k_dir <- fs::path(root_work_dir, "pbmc1k")
try(fs::dir_delete(fs::path(pbmc1k_dir, "config")), silent = TRUE)
init_project(pbmc1k_dir, use_rstudio = FALSE, ask = FALSE, download_example_data = TRUE)
withr::local_dir(pbmc1k_dir)
here::i_am(".here")

test_that("the full single-sample pipeline from the Get started vignette finishes", {
  cli_alert_info("TEST: PBMC 1k full")

  .apply_config_patches(
    patches_dir = fs::path(test_path_abs, "run_pipeline_vignette_config_patches/single_sample_pbmc1k"),
    analysis_config_dir = getOption("scdrake_single_sample_config_dir")
  )

  expect_true(run_single_sample_r())

  sce <- drake::readd(sce_final_input_qc)
  expect_s4_class(sce, "SingleCellExperiment")
  p <- scater::plotExpression(sce, "NOC2L", exprs_values = "counts", swap_rownames = "SYMBOL")
  expect_s3_class(p, "ggplot")
})

## -- This is replicating the vignette part "Modifying parameters and rerunning the pipeline"
test_that("the modified single-sample pipeline from the Get started vignette finishes", {
  cli_alert_info("TEST: PBMC 1k modified")

  .apply_config_patches(
    patches_dir = fs::path(test_path_abs, "run_pipeline_vignette_config_patches/single_sample_pbmc1k_modified"),
    analysis_config_dir = getOption("scdrake_single_sample_config_dir")
  )

  expect_true(run_single_sample_r())
})

skip_if(is_false(getOption("scdrake_test_run_pipeline_vignette_integration")))

## -- This is a second dataset used for integration. We only need the "sce_final_norm_clustering" target
## -- (this is specified in single_sample_pbmc3k/pipeline.default.yaml patch).
pbmc3k_dir <- fs::path(root_work_dir, "pbmc3k")
try(fs::dir_delete(fs::path(pbmc3k_dir, "config")), silent = TRUE)
init_project(pbmc3k_dir, use_rstudio = FALSE, ask = FALSE)
withr::local_dir(pbmc3k_dir)
here::i_am(".here")

test_that("'sce_final_norm_clustering' target in single-sample pipeline for PBMC 3k dataset in the Integration guide vignette finishes", {
  cli_alert_info("TEST: PBMC 3k sce_final_norm_clustering")
  .apply_config_patches(
    patches_dir = fs::path(test_path_abs, "run_pipeline_vignette_config_patches/single_sample_pbmc3k"),
    analysis_config_dir = getOption("scdrake_single_sample_config_dir")
  )

  expect_true(run_single_sample_r())
})

integration_dir <- fs::path(root_work_dir, "integration")
try(fs::dir_delete(fs::path(integration_dir, "config")), silent = TRUE)
init_project(integration_dir, use_rstudio = FALSE, ask = FALSE)
withr::local_dir(integration_dir)
here::i_am(".here")

test_that("integration pipeline finishes", {
  cli_alert_info("TEST: integration")

  .apply_config_patches(
    patches_dir = fs::path(test_path_abs, "run_pipeline_vignette_config_patches/integration"),
    analysis_config_dir = getOption("scdrake_integration_config_dir")
  )

  expect_true(run_integration())
})
