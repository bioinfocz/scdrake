skip_if(!test_env_vars$SCDRAKE_TEST_RUN_PIPELINE)

cli::cli_h1("{.file tests/testthat/test-run_pipeline.R}")

if (test_env_vars$SCDRAKE_TEST_RUN_PIPELINE_CLEAR_CONFIG_PATCHES) {
  ## -- This will delete all files in tests/testthat/run_pipeline_config_patches not matching the "*.default.yaml" pattern.
  cli_alert_info("Removing local config patches in {.file tests/testthat/run_pipeline_config_patches/}")
  fs::dir_ls(fs::path(test_path_abs, "run_pipeline_config_patches/"), glob = "*.default.yaml", recurse = TRUE, invert = TRUE, type = "file") %>%
    fs::file_delete()
}

root_work_dir <- test_env_vars$SCDRAKE_TEST_RUN_PIPELINE_BASE_OUT_DIR %>%
  fs::path_abs() %>%
  fs::path("example_data") %>%
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

data_dir <- fs::path(root_work_dir, "data")
pbmc1k_data_dir <- fs::path(data_dir, "pbmc1k")
if (!fs::dir_exists(pbmc1k_data_dir) || is_empty(fs::dir_ls(pbmc1k_data_dir))) {
  download_pbmc1k(pbmc1k_data_dir, ask = FALSE)
}

pbmc3k_data_dir <- fs::path(data_dir, "pbmc3k")
if (!fs::dir_exists(pbmc3k_data_dir) || is_empty(fs::dir_ls(pbmc3k_data_dir))) {
  download_pbmc3k(pbmc3k_data_dir, ask = FALSE)
}

pbmc1k_dir <- fs::path(root_work_dir, "pbmc1k")
try(fs::dir_delete(fs::path(pbmc1k_dir, "config")), silent = TRUE)
init_project(pbmc1k_dir, use_rstudio = FALSE, ask = FALSE)
withr::local_dir(pbmc1k_dir)
cli_alert_info("Working directory: {.file {getwd()}}")
here::i_am(".here")

## -- A test file for additional cell data.
additional_cell_data <- data.frame(
  Barcode = c("AAACCCAAGGAGAGTA-1", "AAACGCTTCAGCCCAG-1", "AAAGAACAGACGACTG-1", "AAAGAACCAATGGCAG-1", "AAAGAACGTCTGCAAT-1"),
  letters = letters[1:5],
  cluster_sc3_2 = glue("cluster_{letters[1:5]}"),
  cluster_sc3_6_custom = glue("cluster_{LETTERS[1:5]}")
)
saveRDS(additional_cell_data, "additional_cell_data.Rds")

test_that("the full single-sample pipeline for PBMC 1k dataset finishes", {
  skip_if(!test_env_vars$SCDRAKE_TEST_RUN_PIPELINE_SINGLE_SAMPLE_FULL)
  cli::cli_h2("TEST: PBMC 1k full (example data)")

  .apply_config_patches(
    patches_dir = fs::path(test_path_abs, "run_pipeline_config_patches/single_sample_pbmc1k_full"),
    analysis_config_dir = getOption("scdrake_single_sample_config_dir")
  )

  expect_true(run_single_sample_r())
  cli::cli_h2("Done!")
})

## -- For some reason, this test is failing in the sce_norm target in Seurat::SCTransform():
## -- Centering data matrix
## -- <subscriptOutOfBoundsError in `*tmp*`[[1]]: subscript out of bounds>
## -- But when you call run_single_sample_r() in the test project directory, it finishes without any problems.
test_that("the full single-sample pipeline for PBMC 1k dataset finishes (using SCTransform)", {
  skip_if(!test_env_vars$SCDRAKE_TEST_RUN_PIPELINE_SINGLE_SAMPLE_FULL_SCT)

  cli::cli_h2("TEST: PBMC 1k full SCTransform (example data)")

  pbmc1k_sct_dir <- fs::path(root_work_dir, "pbmc1k_sctransform")
  try(fs::dir_delete(fs::path(pbmc1k_sct_dir, "config")), silent = TRUE)
  init_project(pbmc1k_sct_dir, use_rstudio = FALSE, ask = FALSE)
  withr::local_dir(pbmc1k_sct_dir)
  cli_alert_info("Working directory: {.file {getwd()}}")
  here::i_am(".here")

  # cli::cli_alert_info("Reusing the drake cache from the full single-sample pipeline test.")
  # fs::dir_copy(fs::path(pbmc1k_dir, ".drake"), pbmc1k_sct_dir)
  fs::file_copy(fs::path(pbmc1k_dir, "additional_cell_data.Rds"), pbmc1k_sct_dir, overwrite = TRUE)

  .apply_config_patches(
    patches_dir = fs::path(test_path_abs, "run_pipeline_config_patches/single_sample_pbmc1k_full_sct"),
    analysis_config_dir = getOption("scdrake_single_sample_config_dir")
  )

  expect_true(run_single_sample_r())
  cli::cli_h2("Done!")
})

skip_if(!test_env_vars$SCDRAKE_TEST_RUN_PIPELINE_INTEGRATION)

test_that("'sce_final_norm_clustering' target in single-sample pipeline for PBMC 1k dataset finishes", {
  ## -- We can skip this test if full pipeline for PBMC 1k was run -> it also contains the "sce_final_norm_clustering"
  ## -- target needed for integration.
  skip_if(
    test_env_vars$SCDRAKE_TEST_RUN_PIPELINE_SINGLE_SAMPLE_FULL,
    message = "Skipping the test for PBMC 1k sce_final_norm_clustering, because single-sample pipeline has already made this target."
  )
  cli::cli_h2("TEST: PBMC 1k sce_final_norm_clustering (example data)")

  .apply_config_patches(
    patches_dir = fs::path(test_path_abs, "run_pipeline_config_patches/single_sample_pbmc1k"),
    analysis_config_dir = getOption("scdrake_single_sample_config_dir")
  )

  expect_true(run_single_sample_r())
  cli::cli_h2("Done!")
})

## -- This is a second dataset used for integration. We only need the "sce_final_norm_clustering" target
## -- (this is specified in single_sample_pbmc3k/pipeline.default.yaml patch).
pbmc3k_dir <- fs::path(root_work_dir, "pbmc3k")
try(fs::dir_delete(fs::path(pbmc3k_dir, "config")), silent = TRUE)
init_project(pbmc3k_dir, use_rstudio = FALSE, ask = FALSE)
withr::local_dir(pbmc3k_dir)
cli_alert_info("Working directory: {.file {getwd()}}")
here::i_am(".here")

test_that("'sce_final_norm_clustering' target in single-sample pipeline for PBMC 3k dataset finishes", {
  cli::cli_h2("TEST: PBMC 3k sce_final_norm_clustering (example data)")
  .apply_config_patches(
    patches_dir = fs::path(test_path_abs, "run_pipeline_config_patches/single_sample_pbmc3k"),
    analysis_config_dir = getOption("scdrake_single_sample_config_dir")
  )

  expect_true(run_single_sample_r())
  ## -- To test the integration pipeline's ability to also start from a SCE object.
  saveRDS(
    drake::readd(sce_final_norm_clustering),
    fs::path(pbmc3k_dir, "sce_final_norm_clustering.Rds")
  )
  cli::cli_h2("Done!")
})

integration_dir <- fs::path(root_work_dir, "integration")
try(fs::dir_delete(fs::path(integration_dir, "config")), silent = TRUE)
init_project(integration_dir, use_rstudio = FALSE, ask = FALSE)
withr::local_dir(integration_dir)
cli_alert_info("Working directory: {.file {getwd()}}")
here::i_am(".here")

test_that("integration pipeline finishes", {
  cli::cli_h2("TEST: integration (example data)")

  .apply_config_patches(
    patches_dir = fs::path(test_path_abs, "run_pipeline_config_patches/integration"),
    analysis_config_dir = getOption("scdrake_integration_config_dir")
  )

  expect_true(run_integration())
  cli::cli_h2("Done!")
})
