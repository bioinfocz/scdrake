skip_if(is_false(getOption("scdrake_test_run_pipeline")))

project_dir <- getOption(
  "scdrake_test_run_pipeline_base_out_dir",
  default = get_scdrake_default_options[["scdrake_test_run_pipeline_base_out_dir"]]
) %>%
  fs::path_abs() %>%
  as.character()

cli::cat_line()

if (is_false(getOption("scdrake_test_run_pipeline_keep_files"))) {
  cli_alert_info("Test files located in {.file {project_dir}} will be removed after tests finish.")
  withr::defer(fs::dir_delete(project_dir), teardown_env())
} else {
  cli_alert_info("Test files located in {.file {project_dir}} will be kept after tests finish.")
}

data_dir <- fs::path(project_dir, "data")
pbmc1k_data_dir <- fs::path(data_dir, "pbmc1k")
if (!fs::dir_exists(pbmc1k_data_dir) || is_empty(fs::dir_ls(pbmc1k_data_dir))) {
  download_pbmc1k(pbmc1k_data_dir, ask = FALSE)
}

pbmc3k_data_dir <- fs::path(data_dir, "pbmc3k")
if (!fs::dir_exists(pbmc3k_data_dir) || is_empty(fs::dir_ls(pbmc3k_data_dir))) {
  download_pbmc3k(pbmc3k_data_dir, ask = FALSE)
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

withr::local_dir(project_dir)
if (!is_r_build_check) {
  withr::defer(withr::with_dir("../..", here::i_am("DESCRIPTION")), teardown_env())
}
init_project(project_dir, use_rstudio = FALSE, ask = FALSE)
here::i_am(".here")

## -- A test file for additional cell data.
additional_cell_data <- data.frame(
  Barcode = c("AAACCCAAGGAGAGTA-1", "AAACGCTTCAGCCCAG-1", "AAAGAACAGACGACTG-1", "AAAGAACCAATGGCAG-1", "AAAGAACGTCTGCAAT-1"),
  letters = letters[1:5],
  cluster_sc3_2 = glue("cluster_{letters[1:5]}"),
  cluster_sc3_6_custom = glue("cluster_{LETTERS[1:5]}")
)
saveRDS(additional_cell_data, "pbmc1k_additional_cell_data.Rds")

test_that("the full single-sample pipeline for PBMC 1k dataset finishes", {
  skip_if(is_false(getOption("scdrake_test_run_pipeline_single_sample_full")))
  cli_alert_info("TEST: PBMC 1k full")

  .apply_config_patches(
    patches_dir = fs::path(test_path_abs, "run_pipeline_config_patches/single_sample_pbmc1k_full"),
    analysis_config_dir = getOption("scdrake_single_sample_config_dir")
  )

  expect_true(run_single_sample_r())
})

skip_if(is_false(getOption("scdrake_test_run_pipeline_integration")))

test_that("'sce_final_norm_clustering' target in single-sample pipeline for PBMC 1k dataset finishes", {
  ## -- We can skip this test if full pipeline for PBMC 1k was run -> it also contains the "sce_final_norm_clustering"
  ## -- target needed for integration.
  skip_if(is_true(getOption("scdrake_test_run_pipeline_single_sample_full")))
  cli_alert_info("TEST: PBMC 1k sce_final_norm_clustering")

  .apply_config_patches(
    patches_dir = fs::path(test_path_abs, "run_pipeline_config_patches/single_sample_pbmc1k"),
    analysis_config_dir = getOption("scdrake_single_sample_config_dir")
  )

  expect_true(run_single_sample_r())
})

## -- This is a second dataset used for integration. We only need the "sce_final_norm_clustering" target
## -- (this is specified in single_sample_pbmc3k/pipeline.default.yaml patch).
test_that("'sce_final_norm_clustering' target in single-sample pipeline for PBMC 3k dataset finishes", {
  cli_alert_info("TEST: PBMC 3k sce_final_norm_clustering")
  .apply_config_patches(
    patches_dir = fs::path(test_path_abs, "run_pipeline_config_patches/single_sample_pbmc3k"),
    analysis_config_dir = getOption("scdrake_single_sample_config_dir")
  )

  expect_true(run_single_sample_r())
  ## -- To test the integration pipeline's ability to also start from a SCE object.
  saveRDS(
    drake::readd(sce_final_norm_clustering, path = fs::path(project_dir, ".drake_pbmc3k")),
    fs::path(project_dir, "sce_final_norm_clustering_pbmc3k.Rds")
  )
})

test_that("integration pipeline finishes", {
  cli_alert_info("TEST: integration")
  .apply_config_patches(
    patches_dir = fs::path(test_path_abs, "run_pipeline_config_patches/integration"),
    analysis_config_dir = getOption("scdrake_integration_config_dir")
  )

  expect_true(run_integration())
})
