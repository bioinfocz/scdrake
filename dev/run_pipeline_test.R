run_pipeline_test <- function(output_dir, test_pattern = "run_pipeline$") {
  withr::with_envvar(
    new = list(
      SCDRAKE_TEST_DOWNLOAD_YQ = "FALSE",
      SCDRAKE_TEST_RUN_PIPELINE = "TRUE",
      SCDRAKE_TEST_RUN_PIPELINE_SINGLE_SAMPLE_FULL = "TRUE",
      SCDRAKE_TEST_RUN_PIPELINE_INTEGRATION = "TRUE",
      SCDRAKE_TEST_RUN_PIPELINE_BASE_OUT_DIR = fs::path_abs(output_dir),
      SCDRAKE_TEST_RUN_PIPELINE_CLEAR_CONFIG_PATCHES = "TRUE",
      SCDRAKE_TEST_RUN_PIPELINE_VIGNETTE_GET_STARTED = "TRUE",
      SCDRAKE_TEST_RUN_PIPELINE_VIGNETTE_INTEGRATION = "TRUE"
    ),
    code = {
      devtools::test(filter = test_pattern)
    }
  )
}

run_pipeline_test("../scdrake_run_pipeline_test_output_20")
run_pipeline_test("../scdrake_run_pipeline_vignette_test_outputs/01", test_pattern = "run_pipeline_vignette$")
