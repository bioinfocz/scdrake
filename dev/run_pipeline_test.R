run_pipeline_test <- function(output_dir) {
  withr::with_envvar(
    new = list(
      SCDRAKE_TEST_DOWNLOAD_YQ = "FALSE",
      SCDRAKE_TEST_RUN_PIPELINE = "TRUE",
      SCDRAKE_TEST_RUN_PIPELINE_SINGLE_SAMPLE_FULL = "TRUE",
      SCDRAKE_TEST_RUN_PIPELINE_INTEGRATION = "TRUE",
      SCDRAKE_TEST_RUN_PIPELINE_BASE_OUT_DIR = fs::path_abs(output_dir)
    ),
    code = {
      devtools::test(filter = "run_pipeline")
    }
  )
}

run_pipeline_test("../scdrake_run_pipeline_test_output_10")
