withr::with_envvar(
  new = list(
    SCDRAKE_TEST_DOWNLOAD_YQ = "FALSE",
    SCDRAKE_TEST_RUN_PIPELINE = "TRUE",
    SCDRAKE_TEST_RUN_PIPELINE_SINGLE_SAMPLE_FULL = "TRUE",
    SCDRAKE_TEST_RUN_PIPELINE_INTEGRATION = "TRUE",
    SCDRAKE_TEST_RUN_PIPELINE_BASE_OUT_DIR = fs::path_abs("../scdrake_run_pipeline_test_output_8")
  ),
  code = {
    devtools::test(filter = "run_pipeline")
  }
)

# callr::r(
#   devtools::test,
#   args = list(filter = "run_pipeline"),
#   env = c(
#     callr::rcmd_safe_env(),
#     SCDRAKE_TEST_DOWNLOAD_YQ = "FALSE",
#     SCDRAKE_TEST_RUN_PIPELINE = "TRUE",
#     SCDRAKE_TEST_RUN_PIPELINE_SINGLE_SAMPLE_FULL = "TRUE",
#     SCDRAKE_TEST_RUN_PIPELINE_INTEGRATION = "TRUE",
#     SCDRAKE_TEST_RUN_PIPELINE_BASE_OUT_DIR = "/mnt/foibe3/Users/novotnyj/projects/bioinfocz/scdrake_run_pipeline_test_output_3"
#   ),
#   show = TRUE,
#   package = "devtools"
# )
