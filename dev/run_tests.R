withr::with_envvar(
  new = list(
    SCDRAKE_TEST_DOWNLOAD_YQ = "TRUE",
    SCDRAKE_TEST_RUN_PIPELINE = "FALSE"
  ),
  code = {
    devtools::test()
  }
)
