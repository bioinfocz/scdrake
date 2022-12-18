cli::cli_h1("{.file tests/testthat/test-yq.R}")

withr::local_options(list(
  scdrake_verbose = FALSE
))

test_that("yq tool's binary is successfully downloaded or reused", {
  skip_if(!test_env_vars$SCDRAKE_TEST_DOWNLOAD_YQ)
  skip_on_bioc()
  skip_on_cran()

  tmpfile <- fs::file_temp()
  res <- download_yq(destfile = tmpfile, set_as_default = FALSE, ask = FALSE, overwrite = TRUE)
  expect_type(res, "character")
  yq_binary <- res[1]
  expect_true(fs::file_exists(yq_binary))
  expect_invisible(check_yq(yq_binary = yq_binary))
  expect_null(download_yq(destfile = yq_binary, set_as_default = FALSE, ask = FALSE))
})

test_that("yq merge shell command works", {
  res <- yq_merge_cmd("yq_test_files/src.yaml", "yq_test_files/dest.yaml")
  expect_type(res, "character")
  expect_length(res, 1)
  expect_snapshot(res, cran = TRUE)

  tmpfile <- fs::file_temp()
  yq_merge_cmd("yq_test_files/src.yaml", "yq_test_files/dest.yaml", stdout = tmpfile)
  yaml <- yaml::yaml.load_file(tmpfile)
  expect_identical(yaml, list(a = "aa", b = "bb", c = "c"))

  expect_error(yq_merge_cmd("non_existing_file", "yq_test_files/dest.yaml"))
  expect_error(yq_merge_cmd("yq_test_files/src.yaml", "non_existing_file"))
  expect_error(yq_merge_cmd("yq_test_files/src.yaml", "yq_test_files/dest.yaml", yq_binary = "non_existing"))
})
