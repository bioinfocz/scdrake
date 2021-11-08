withr::local_options(list(
  scdrake_verbose = FALSE
))

test_that("yq tool's binary is successfully downloaded or reused", {
  skip_if(is_false(getOption("scdrake_test_download_yq")))
  skip_on_bioc()
  skip_on_cran()

  tmpfile <- fs::file_temp()
  expect_type(download_yq(destfile = tmpfile, set_as_default = FALSE, ask = FALSE, overwrite = TRUE), "character")
  expect_true(fs::file_exists(tmpfile))
  expect_invisible(check_yq(yq_binary = tmpfile))
  expect_null(download_yq(destfile = tmpfile, set_as_default = FALSE, ask = FALSE))
})

test_that("yq merge shell command works", {
  res <- yq_merge_cmd("yq_test_files/src.yaml", "yq_test_files/dest.yaml")
  expect_type(res, "character")
  expect_length(res, 1)
  expect_snapshot(res, cran = TRUE)

  tmpfile <- fs::file_temp()
  yq_merge_cmd("yq_test_files/src.yaml", "yq_test_files/dest.yaml", stdout = tmpfile)
  expect_snapshot_file(tmpfile, name = "yq_merge_cmd.yaml", cran = TRUE)

  expect_error(yq_merge_cmd("non_existing_file", "yq_test_files/dest.yaml"))
  expect_error(yq_merge_cmd("yq_test_files/src.yaml", "non_existing_file"))
  expect_error(yq_merge_cmd("yq_test_files/src.yaml", "yq_test_files/dest.yaml", yq_binary = "non_existing"))
})
