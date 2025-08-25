cli::cli_h1("{.file tests/testthat/test-project.R}")

project_dir <- fs::file_temp("scdrake_test_project_") %>%
  fs::path_abs() %>%
  as.character()
withr::defer(
  {
    fs::dir_delete(project_dir)
    ## -- This is ugly, but I didn't find an another way to reset here() to original state.
    if (!test_env_vars$R_BUILD_CHECK) {
      withr::with_dir("../..", here::i_am("DESCRIPTION"))
    }
  },
  teardown_env()
)

withr::local_options(list(
  scdrake_verbose = FALSE,
  scdrake_pipeline_config_dir = "config",
  scdrake_single_sample_config_dir = "config/single_sample",
  scdrake_integration_config_dir = "config/integration"
))

test_that("project initialization and update works", {
  init_project(project_dir, use_rstudio = FALSE, ask = FALSE)

  expect_equal(getwd() %>% fs::path_file() %>% as.character(), fs::path_file(project_dir) %>% as.character())
  expect_snapshot(list.files(recursive = TRUE), cran = TRUE)

  update_project(project_dir, ask = FALSE)
  expect_snapshot(list.files(recursive = TRUE), cran = TRUE)
})
