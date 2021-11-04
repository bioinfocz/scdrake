project_dir <- fs::file_temp("scdrake_test_project_") %>%
  fs::path_abs() %>%
  as.character()

withr::local_options(list(
  scdrake_yq_binary = yq_binary,
  scdrake_verbose = FALSE,
  scdrake_pipeline_config_dir = "config",
  scdrake_single_sample_config_dir = "config/single_sample",
  scdrake_integration_config_dir = "config/integration"
))

init_project(project_dir, use_rstudio = FALSE, ask = FALSE, destfile = yq_binary)

test_that("single-sample pipeline is prepared for run", {
  expect_true(run_single_sample(.dry = TRUE))
})

test_that("integration pipeline is prepared for run", {
  expect_true(run_integration(.dry = TRUE))
})
