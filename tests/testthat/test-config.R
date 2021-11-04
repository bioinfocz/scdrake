project_dir <- fs::file_temp("scdrake_test_project_") %>%
  fs::path_abs() %>%
  as.character()
withr::defer(fs::dir_delete(project_dir), teardown_env())

withr::local_options(list(
  scdrake_verbose = FALSE,
  scdrake_pipeline_config_dir = "config",
  scdrake_single_sample_config_dir = "config/single_sample",
  scdrake_integration_config_dir = "config/integration"
))

fs::dir_create(project_dir)
fs::dir_copy(
  system.file("config", package = "scdrake", mustWork = TRUE),
  fs::path(project_dir, "config")
)

test_that("local YAML configs are created and updated", {
  withr::local_dir(project_dir)

  expect_error(update_configs(yq_binary = "non_existing"))

  ## -- Local configs are created.
  update_configs()
  expect_snapshot(list.files(recursive = TRUE), cran = TRUE)

  ## -- Local configs are backuped and updated.
  update_configs()
  expect_snapshot(list.files(recursive = TRUE), cran = TRUE)
})

test_that("parameters in a YAML file starting with '!code ' are evaluated", {
  yaml_test_1_file <- test_path("config_test_files/config_test_1.yaml")
  yaml_test_2_file <- test_path("config_test_files/config_test_2.yaml")
  yaml_test_3_file <- test_path("config_test_files/config_test_3.yaml")

  yaml_1 <- load_config(yaml_test_1_file)

  expect_snapshot_value(yaml_1, style = "serialize", cran = TRUE)
  expect_snapshot_value(
    load_config(yaml_test_2_file, other_variables = yaml_1),
    style = "serialize",
    cran = TRUE
  )
  expect_error(load_config(yaml_test_3_file))
})

test_that("default single-sample configs could be loaded", {
  withr::local_dir(project_dir)

  cfg_pipeline <- load_pipeline_config()
  cfg_ss <- load_single_sample_configs(cfg_pipeline = cfg_pipeline)

  for (cfg in c(list(cfg_pipeline), cfg_ss)) {
    expect_s3_class(cfg, "scdrake_list")
    expect_false(is_empty(cfg))
  }
})

test_that("default integration configs could be loaded", {
  withr::local_dir(project_dir)

  cfg_pipeline <- load_pipeline_config()
  cfg_ss <- load_integration_configs(cfg_pipeline = cfg_pipeline)

  for (cfg in c(list(cfg_pipeline), cfg_ss)) {
    expect_s3_class(cfg, "scdrake_list")
    expect_false(is_empty(cfg))
  }
})
