cli::cli_h1("{.file tests/testthat/test-plans.R}")

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
withr::local_dir(project_dir)
update_configs()

test_that("single-sample plans can be generated", {
  cfg_pipeline <- load_pipeline_config()
  cfg <- load_single_sample_configs(cfg_pipeline = cfg_pipeline)

  input_qc_plan <- get_input_qc_subplan(cfg$input_qc, cfg_pipeline, cfg$main)
  expect_s3_class(input_qc_plan, "drake_plan")

  norm_clustering_plan <- get_norm_clustering_subplan(cfg$norm_clustering, cfg_pipeline, cfg$main)
  expect_s3_class(norm_clustering_plan, "drake_plan")

  cluster_markers_plan <- get_cluster_markers_subplan(cfg$cluster_markers, cfg_pipeline, cfg$main)
  expect_s3_class(cluster_markers_plan, "drake_plan")

  contrasts_plan <- get_contrasts_subplan(cfg$contrasts, cfg_pipeline, cfg$main)
  expect_s3_class(contrasts_plan, "drake_plan")
})

test_that("a full single-sample plan can be generated", {
  cfg_pipeline <- load_pipeline_config()
  cfg <- load_single_sample_configs(cfg_pipeline = cfg_pipeline)
  single_sample_plan <- get_single_sample_plan(cfg, cfg_pipeline)
  expect_s3_class(single_sample_plan, "drake_plan")
})

test_that("integration plans can be generated", {
  cfg_pipeline <- load_pipeline_config()
  cfg <- load_integration_configs(cfg_pipeline = cfg_pipeline)

  integration_plan <- get_integration_subplan(cfg$integration, cfg_pipeline, cfg$main)
  expect_s3_class(integration_plan, "drake_plan")

  int_clustering_plan <- get_int_clustering_subplan(cfg$int_clustering, cfg_pipeline, cfg$main)
  expect_s3_class(int_clustering_plan, "drake_plan")

  cluster_markers_plan <- get_cluster_markers_subplan(cfg$cluster_markers, cfg_pipeline, cfg$main)
  expect_s3_class(cluster_markers_plan, "drake_plan")

  contrasts_plan <- get_contrasts_subplan(cfg$contrasts, cfg_pipeline, cfg$main)
  expect_s3_class(contrasts_plan, "drake_plan")
})

test_that("a full integration plan can be generated", {
  cfg_pipeline <- load_pipeline_config()
  cfg <- load_integration_configs(cfg_pipeline = cfg_pipeline)
  integration_plan <- get_integration_plan(cfg, cfg_pipeline)
  expect_s3_class(integration_plan, "drake_plan")
})
