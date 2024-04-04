cli::cli_h1("{.file tests/testthat/test-run_cli.R}")

cli_dir <- fs::path(get_tmp_dir(), glue("scdrake-cli-bin-test-{get_random_strings(1, 10)}")) %>% fs::path_abs()
withr::local_envvar(PATH = str_c(cli_dir, Sys.getenv("PATH"), sep = ":"))
withr::defer(fs::dir_delete(cli_dir), teardown_env())
cli_alert_info("Using {.file {cli_dir}} to temporarily store the CLI scripts.")

test_that("CLI installation works", {
  install_cli(dir = cli_dir, ask = FALSE)
  expect_equal(Sys.which("scdrake") %>% unname(), str_c(cli_dir, "/scdrake"))
  res <- .run_cli(args = c("-h"))
  expect_equal(res$status, 0)
  expect_true(check_cli())
})

project_dir <- fs::path(get_tmp_dir(), glue("scdrake-cli-test-{get_random_strings(1, 10)}")) %>% fs::path_abs()
withr::defer(fs::dir_delete(project_dir), teardown_env())
cli_alert_info("Using {.file {project_dir}} as a temporary project directory.")

test_that("init-project command works", {
  res <- .run_cli(args = c("-d", project_dir, "init-project"))
  expect_equal(res$status, 0)
  ## -- This is causing unpredictable problems, so just check some files.
  # expect_snapshot(list.files(project_dir, recursive = TRUE), cran = TRUE)
  expect_true(fs::dir_exists(fs::path(project_dir, "config")))
  expect_true(fs::dir_exists(fs::path(project_dir, "Rmd")))
  expect_true(fs::file_exists(fs::path(project_dir, "config/pipeline.default.yaml")))
  expect_true(fs::file_exists(fs::path(project_dir, "config/pipeline.yaml")))
  expect_true(fs::file_exists(fs::path(project_dir, "config/single_sample/01_input_qc.default.yaml")))
  expect_true(fs::file_exists(fs::path(project_dir, "config/single_sample/01_input_qc.yaml")))
  expect_true(fs::dir_exists(fs::path(project_dir, "Rmd/single_sample")))
  expect_true(fs::file_exists(fs::path(project_dir, "Rmd/single_sample/01_input_qc.Rmd")))
  expect_true(fs::file_exists(fs::path(project_dir, "renv.lock")))
  expect_true(fs::file_exists(fs::path(project_dir, "_drake_integration.R")))
  expect_true(fs::file_exists(fs::path(project_dir, "_drake_single_sample.R")))
  expect_true(fs::file_exists(fs::path(project_dir, ".here")))
})

test_that("update-project command works", {
  res <- .run_cli(args = c("-d", project_dir, "update-project"))
  expect_equal(res$status, 0)
})

test_that("download-example-data command works", {
  tmpdir <- fs::file_temp()
  res <- .run_cli(args = c("--example-data-dir", tmpdir, "download-example-data"))
  expect_equal(res$status, 0)

  pbmc1k_dir <- fs::path(tmpdir, "pbmc1k")
  expect_true(fs::dir_exists(pbmc1k_dir))
  expect_true(all(fs::file_exists(fs::path(pbmc1k_dir, c("barcodes.tsv.gz", "features.tsv.gz", "matrix.mtx.gz")))))

  pbmc3k_dir <- fs::path(tmpdir, "pbmc3k")
  expect_true(fs::dir_exists(pbmc3k_dir))
  expect_true(all(fs::file_exists(fs::path(pbmc3k_dir, c("barcodes.tsv", "genes.tsv", "matrix.mtx")))))
})

test_that("check command works", {
  res <- .run_cli(args = "check")
  expect_equal(res$status, 0)
})

skip_if(!test_env_vars$SCDRAKE_TEST_CLI_RUN)

test_that("run command works", {
  withr::with_dir(project_dir, {
    cfg <- load_pipeline_config()
    cfg$DRAKE_TARGETS <- c("config_pipeline", "config_main")
    yaml::write_yaml(cfg, "config/pipeline.yaml")
    res <- .run_cli(args = c("--pipeline-type", "single_sample", "run"))
    expect_equal(res$status, 0)
  })
})

test_that("run command returns exit code 1", {
  withr::with_dir(project_dir, {
    cfg <- load_pipeline_config()
    cfg$DRAKE_TARGETS <- c("sce_orig")
    yaml::write_yaml(cfg, "config/pipeline.yaml")
    res <- .run_cli(args = c("--pipeline-type", "single_sample", "run"))
    expect_equal(res$status, 1)
  })
})
