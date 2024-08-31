#!/usr/bin/env Rscript

## -- A wrapper for devtools::test()
## -- The pipeline output can be persistently saved to a directory.

library(argparser)
library(magrittr)
library(glue)

run_tests <- function(output_dir_pipeline_tests = NULL,
                      scdrake_pkg_dir = NULL,
                      skip_yq_download_test = FALSE,
                      skip_cli_run_test = FALSE,
                      skip_pipeline_tests_example_data = FALSE,
                      skip_single_sample_full = FALSE,
                      skip_single_sample_full_sct = FALSE,
                      skip_integration = FALSE,
                      skip_vignettes = FALSE,
                      skip_vignette_integration = FALSE,
                      no_clear_config_patches = FALSE,
                      filter = NULL) {
  env_vars <- c(
    R_BUILD_CHECK = "FALSE",
    SCDRAKE_VERBOSE = "FALSE",
    SCDRAKE_PKG_DIR = scdrake_pkg_dir,
    SCDRAKE_TEST_DOWNLOAD_YQ = as.character(!skip_yq_download_test),
    SCDRAKE_TEST_CLI_RUN = as.character(!skip_cli_run_test),
    SCDRAKE_TEST_RUN_PIPELINE = as.character(!skip_pipeline_tests_example_data),
    SCDRAKE_TEST_RUN_PIPELINE_BASE_OUT_DIR = output_dir_pipeline_tests,
    SCDRAKE_TEST_RUN_PIPELINE_KEEP_FILES = TRUE,
    SCDRAKE_TEST_RUN_PIPELINE_CLEAR_CONFIG_PATCHES = as.character(!no_clear_config_patches),
    SCDRAKE_TEST_RUN_PIPELINE_SINGLE_SAMPLE_FULL = as.character(!skip_single_sample_full),
    SCDRAKE_TEST_RUN_PIPELINE_SINGLE_SAMPLE_FULL_SCT = as.character(!skip_single_sample_full_sct),
    SCDRAKE_TEST_RUN_PIPELINE_INTEGRATION = as.character(!skip_integration),
    SCDRAKE_TEST_RUN_PIPELINE_VIGNETTES = as.character(!skip_vignettes),
    SCDRAKE_TEST_RUN_PIPELINE_VIGNETTE_INTEGRATION = as.character(!skip_vignette_integration)
  )

  cli::cli_alert_info("Local environment variables:")
  cli::cli_ul(paste(names(env_vars), env_vars, sep = ": "))
  if (is.null(filter)) {
    call_msg <- glue("devtools::test(filter = NULL)")
  } else {
    call_msg <- glue("devtools::test(filter = '{filter}')")
  }

  test_files <- fs::dir_ls("tests/testthat", glob = "*/test-*", type = "file")

  if (is.null(filter)) {
    matching_files <- test_files
  } else {
    test_files <- test_files %>%
      fs::path_file() %>%
      stringr::str_remove("^test-") %>%
      stringr::str_remove("\\.R$")
    matching_files <- stringr::str_subset(test_files, filter)
    matching_files <- glue("{{.file tests/testthat/test-{matching_files}.R}}")
  }

  if (rlang::is_empty(matching_files)) {
    cli::cli_abort("No test files are matching the regexp filter {.val {filter}}")
  }

  cli::cli_alert_info("Matching test files:")
  cli::cli_ul(matching_files)

  cli::cli_alert_info("Running {.code {call_msg}}")
  withr::with_envvar(env_vars, devtools::test(filter = filter))
}

if (!exists("argv")) {
  argv <- commandArgs(trailingOnly = TRUE)
}

p <- arg_parser("Run tests for pipeline execution.", hide.opts = TRUE)
p <- add_argument(p, "--filter", "Regex filter for test filenames. If '-' (default), all files will be included. Example: 'run_pipeline$'", default = "-", short = "")
p <- add_argument(p, "--no-test-cli-run", "Skip the run command test for CLI.", flag = TRUE, short = "")
p <- add_argument(p, "--no-test-yq-download", "Skip the test for yq tool's binary download.", flag = TRUE, short = "")
p <- add_argument(p, "--no-test-pipeline", "Skip all pipeline tests ('--no-test-single_sample-full' etc. will be ignored).", flag = TRUE, short = "")
p <- add_argument(
  p,
  "--scdrake-pkg-dir",
  "For development: path to directory with scdrake package source which will be loaded with devtools. Will be converted to absolute path. If '-', ignore this parameter.",
  default = ".",
  short = ""
)
p <- add_argument(p, "--no-test-pipeline-example-data", "Skip the pipeline test for example data.", flag = TRUE, short = "")
p <- add_argument(
  p,
  "--no-test-single_sample-full",
  "Skip the test for the full single-sample pipeline. In case the test for the integration pipeline is enabled, only the target sce_final_norm_clustering will be made.",
  flag = TRUE,
  short = ""
)
p <- add_argument(p, "--no-test-single_sample-full-sct", "Skip the test for the full-single sample pipeline using the SCTransform normalization.", flag = TRUE, short = "")
p <- add_argument(p, "--no-test-integration", "Skip the test for the integration pipeline.", flag = TRUE, short = "")
p <- add_argument(p, "--no-test-vignettes", "Skip the test for the steps in the Get Started (+ Integration) vignettes", flag = TRUE, short = "")
p <- add_argument(p, "--no-test-vignette-integration", "Skip the test for the steps in the Integration vignette.", flag = TRUE, short = "")
p <- add_argument(p, "--no-clear-config-patches", "Do not remove the local versions of config patches (i.e. non default YAML files).", flag = TRUE, short = "")
p <- add_argument(p, "--output-dir", "Path to output dir for test results (Rds and CSV files). If '-' (default), do not write anything.", default = "-")
p <- add_argument(p, "--output-dir-pipeline-tests", "Output directory for pipeline tests (if enabled). If '-', use R temp subdirectory.", default = "-")

if (length(argv) == 0) {
  print(p)
  withr::with_options(list(show.error.messages = FALSE), stop())
}

argv <- parse_args(p, argv = argv)

if (argv$scdrake_pkg_dir == "-") {
  argv["scdrake_pkg_dir"] <- list(NULL)
} else {
  argv$scdrake_pkg_dir <- fs::path_abs(argv$scdrake_pkg_dir)
}

run_test_args <- c(
  "no_test_pipeline_example_data", "no_test_single_sample_full", "no_test_single_sample_full_sct",
  "no_test_integration", "no_test_vignettes", "no_test_vignette_integration"
)

if (argv$no_test_pipeline) {
  argv[run_test_args] <- TRUE
}

if (argv$filter == "-") {
  argv["filter"] <- list(NULL)
}

cli::cli_alert_info("CLI parameters:")
cli::cli_ul(paste(names(argv), argv, sep = ": "))

res <- run_tests(
  output_dir_pipeline_tests = argv$output_dir_pipeline_tests,
  scdrake_pkg_dir = argv$scdrake_pkg_dir,
  skip_yq_download_test = argv$no_test_yq_download,
  skip_cli_run_test = argv$no_test_cli_run,
  skip_pipeline_tests_example_data = argv$no_test_pipeline_example_data,
  skip_single_sample_full = argv$no_test_single_sample_full,
  skip_single_sample_full_sct = argv$no_test_single_sample_full_sct,
  skip_integration = argv$no_test_integration,
  skip_vignettes = argv$no_test_vignettes,
  skip_vignette_integration = argv$no_test_vignette_integration,
  no_clear_config_patches = argv$no_clear_config_patches,
  filter = argv$filter
) %>%
  tibble::as_tibble()
cli::cli_alert_success("{.code devtools::test()} is done")

res_summary <- dplyr::summarise(res, dplyr::across(c(failed, skipped, error, warning), sum))
cli::cli_h1("Summary test results")
print(res_summary)

output_dir <- argv$output_dir

if (output_dir != "-") {
  fs::dir_create(output_dir)

  out_file <- fs::path(output_dir, "test_results.csv")
  readr::write_csv(res, out_file)
  cli::cli_alert_success("Writing {.file {out_file}}")

  out_file <- fs::path(output_dir, "test_results_summary.csv")
  readr::write_csv(res_summary, out_file)
  cli::cli_alert_success("Writing {.file {out_file}}")

  out_file <- fs::path(output_dir, "test_results.Rds")
  saveRDS(res, out_file)
  cli::cli_alert_success("Writing {.file {out_file}}")
}

status <- dplyr::if_else(all(c(res_summary$failed, res_summary$error) == 0), 0, 1)
cli::cli_alert_info("Exiting with status {.val {status}}")
quit(save = "no", status = status)
