#!/usr/bin/env Rscript

## -- A wrapper script for devtools::check()
## -- By default, pipeline tests (including those in CLI test) are skipped as they are dysfunctional during R CMD CHECK.

library(argparser)

run_check <- function(check_dir = NULL,
                      r_libs = Sys.getenv("R_LIBS_SITE", "/usr/local/lib/R/site-library:/usr/local/lib/R/library"),
                      # pkg_tmp_dir = FALSE,
                      output_dir_pipeline_tests = fs::file_temp(),
                      run_yq_download_test = FALSE,
                      run_cli_run_test = FALSE,
                      run_pipeline_tests_example_data = FALSE,
                      run_single_sample_full_pipeline_test = FALSE,
                      run_single_sample_full_sct_pipeline_test = FALSE,
                      run_integration_pipeline_test = FALSE,
                      run_vignettes_test = FALSE,
                      run_vignette_integration_test = FALSE,
                      no_clear_config_patches = FALSE,
                      ...) {
  if (is.null(check_dir)) {
    check_dir <- fs::file_temp()
  }

  cli::cli_alert_info("Output directory: {.file {check_dir}}")

  env_vars <- c(
    NOT_CRAN = "true",
    R_LIBS = r_libs,
    R_BUILD_CHECK = "TRUE",
    SCDRAKE_PKG_DIR = "",
    SCDRAKE_VERBOSE = "FALSE",
    SCDRAKE_TEST_CLI_RUN = as.character(run_cli_run_test),
    SCDRAKE_TEST_RUN_PIPELINE = as.character(run_pipeline_tests_example_data),
    SCDRAKE_TEST_RUN_PIPELINE_BASE_OUT_DIR = output_dir_pipeline_tests,
    SCDRAKE_TEST_DOWNLOAD_YQ = as.character(run_yq_download_test),
    SCDRAKE_TEST_RUN_PIPELINE_CLEAR_CONFIG_PATCHES = as.character(!no_clear_config_patches),
    SCDRAKE_TEST_RUN_PIPELINE_SINGLE_SAMPLE_FULL = as.character(run_single_sample_full_pipeline_test),
    SCDRAKE_TEST_RUN_PIPELINE_SINGLE_SAMPLE_FULL_SCT = as.character(run_single_sample_full_sct_pipeline_test),
    SCDRAKE_TEST_RUN_PIPELINE_INTEGRATION = as.character(run_integration_pipeline_test),
    SCDRAKE_TEST_RUN_PIPELINE_VIGNETTES = as.character(run_vignettes_test),
    SCDRAKE_TEST_RUN_PIPELINE_VIGNETTE_INTEGRATION = as.character(run_vignette_integration_test)
  )

  withr::local_envvar(env_vars)
  cli::cli_alert_info("Local environment variables:")
  cli::cli_ul(paste(names(env_vars), env_vars, sep = ": "))
  cli::cli_alert_info("Running {.code devtools::check()}")
  devtools::check(check_dir = check_dir, env_vars = env_vars, ...)
}

if (!exists("argv")) {
  argv <- commandArgs(trailingOnly = TRUE)
}

p <- arg_parser("Run R CMD CHECK via devtools::check(). Pipeline tests are skipped by default.", hide.opts = TRUE)
p <- add_argument(
  p,
  "--r-libs",
  "Used to set R_LIBS environment variable.",
  default = "/usr/local/lib/R/site-library:/usr/local/lib/R/library",
  short = ""
)
p <- add_argument(p, "--test-pipeline", "Perform all pipeline tests ('--test-single_sample-full' etc. will be ignored).", flag = TRUE, short = "")
p <- add_argument(p, "--output-dir-pipeline-tests", "Output directory for pipeline tests (if enabled). If '-', use R temp subdirectory.", default = "-")
p <- add_argument(p, "--test-yq-download", "Perform the test for yq tool download.", flag = TRUE, short = "")
p <- add_argument(p, "--test-cli-run", "Perform the test for CLI run command.", flag = TRUE, short = "")
p <- add_argument(p, "--test-pipeline-example-data", "Perform the pipeline test for example data.", flag = TRUE, short = "")
p <- add_argument(
  p,
  "--test-pipeline-single_sample-full",
  "Perform the test for the full single-sample pipeline. In case the test for the integration pipeline is enabled, only the target sce_final_norm_clustering will be made.",
  flag = TRUE,
  short = ""
)
p <- add_argument(p, "--test-pipeline-single_sample-full-sct", "Perform the test for the full single-sample pipeline using the SCTransform normalization.", flag = TRUE, short = "")
p <- add_argument(p, "--test-pipeline-integration", "Perform the test for the integration pipeline.", flag = TRUE, short = "")
p <- add_argument(p, "--test-vignettes", "Perform the test for the steps in the Get Started (+ Integration) vignettes.", flag = TRUE, short = "")
p <- add_argument(p, "--test-vignette-integration", "Perform the test for the steps in the Integration vignette.", flag = TRUE, short = "")
p <- add_argument(p, "--no-clear-config-patches", "Do not remove the local versions of config patches (i.e. non default YAML files).", flag = TRUE, short = "")
p <- add_argument(p, "output_dir", "Output directory for results. Also, the object returned by devtools::check() will be saved there as 'devtools-check.Rds' file.")

if (length(argv) == 0) {
  print(p)
  withr::with_options(list(show.error.messages = FALSE), stop())
}

argv <- parse_args(p, argv = argv)

if (argv$output_dir_pipeline_tests == "-" || is.null(argv$output_dir_pipeline_tests)) {
  argv$output_dir_pipeline_tests <- fs::file_temp()
}

run_test_args <- c(
  "test_pipeline_example_data", "test_pipeline_single_sample_full", "test_pipeline_single_sample_full_sct",
  "test_pipeline_integration", "test_vignettes", "test_vignette_integration"
)

if (argv$test_pipeline) {
  argv[run_test_args] <- TRUE
}

cli::cli_alert_info("CLI parameters:")
cli::cli_ul(paste(names(argv), argv, sep = ": "))

check <- run_check(
  check_dir = argv$output_dir,
  r_libs = argv[["r-libs"]],
  # pkg_tmp_dir = pkg_tmp_dir,
  output_dir_pipeline_tests = argv$output_dir_pipeline_tests,
  run_yq_download_test = argv$test_yq_download,
  run_cli_run_test = argv$test_cli_run,
  run_pipeline_tests_example_data = argv$test_pipeline_example_data,
  run_single_sample_full_pipeline_test = argv$test_pipeline_single_sample_full,
  run_single_sample_full_sct_pipeline_test = argv$test_pipeline_single_sample_full_sct,
  run_integration_pipeline_test = argv$test_pipeline_integration,
  run_vignettes_test = argv$test_vignettes,
  run_vignette_integration_test = argv$test_vignette_integration,
  no_clear_config_patches = argv$no_clear_config_patches
)
cli::cli_alert_success("{.code devtools::check()} is done")
out_file <- fs::path(argv$output_dir, "devtools-check.Rds")
saveRDS(check, out_file)
cli::cli_alert_success("Saved the output object to {.file {out_file}}")

cli::cli_alert_info("Exiting with status {.val {check$status}}")
quit(save = "no", status = check$status)
