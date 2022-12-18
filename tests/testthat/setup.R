cli::cli_h1("{.file tests/testthat/setup.R}")

test_path_abs <- test_path() %>%
  fs::path_abs() %>%
  as.character()

cli_alert_info("Location of tests: {.file {test_path_abs}}")
cli_alert_info("System tmp directory: {.file {get_tmp_dir()}}")

test_env_vars_lgl <- c(
  "R_BUILD_CHECK", "SCDRAKE_TEST_CLI_RUN", "SCDRAKE_TEST_DOWNLOAD_YQ", "SCDRAKE_TEST_RUN_PIPELINE",
  "SCDRAKE_TEST_RUN_PIPELINE_VIGNETTES", "SCDRAKE_TEST_RUN_PIPELINE_KEEP_FILES", "SCDRAKE_TEST_RUN_PIPELINE_CLEAR_CONFIG_PATCHES",
  "SCDRAKE_TEST_RUN_PIPELINE_SINGLE_SAMPLE_FULL", "SCDRAKE_TEST_RUN_PIPELINE_SINGLE_SAMPLE_FULL_SCT",
  "SCDRAKE_TEST_RUN_PIPELINE_INTEGRATION", "SCDRAKE_TEST_RUN_PIPELINE_VIGNETTE_INTEGRATION"
)
test_env_vars_lgl <- purrr::map(test_env_vars_lgl, ~ get_sys_env(., type = "logical", default = FALSE, verbose = FALSE)) %>%
  set_names(test_env_vars_lgl) %>%
  scdrake_list()

test_env_vars_chr <- c("SCDRAKE_TEST_RUN_PIPELINE_BASE_OUT_DIR")
test_env_vars_chr <- purrr::map(test_env_vars_chr, ~ get_sys_env(., type = "character", default = "", verbose = FALSE)) %>%
  set_names(test_env_vars_chr) %>%
  scdrake_list()

if (test_env_vars_chr$SCDRAKE_TEST_RUN_PIPELINE_BASE_OUT_DIR == "") {
  test_env_vars_chr$SCDRAKE_TEST_RUN_PIPELINE_BASE_OUT_DIR <- fs::file_temp()
  cli_alert_info("{.envvar SCDRAKE_TEST_RUN_PIPELINE_BASE_OUT_DIR} is not set: using {.file {test_env_vars_chr$SCDRAKE_TEST_RUN_PIPELINE_BASE_OUT_DIR}}")
}

if (test_env_vars_lgl$R_BUILD_CHECK) {
  test_env_vars_chr$SCDRAKE_PKG_DIR <- ""
  cli_alert_info("{.envvar R_BUILD_CHECK} is {.val TRUE}: ignoring {.envvar SCDRAKE_PKG_DIR}")
} else {
  test_env_vars_chr$SCDRAKE_PKG_DIR <- get_sys_env("SCDRAKE_PKG_DIR", default = fs::path(test_path_abs, "../.."), type = "character", verbose = FALSE) %>%
    fs::path_abs() %>%
    as.character()
}

test_env_vars <- c(test_env_vars_lgl, test_env_vars_chr) %>%
  scdrake_list()

cli::cli_alert_info("Environment variables for tests:")
cli::cli_ul(paste(names(test_env_vars), test_env_vars, sep = ": "))

download_yq(ask = FALSE)
