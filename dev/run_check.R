run_check <- function(out_dir = NULL, ...) {
  env_vars <- c(
    # NOT_CRAN = "true",
    R_LIBS = Sys.getenv("R_LIBS_USER"),
    R_BUILD_CHECK = "TRUE",
    SCDRAKE_TEST_DOWNLOAD_YQ = "FALSE",
    SCDRAKE_TEST_RUN_PIPELINE = "FALSE"
  )

  withr::local_envvar(c(R_BUILD_CHECK = "TRUE"))

  if (is.null(out_dir)) {
    out_dir <- tempfile()
  }

  message("Out dir: ", out_dir)
  list(out_dir = out_dir, res = devtools::check(check_dir = out_dir, env_vars = env_vars, ...))
}

check <- run_check("../scdrake_check_20220919-bioc3.15")
