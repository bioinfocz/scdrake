## -- Unit testing helpers.

#' @title Apply YAML config patches to package-bundled configs.
#' @description This is used in `tests/testthat/test-run_pipeline.R` tests where some modifications of default
#' config files are needed. The patches are located in `tests/testthat/run_pipeline_config_patches`.
#'
#' Before the patches are applied, their local versions are updated from the default ones, similar to standard behaviour
#' ([update_config()]).
#' @param patches_dir A character scalar: path to directory with config patches. The files there must be named the same
#'   as the bundled ones, e.g. `02_norm_clustering.yaml`.
#' @param analysis_config_dir A character scalar: path to directory with analysis config files.
#' @inheritParams pipeline_config_dir
#' @return Invisibly `NULL`.
#'
#' @details `pipeline_config_dir` directory will be used if `pipeline.default.yaml` file is present in `patches_dir`.
#'
#' @concept internal
.apply_config_patches <- function(patches_dir,
                                  analysis_config_dir,
                                  pipeline_config_dir = getOption("scdrake_pipeline_config_dir")) {
  patch_files <- fs::dir_ls(patches_dir, glob = "*.default.yaml")

  for (patch_file in patch_files) {
    update_config(patch_file)
    patch_file_name <- fs::path_file(patch_file)
    patch_file_local_name <- .default_to_local_config_file(patch_file_name)

    if (patch_file_name == "pipeline.default.yaml") {
      out_dir <- pipeline_config_dir
    } else {
      out_dir <- analysis_config_dir
    }

    default_file <- fs::path(out_dir, patch_file_name)
    patch_file_local <- fs::path(patches_dir, patch_file_local_name)
    out_file <- fs::path(out_dir, patch_file_local_name)
    tmpfile <- fs::file_temp()

    yq_merge_cmd(default_file, patch_file_local, stdout = tmpfile)
    fs::file_copy(tmpfile, out_file, overwrite = TRUE)
  }

  invisible(NULL)
}

#' @concept internal
.run_cli <- function(command = "scdrake",
                     args = character(),
                     error_on_status = FALSE,
                     echo_cmd = TRUE,
                     echo = TRUE,
                     scdrake_pkg_dir = Sys.getenv("SCDRAKE_PKG_DIR", ""),
                     cli_verbose = FALSE,
                     verbose = FALSE,
                     ...) {
  if (!is_empty(scdrake_pkg_dir) && scdrake_pkg_dir != "") {
    args <- c("--scdrake-pkg-dir", scdrake_pkg_dir, args)
  } else {
    cli_alert_warning("{.code .run_cli()}: {.envvar SCDRAKE_PKG_DIR} is not set")
  }

  if (!cli_verbose) {
    args <- c(args, "--quiet")
  }

  args <- c(args, "--no-ask")

  res <- processx::run(command = command, args = args, error_on_status = error_on_status, echo_cmd = echo_cmd, echo = echo, ...)
  if (res$status > 0) {
    cli_alert_danger("The command has finished with exit code {.val {res$status}}")
    if (verbose) {
      cli_alert_info("STDOUT:")
      message(res$stdout)
      cli_alert_info("STDERR:")
      message(res$stderr)
    }
  }

  return(res)
}
