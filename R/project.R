.copy_drake_file <- function(file_src, dir, ask = TRUE, verbose = getOption("scdrake_verbose")) {
  file_dest <- fs::path(dir, fs::path_file(file_src))

  if (ask && fs::file_exists(file_dest)) {
    cli_alert_warning("The file {.file {file_dest}} already exists.")
    if (!.confirm_menu()) {
      cli_alert_info("Skipping...")
      return(invisible(FALSE))
    }
  }

  fs::dir_create(fs::path_dir(file_dest))
  fs::file_copy(file_src, file_dest, overwrite = TRUE)
  verbose %&&% cli_alert_success("Copied the {.file {fs::path_file(file_src)}} file.")
  invisible(TRUE)
}

copy_single_sample_drake_file <- function(dir, ask = TRUE, verbose = getOption("scdrake_verbose")) {
  .copy_drake_file(system.file("_drake_single_sample.R", package = "scdrake", mustWork = TRUE), dir, ask = ask, verbose = verbose)
  invisible(TRUE)
}

copy_integration_drake_file <- function(dir, ask = TRUE, verbose = getOption("scdrake_verbose")) {
  .copy_drake_file(system.file("_drake_integration.R", package = "scdrake", mustWork = TRUE), dir, ask = ask, verbose = verbose)
  invisible(TRUE)
}

#' @title Initialize a new `scdrake` project.
#' @param dir A character scalar: path to directory in which the project will be created.
#' If `NULL`, the current working directory will be used (same as the default: `"."`). Subdirectories will be created recursively.
#' @param use_rstudio A logical scalar: if `TRUE`, initiate an RStudio project inside `dir`.
#' @param set_active_project A logical scalar: if `TRUE`, set active project to `dir`.
#' @param set_wd A logical scalar: if `TRUE`, change the current working directory to `dir`.
#' @param download_example_data A logical scalar: if `TRUE`, download example PBMC 1k and 3k data to `{dir}/example_data`.
#'   See [download_pbmc1k()] and [download_pbmc3k()] for more details on these datasets.
#' @param ask A logical scalar: if `TRUE`, do not prompt and overwrite an existing project.
#' @inheritParams verbose
#' @param ... Passed to `download_yq()`.
#'
#' @details
#' This function will:
#'
#' - Create the project's root directory.
#' - (Optional) Switch the current working directory to the project's root and call [here::i_am()].
#' - Copy config, RMarkdown, and `_drake_*.R` files.
#' - Create an empty `.here` file. This ensures that the [here](https://here.r-lib.org/) package will be able to find
#'   the project's root directory in case an RStudio project is not initialized.
#' - Check whether the `yq` tool is available in `PATH` environment variable.
#' - (Optional) Download the example data using [download_pbmc1k()] and [download_pbmc3k()].
#' - (Optional) Initialize an RStudio project and set it as the active project.
#'
#' @examples
#' \dontrun{
#' init_project("my_project")
#' }
#'
#' @export
init_project <- function(dir = ".",
                         use_rstudio = TRUE,
                         set_active_project = TRUE,
                         set_wd = TRUE,
                         download_example_data = FALSE,
                         ask = TRUE,
                         verbose = getOption("scdrake_verbose"),
                         ...) {
  verbose %&&% cli::cli_h1("Going to initialize a new {.pkg scdrake} project")

  if (dir == "." || is_null(dir)) {
    dir_is_wd <- TRUE
    dir <- getwd()
    verbose %&&% cli_alert_info("Using the current working directory: {.file {dir}}")
    ## -- This file list could be extended in the future.
    project_files <- fs::path(dir, c("config", "Rmd"))
    want_continue <- any(fs::file_exists(project_files)) && ask
    msg <- "Some project files already exists: {.file {project_files}}"
  } else {
    dir_is_wd <- FALSE
    dir <- fs::path_abs(dir)
    want_continue <- fs::dir_exists(dir) && ask
    msg <- "The project's root directory already exists: {.file {dir}}"
  }

  if (want_continue) {
    cli_alert_warning(msg)
    if (!.confirm_menu()) {
      cli_abort("Interrupting the project initialization.")
    }
  }

  if (!dir_is_wd) {
    verbose %&&% cli_alert_success("Creating the project's root directory: {.file {dir}}")
    fs::dir_create(dir, recurse = TRUE)
  }

  verbose %&&% cli_alert_success(
    "Creating an empty {.file .here} file for consistent, project-relative navigation with {.pkg here} package."
  )
  fs::file_touch(fs::path(dir, ".here"))

  verbose %&&% cli_alert_success("Copying the files.")
  for (out_dir in c("config", "Rmd")) {
    fs::dir_copy(
      system.file(out_dir, package = "scdrake", mustWork = TRUE),
      fs::path(dir, out_dir),
      overwrite = TRUE
    )
  }
  fs::file_copy(system.file("renv.lock", package = "scdrake", mustWork = TRUE), dir, overwrite = TRUE)

  copy_single_sample_drake_file(dir, ask = FALSE, verbose = verbose)
  copy_integration_drake_file(dir, ask = FALSE, verbose = verbose)
  fs::file_copy(system.file("plan_custom.R", package = "scdrake", mustWork = TRUE), dir, overwrite = TRUE)
  verbose %&&% cli_alert_success("Copied the {.file plan_custom.R} file.")

  if (set_wd) {
    verbose %&&% cli_alert_success("Changing the working directory and project's root for {.pkg here} package.")
    setwd(dir)
    here::i_am(".here")
  }

  if (use_rstudio) {
    usethis::create_project(dir, rstudio = TRUE, open = FALSE)
    if (is_empty(fs::dir_ls(fs::path(dir, "R"), all = TRUE))) {
      fs::dir_delete(fs::path(dir, "R"))
    }
    # fs::file_delete(fs::path(dir, ".gitignore"))
  }

  download_yq(verbose = verbose, ask = ask, do_check = TRUE, ...)

  verbose %&&% cli_alert_info("Updating configs.")
  update_configs(
    pipeline_config_dir = fs::path(dir, "config"),
    single_sample_config_dir = fs::path(dir, "config/single_sample"),
    integration_config_dir = fs::path(dir, "config/integration"),
    force = FALSE,
    verbose = verbose
  )

  if (download_example_data) {
    cli_alert_info("Downloading example data.")
    download_pbmc1k(fs::path(dir, "example_data/pbmc1k"), ask = FALSE, verbose = verbose)
    download_pbmc3k(fs::path(dir, "example_data/pbmc3k"), ask = FALSE, verbose = verbose)
  }

  verbose %&&% cli::cli_h1("Done!")

  if (use_rstudio && set_active_project) {
    usethis::proj_activate(dir)
  }

  return(invisible(NULL))
}

#' @title Update `scdrake` project files.
#' @description This will **overwrite** project files by the package-bundled ones:
#' - RMarkdown documents in `Rmd/`
#' - Initial scripts for [drake::r_make()]: `_drake_single_sample.R` and `_drake_integration.R`
#' - Update default YAML configs.
#' @param dir A character scalar: path to `scdrake` project directory.
#'   If `NULL`, the current working directory will be used (same as the default: `"."`).
#' @inheritParams pipeline_config_dir
#' @inheritParams single_sample_config_dir
#' @inheritParams integration_config_dir
#' @param ask A logical scalar: if `TRUE`, ask before updating the files.
#' @inheritParams verbose
#' @return Invisibly `NULL`.
#'
#' @export
update_project <- function(dir = ".",
                           pipeline_config_dir = getOption("scdrake_pipeline_config_dir"),
                           single_sample_config_dir = getOption("scdrake_single_sample_config_dir"),
                           integration_config_dir = getOption("scdrake_integration_config_dir"),
                           ask = TRUE,
                           verbose = getOption("scdrake_verbose")) {
  if (ask) {
    cli_alert_warning("Updating the project files may overwrite your modifications.")
    if (!.confirm_menu()) {
      cli_abort("Interrupting the project update.")
    }
  }

  if (dir == "." || is_null(dir)) {
    dir <- getwd()
    verbose %&&% cli_alert_info("Project is located in the current working directory: {.file {dir}}")
  } else {
    dir <- fs::path_abs(dir)
  }

  assert_that_(fs::dir_exists(dir), msg = "Project directory {.file {dir}} doesn't exist.")

  fs::file_touch(fs::path(dir, ".here"))
  fs::dir_copy(
    system.file("Rmd", package = "scdrake", mustWork = TRUE),
    fs::path(dir, "Rmd"),
    overwrite = TRUE
  )
  verbose %&&% cli_alert_success("Copied the Rmd files.")
  fs::file_copy(system.file("renv.lock", package = "scdrake", mustWork = TRUE), dir, overwrite = TRUE)
  verbose %&&% cli_alert_success("Copied the {.file renv.lock} file.")

  copy_single_sample_drake_file(dir, ask = FALSE, verbose = verbose)
  copy_integration_drake_file(dir, ask = FALSE, verbose = verbose)
  plan_custom_path <- fs::path(dir, "plan_custom.R")
  if (fs::file_exists(plan_custom_path)) {
    plan_custom_new_file <- fs::path(dir, "plan_custom.orig.R")
    verbose %&&% cli_alert_info(
      "{.file {fs::path_file(plan_custom_path)}} exists, copying the original file as {.file {fs::path_file(plan_custom_new_file)}}"
    )
  } else {
    plan_custom_new_file <- "plan_custom.R"
  }
  fs::file_copy(system.file("plan_custom.R", package = "scdrake", mustWork = TRUE), plan_custom_new_file, overwrite = TRUE)
  verbose %&&% cli_alert_success("Copied the {.file plan_custom.R} file.")

  update_configs(
    pipeline_config_dir = pipeline_config_dir,
    single_sample_config_dir = single_sample_config_dir,
    integration_config_dir = integration_config_dir,
    force = FALSE,
    verbose = verbose
  )

  return(invisible(NULL))
}
