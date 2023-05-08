.default_to_local_config_file <- function(f) {
  stringr::str_replace(f, "\\.default\\.yaml$", "\\.yaml")
}

#' @title Update a local YAML config file using a default one.
#' @description By update is meant the following: if a default config contains a new parameter,
#' it will be appended to a local config.
#' If a parameter is present in both configs, its value from the local config will be used.
#' In case the local config does not exist, the default one will be simply copied.
#' See `vignette("config", package = "scdrake")` for details.
#'
#' `update_config()` will update a single config file.
#'
#' `update_*_config()` functions update a specific config group (pipeline, single-sample, integration).
#'
#' `update_configs()` will run all `update_*_config()`.
#'
#' @param default_file A character scalar: path to the default YAML config file. Must have a `.default.yaml` extension.
#' @param yq_binary A character scalar: path to `yq` tool's binary.
#' @param use_default_structure A logical scalar: if `TRUE`, a structure (comments, order of parameters) of the
#'   default config will be used, otherwise a structure of the local config will be preserved.
#'   See description for more details.
#' @param force A logical scalar: if `TRUE`, overwrite local configs with default ones (instead of updating them).
#' @inheritParams verbose1_param
#' @return Invisibly `NULL`.
#'
#' @details Internally, the [yq](https://github.com/mikefarah/yq) tool (version 3) is used for merging of YAML files.
#'
#' @section Using default config structure:
#' Consider the following example of config update, showing also differences in the `use_default_structure` parameter:
#'
#' Default config:
#'
#' ```yaml
#' # Comment 1.
#' PARAM_1: 1
#' PARAM_2: 2
#' PARAM_3: 3
#' ```
#'
#' Local config:
#'
#' ```yaml
#' # Comment 2.
#' PARAM_2: 6
#' # Comment 3.
#' PARAM_1: 5
#' ```
#'
#' With `use_default_structure = TRUE`, the result of update will be:
#'
#' ```yaml
#' # Comment 1.
#' PARAM_1: 5
#' PARAM_2: 6
#' PARAM_3: 3
#' ```
#'
#' With `use_default_structure = FALSE`:
#'
#' ```yaml
#' # Comment 2.
#' PARAM_2: 6
#' # Comment 3.
#' PARAM_1: 5
#' PARAM_3: 3
#' ```
#'
#' **NOTE**: `yq` tool, which is used internally for merging (updating) of YAML files,
#' cannot ovewrite comments, and thus we need to use this schema to preserve local structure.
#'
#' @seealso [download_yq()], [check_yq()], [yq_merge_cmd()], `vignette("scdrake_config")`
#'
#' @examples
#' # If a scdrake project is in the current working directory.
#' \dontrun{
#' update_config("config/pipeline.default.yaml")
#' }
#'
#' @concept update_config
#' @rdname update_config
#' @export
update_config <- function(default_file,
                          use_default_structure = FALSE,
                          force = FALSE,
                          yq_binary = getOption("scdrake_yq_binary"),
                          verbose = getOption("scdrake_verbose")) {
  assert_that_(fs::file_exists(default_file), msg = "Default config file {.file {default_file}} not found.")
  check_yq(yq_binary = yq_binary, verbose = FALSE)

  assert_that_(
    stringr::str_detect(default_file, "\\.default\\.yaml$"),
    msg = "{.code default_file} does not have {.file .default.yaml} extension"
  )

  out_file <- .default_to_local_config_file(default_file)

  if (fs::file_exists(out_file) && !force) {
    verbose %&&% cli_alert_info("Updating {.file {out_file}} from {.file {default_file}}")
    out_file_backup <- glue("{out_file}.backup")
    out_file_tmp <- glue("{out_file}.tmp")
    verbose %&&% cli_alert_success("{.file {out_file}} exists -> backuping to {.file {out_file_backup}}")
    fs::file_copy(out_file, out_file_backup, overwrite = TRUE)
    yq_merge_cmd(default_file, out_file_backup, stdout = out_file_tmp, yq_binary = yq_binary, check_yq = FALSE)

    if (use_default_structure) {
      fs::file_move(out_file_tmp, out_file)
    } else {
      yq_merge_cmd(out_file_backup, out_file_tmp, stdout = out_file, yq_binary = yq_binary, check_yq = FALSE)
      fs::file_delete(out_file_tmp)
    }
  } else {
    verbose %&&%
      cli_alert_success("{.file {out_file}} does not exist or {.code force = TRUE} -> copying from {.file {default_file}}")
    fs::file_copy(default_file, out_file, overwrite = TRUE)
  }

  invisible(NULL)
}

#' @title Update a group of configs, i.e. pipeline, single-sample, or integration.
#' @param dir A character scalar:
#' - For `update_pipeline_config()`: a path to directory with `pipeline.default.yaml` file.
#' - For `update_single_sample_configs()`: a path to directory with `00_main.default.yaml`,
#'   `01_input_qc.default.yaml`, `02_norm_clustering.default.yaml`,
#'   `cluster_markers.default.yaml`, and `contrasts.default.yaml` files.
#' - For `update_integration_configs()`: a path to directory with `00_main.default.yaml`,
#'   `01_integration.default.yaml`, `02_int_clustering.default.yaml`,
#'   `cluster_markers.default.yaml`, and `contrasts.default.yaml` files.
#' @param ... Passed to `update_config()`.
#'
#' @name update_config_group
#' @concept internal
NULL

#' @inheritParams update_config_group
#'
#' @examples
#' \dontrun{
#' update_pipeline_config("config")
#' }
#'
#' @rdname update_config
#' @export
update_pipeline_config <- function(dir = getOption("scdrake_pipeline_config_dir"),
                                   verbose = getOption("scdrake_verbose"),
                                   ...) {
  fs::dir_create(dir)
  default_file <- fs::path(dir, "pipeline.default.yaml")
  default_file_name <- fs::path_file(default_file)
  fs::file_copy(
    system.file(fs::path("config/", default_file_name), package = "scdrake", mustWork = TRUE),
    default_file,
    overwrite = TRUE
  )
  verbose %&&% cli_alert_success("Updated {.file {default_file}} from the package-bundled one.")
  update_config(default_file, verbose = verbose, ...)
}

#' @title Recursively update local configs using a file glob `*.default.yaml`.
#' @param dir A character scalar: path to directory to recursively search for config files.
#' @param ... Passed to `update_config()`.
#'
#' @examples
#' \dontrun{
#' .update_configs_recursive("config/single_sample")
#' }
#'
#' @concept internal
.update_configs_recursive <- function(dir, ...) {
  assert_that_(fs::dir_exists(dir), msg = "The requested config directory {.file {dir}} was not found.")
  default_files <- fs::dir_ls(dir, glob = "*.default.yaml")
  lapply(default_files, function(default_file) {
    update_config(default_file = default_file, ...)
  })
  invisible(NULL)
}

.copy_bundled_default_configs_dir <- function(package_dir, out_dir, verbose = getOption("scdrake_verbose")) {
  fs::dir_copy(
    system.file(package_dir, package = "scdrake", mustWork = TRUE),
    fs::path(out_dir),
    overwrite = TRUE
  )
  verbose %&&% cli_alert_success("Updated default configs from the package-bundled ones.")
  invisible(NULL)
}

#' @inheritParams update_config_group
#'
#' @export
#' @rdname update_config
#'
#' @examples
#' \dontrun{
#' update_single_sample_configs("config/single_sample")
#' }
update_single_sample_configs <- function(dir = getOption("scdrake_single_sample_config_dir"),
                                         verbose = getOption("scdrake_verbose"),
                                         ...) {
  .copy_bundled_default_configs_dir("config/single_sample", dir, verbose = verbose)
  .update_configs_recursive(dir, verbose = verbose, ...)
  invisible(NULL)
}

#' @inheritParams update_config_group
#'
#' @examples
#' \dontrun{
#' update_integration_configs("config/integration")
#' }
#'
#' @rdname update_config
#' @export
update_integration_configs <- function(dir = getOption("scdrake_integration_config_dir"),
                                       verbose = getOption("scdrake_verbose"),
                                       ...) {
  .copy_bundled_default_configs_dir("config/integration", dir, verbose = verbose)
  .update_configs_recursive(dir, verbose = verbose, ...)
  invisible(NULL)
}

#' @param pipeline_config_dir Passed to `update_pipeline_config()`.
#' @param single_sample_config_dir Passed to `update_single_sample_configs()`.
#' @param integration_config_dir Passed to `update_integration_configs()`.
#' @inheritParams verbose1_param
#'
#' @export
#' @rdname update_config
update_configs <- function(pipeline_config_dir = getOption("scdrake_pipeline_config_dir"),
                           single_sample_config_dir = getOption("scdrake_single_sample_config_dir"),
                           integration_config_dir = getOption("scdrake_integration_config_dir"),
                           verbose = getOption("scdrake_verbose"),
                           ...) {
  update_pipeline_config(pipeline_config_dir, verbose = verbose, ...)
  update_single_sample_configs(single_sample_config_dir, verbose = verbose, ...)
  update_integration_configs(integration_config_dir, verbose = verbose, ...)
  verbose %&&% cli_alert_success("All configs were successfully updated.")
  invisible(NULL)
}
