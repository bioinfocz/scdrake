#' @title Unlist a list of length one containing a named list.
#' @description
#' This is used for some YAML parameters which need to be defined this way, otherwise they would get
#' overwritten by default values. Example of such parameter:
#'
#' ```yaml
#' PARAM:
#'   - var_1:
#'       a: 1
#'     var_2:
#'       b: 2
#' ````
#'
#' @param param A list of length one containing a named list.
#' @param not_empty A logical scalar: if `TRUE`, throw error when `param` is `NULL` or empty list.
#' @param empty_to_null A logical scalar: if `TRUE` and `param` is empty list, return `NULL`.
#' @param name A passed `param` symbol, used for error messages.
#' @return A named list, or `NULL` if `empty_to_null` is `TRUE` and the conditions are met.
#'
#' @concept internal
.get_dict_param <- function(param, not_empty = FALSE, empty_to_null = TRUE, name = deparse(substitute(param))) {
  if (not_empty) {
    assert_that_(!is_empty(param), msg = "Parameter {.var {name}} cannot be NULL.")
  }

  if (is_empty(param)) {
    if (empty_to_null) {
      return(NULL)
    } else {
      return(list())
    }
  }

  assert_that(
    length(param) == 1,
    is_list(param[[1]]),
    msg = cli({
      cli_alert_danger(
        str_space(
          "Parameter {.var {name}} must be an array of length one.",
          "Check the YAML file if this parameter is set correctly.",
          "Watch out for proper indentation. Example:"
        )
      )
      cli::cli_code(
        "PARAM:",
        "  - var_1:",
        "      a: 1",
        "    var_2:",
        "      b: 2"
      )
    })
  )

  return(param[[1]])
}

.check_duplicated_list_names <- function(l, config_name, param_name = deparse(substitute(l))) {
  assert_that_(
    anyDuplicated(names(l)) == 0,
    msg = "Error in {config_name} config parameter {.field {param_name}}: Names must be unique."
  )

  invisible(TRUE)
}

#' @title Check for correct structure of `CLUSTER_MARKERS_SOURCES` or `CONTRASTS_SOURCES`.
#' @param marker_sources A named list.
#' @param check_test_params A logical scalar: if `TRUE`, check also `params_*` for each test type.
#' @param marker_type A character scalar: `"global"` for cluster markers, `"contrast"` for contrasts.
#' @return `TRUE` invisibly if all checks are valid.
#'
#' @concept internal
.check_marker_sources <- function(marker_sources, check_test_params = FALSE, marker_type = c("global", "contrast")) {
  marker_type <- arg_match(marker_type)
  stage <- dplyr::if_else(marker_type == "global", "cluster_markers", "contrasts")
  stage_var <- dplyr::if_else(marker_type == "global", "CLUSTER_MARKERS_SOURCES", "CONTRASTS_SOURCES")

  lapply(names(marker_sources), FUN = function(marker_source_name) {
    err_msg <- function(msg) {
      cli(cli_alert_danger("Error in {stage} config parameter {stage_var}: {marker_source_name} -> {msg}"))
      return("")
    }

    marker_source <- marker_sources[[marker_source_name]]
    source_column <- marker_source$source_column
    assert_that(is_scalar_character(source_column), msg = err_msg("{.field source_column} must be a scalar character."))

    description <- marker_source$description
    assert_that(is_scalar_character(description), msg = err_msg("{.field description} must be a scalar character."))

    if (check_test_params && marker_type == "global") {
      lapply(c("params_t", "params_wilcox", "params_binom"), FUN = function(test_params_name) {
        test_params <- marker_source[[test_params_name]]

        if (test_params$pval_type != "any") {
          assert_that(
            test_params$top_n_wt_heatmap != "top",
            msg = err_msg(glue("{test_params_name}: top_n_wt_heatmap cannot be 'top' when pval_type is '{test_params$pval_type}'"))
          )

          assert_that(
            test_params$top_n_wt_plot != "top",
            msg = err_msg(glue("{test_params_name}: top_n_wt_plot cannot be 'top' when pval_type is '{test_params$pval_type}'"))
          )
        }
      })
    }
  })

  invisible(TRUE)
}

#' @title Prepare a `tibble` with parameters for cluster marker tests.
#' @param marker_source A named list.
#' @param marker_source_name A character scalar: name of a marker source.
#' @param marker_default_params A named list with default parameters (common and for tests).
#'   Obtained from `CLUSTER_MARKERS_SOURCES_DEFAULTS` or `CONTRASTS_SOURCES_DEFAULTS`.
#' @return A named list (of named lists).
#'
#' @rdname prepare_marker_source_params
#' @concept internal
.prepare_marker_source_params <- function(marker_source, marker_source_name, marker_default_params) {
  test_descriptions <- list(
    params_t = list(test_label = "t-test", test_dirname = "t_test"),
    params_wilcox = list(test_label = "Wilcox test", test_dirname = "wilcox_test"),
    params_binom = list(test_label = "Binomial test", test_dirname = "binomial_test")
  )

  test_names <- names(test_descriptions)
  marker_source$name <- marker_source_name
  common_params <- marker_source$common_params
  common_params_default <- marker_default_params$common_params

  if (is_null(common_params)) {
    common_params <- common_params_default
  } else {
    not_set_common_params_names <- setdiff(names(common_params_default), names(common_params))
    if (!is_empty(not_set_common_params_names)) {
      common_params[not_set_common_params_names] <- common_params_default[not_set_common_params_names]
    }
    common_params <- common_params[names(common_params_default)]
  }

  marker_source$common_params <- common_params

  params_per_test <- lapply(test_names, FUN = function(test_name) {
    params <- scdrake_list(marker_source[[test_name]])
    default_params <- scdrake_list(marker_default_params[[test_name]])

    params <- c(params, test_descriptions[[test_name]][c("test_label", "test_dirname")])

    if (is_null(params)) {
      return(default_params)
    }

    not_set_params_names <- setdiff(names(default_params), names(params))

    if (!is_empty(not_set_params_names)) {
      params[not_set_params_names] <- default_params[not_set_params_names]
    }

    params <- params[c(names(default_params), c("test_label", "test_dirname"))] %>% scdrake_list()

    return(params)
  }) %>%
    set_names(test_names)

  marker_source[test_names] <- params_per_test

  return(marker_source)
}

#' @param marker_sources A named list of named lists of marker sources.
#' @rdname prepare_marker_source_params
.prepare_marker_sources_params <- function(marker_sources, marker_default_params) {
  purrr::map(
    names(marker_sources),
    ~ .prepare_marker_source_params(marker_sources[[.]], marker_source_name = ., marker_default_params = marker_default_params)
  ) %>% set_names(names(marker_sources))
}

#' @title Using the `here` package, contruct paths relative to project's root directory.
#' @param cfg A named list.
#' @param paths A character vector: names of items in `cfg` with paths to modify.
#' @return A modified `cfg`.
#'
#' @concept internal
.hereize_paths <- function(cfg, paths) {
  for (path in paths) {
    cfg[[path]] <- here(get(path, cfg))
    assert_that_(!is_null(cfg[[path]]), !is_empty(cfg[[path]]), msg = "{.field {path}} must not be {.var NULL} or empty.")
  }

  return(cfg)
}

#' @title Construct paths relative to base directory.
#' @param cfg A named list.
#' @param base_out_dir A character scalar: path to base directory.
#' @param paths A character vector: names of items in `cfg` with paths to modify.
#' @return A modified `cfg`.
#'
#' @concept internal
.paths_to_base_dir <- function(cfg, base_out_dir, paths) {
  for (path in paths) {
    cfg[[path]] <- fs::path(base_out_dir, get(path, cfg)) %>% as.character()
    assert_that_(!is_null(cfg[[path]]), !is_empty(cfg[[path]]), msg = "{.field {path}} must not be {.var NULL} or empty.")
  }

  return(cfg)
}

#' @title Config processing.
#' @description These functions are responsible for checking and processing of config parameters. The most important steps are:
#' - Setting up the pipeline parameters, such as `drake` targets and cache, parallel processing, etc.
#' - Extracting a path to SQLite database file from gene annotation package.
#' - Conversion of relative paths (to project's root directory) to absolute and putting some paths under a base output directory.
#' - Checking and processing of `CLUSTER_MARKERS_SOURCES`, `CONTRASTS_SOURCES`, and `INTEGRATION_SOURCES` parameters.
#' @param cfg A named list with pipeline parameters.
#' @param other_variables A list of variables in whose environment the `yaml_file` will be evaluated,
#'   see details in [load_config()].
#' @return A modified `cfg`.
#'
#' @name process_config
#' @rdname process_config
#' @concept internal
NULL

#' @rdname process_config
.process_pipeline_config <- function(cfg) {
  ## -- Always include the configs if not all targets (NULL) are specified.
  if (!is_null(cfg$DRAKE_TARGETS)) {
    cfg$DRAKE_TARGETS <- unique(c(
      "config_pipeline",
      ## -- Single-sample configs.
      "config_input_qc", "config_norm_clustering",
      ## -- Integration configs.
      "config_integration", "config_int_clustering",
      ## -- Common configs.
      "config_main", "config_cluster_markers", "config_contrasts",
      ## -- User-specified targets.
      cfg$DRAKE_TARGETS
    ))
  }

  if (is_null(cfg$DRAKE_CACHE_DIR)) {
    cfg$DRAKE_CACHE_DIR <- ".drake"
  }

  cfg$DRAKE_CACHE_DIR <- here(cfg$DRAKE_CACHE_DIR)

  if (cfg$DRAKE_PARALLELISM == "loop") {
    cfg$DRAKE_N_JOBS <- 1L
    cfg$DRAKE_N_JOBS_PREPROCESS <- 1L
  } else {
    if ((cfg$DRAKE_PARALLELISM == "future" && !check_future_installed()) ||
      (cfg$DRAKE_PARALLELISM == "clustermq" && !check_clustermq_installed()) ||
      (cfg$DRAKE_PARALLELISM == "future" && !parallelly::supportsMulticore() && !check_future.callr_installed())) {
      cfg$DRAKE_PARALLELISM <- "loop"
      cfg$DRAKE_N_JOBS <- 1L
      cfg$DRAKE_N_JOBS_PREPROCESS <- 1L
      cli_alert_info("Setting {.pkg drake} parallelism to {.val loop}")
    }
  }

  if (cfg$DRAKE_FORMAT == "qs" && !check_qs_installed()) {
    cfg$DRAKE_FORMAT <- "rds"
    cli_alert_info("Setting {.pkg drake} storage format to {.val rds}")
  }

  cfg$DRAKE_VERBOSITY <- as.integer(cfg$DRAKE_VERBOSITY)

  return(cfg)
}

#' @rdname process_config
.process_main_config <- function(cfg) {
  ANNOTATION_PKG <- cfg$ANNOTATION_LIST[[cfg$ORGANISM]]

  assert_that(
    !is_null(ANNOTATION_PKG),
    msg = cli({
      cli_alert_danger("Organism ({.val {cfg$ORGANISM}}) not found in {.field ANNOTATION_LIST}. Valid organisms:")
      cli::cli_ul(names(cfg$ANNOTATION_LIST))
    })
  )

  if (!rlang::is_installed(ANNOTATION_PKG)) {
    cli_alert_info(str_space(
      "Annotation package {.pkg ANNOTATION_PKG} specified by {.field ANNOTATION_LIST} and {.field ORGANISM} parameters",
      "in {.file 00_main.yaml} config is not installed."
    ))
    continue <- .confirm_menu(title = "Do you want to install this package?")
    if (continue == 1L) {
      cli_alert_info('Calling {.code BiocManager::install("{ANNOTATION_PKG}")}')
      BiocManager::install(ANNOTATION_PKG, update = FALSE)
    }
  }

  withr::with_package(ANNOTATION_PKG, character.only = TRUE, quietly = TRUE, code = {
    ann_db <- get(ANNOTATION_PKG)
    cfg$ANNOTATION_DB_FILE <- ann_db$conn@dbname
  })

  cfg$ANNOTATION_PKG <- ANNOTATION_PKG

  cfg <- .hereize_paths(cfg, c("CSS_FILE", "BASE_OUT_DIR"))

  return(list(main = cfg))
}

#' @rdname process_config
.process_cluster_markers_config <- function(cfg, other_variables) {
  CLUSTER_MARKERS_SOURCES <- .get_dict_param(cfg$CLUSTER_MARKERS_SOURCES, not_empty = FALSE, empty_to_null = TRUE)
  CLUSTER_MARKERS_SOURCES_DEFAULTS <- cfg$CLUSTER_MARKERS_SOURCES_DEFAULTS
  .check_duplicated_list_names(CLUSTER_MARKERS_SOURCES, "cluster_markers", "CLUSTER_MARKERS_SOURCES")

  if (is_empty(CLUSTER_MARKERS_SOURCES)) {
    CLUSTER_MARKERS_SOURCES <- list()
  } else {
    .check_marker_sources(CLUSTER_MARKERS_SOURCES)
    assert_that_(
      all(names(CLUSTER_MARKERS_SOURCES_DEFAULTS) %in% c("COMMON_PARAMS", "PARAMS_T", "PARAMS_WILCOX", "PARAMS_BINOM")),
      msg = str_space(
        "Error in cluster_markers config parameter {.field CLUSTER_MARKERS_SOURCES_DEFAULTS}:",
        "Names must be {.val {c('COMMON_PARAMS', 'PARAMS_T', 'PARAMS_WILCOX', 'PARAMS_BINOM')}}"
      )
    )

    marker_default_params_lower <- set_names(
      CLUSTER_MARKERS_SOURCES_DEFAULTS,
      names(CLUSTER_MARKERS_SOURCES_DEFAULTS) %>% stringr::str_to_lower()
    )
    marker_default_params_lower <- purrr::map(
      marker_default_params_lower,
      ~ set_names(., names(.) %>% stringr::str_to_lower())
    )

    CLUSTER_MARKERS_SOURCES <- .prepare_marker_sources_params(CLUSTER_MARKERS_SOURCES, marker_default_params_lower)
    .check_marker_sources(CLUSTER_MARKERS_SOURCES, check_test_params = TRUE)
  }

  cfg$CLUSTER_MARKERS_SOURCES <- CLUSTER_MARKERS_SOURCES

  cfg <- .hereize_paths(cfg, c("CLUSTER_MARKERS_TABLE_TEMPLATE_RMD_FILE", "CLUSTER_MARKERS_REPORT_RMD_FILE"))
  cfg <- .paths_to_base_dir(cfg, other_variables$BASE_OUT_DIR, "CLUSTER_MARKERS_BASE_OUT_DIR")
  cfg <- .paths_to_base_dir(cfg, cfg$CLUSTER_MARKERS_BASE_OUT_DIR, c(
    "CLUSTER_MARKERS_REPORT_HTML_FILE", "CLUSTER_MARKERS_HEATMAPS_OUT_DIR",
    "CLUSTER_MARKERS_DIMRED_PLOTS_BASE_OUT_DIR", "CLUSTER_MARKERS_TABLES_OUT_DIR", "CLUSTER_MARKERS_PLOTS_BASE_OUT_DIR"
  ))

  return(cfg)
}

#' @rdname process_config
.process_contrasts_config <- function(cfg, other_variables) {
  CONTRASTS_SOURCES <- .get_dict_param(cfg$CONTRASTS_SOURCES, not_empty = FALSE, empty_to_null = TRUE)
  CONTRASTS_SOURCES_DEFAULTS <- cfg$CONTRASTS_SOURCES_DEFAULTS
  .check_duplicated_list_names(CONTRASTS_SOURCES, "contrasts", "CONTRASTS_SOURCES")

  if (is_empty(CONTRASTS_SOURCES)) {
    CONTRASTS_SOURCES <- list()
  } else {
    .check_marker_sources(CONTRASTS_SOURCES)
    assert_that_(
      all(names(CONTRASTS_SOURCES_DEFAULTS) %in% c("COMMON_PARAMS", "PARAMS_T", "PARAMS_WILCOX", "PARAMS_BINOM")),
      msg = str_space(
        "Error in contrasts config parameter {.field CONTRASTS_SOURCES_DEFAULTS}:",
        "Names must be {.val {c('COMMON_PARAMS', 'PARAMS_T', 'PARAMS_WILCOX' and 'PARAMS_BINOM'}}"
      )
    )

    marker_default_params_lower <- set_names(
      CONTRASTS_SOURCES_DEFAULTS,
      names(CONTRASTS_SOURCES_DEFAULTS) %>% stringr::str_to_lower()
    )
    marker_default_params_lower <- purrr::map(
      marker_default_params_lower,
      ~ set_names(., names(.) %>% stringr::str_to_lower())
    )

    CONTRASTS_SOURCES <- .prepare_marker_sources_params(CONTRASTS_SOURCES, marker_default_params_lower)
    .check_marker_sources(CONTRASTS_SOURCES)
  }

  cfg$CONTRASTS_SOURCES <- CONTRASTS_SOURCES

  cfg <- .hereize_paths(cfg, c("CONTRASTS_TABLE_TEMPLATE_RMD_FILE", "CONTRASTS_REPORT_RMD_FILE"))
  cfg <- .paths_to_base_dir(cfg, other_variables$BASE_OUT_DIR, "CONTRASTS_BASE_OUT_DIR")
  cfg <- .paths_to_base_dir(cfg, cfg$CONTRASTS_BASE_OUT_DIR, c(
    "CONTRASTS_REPORT_HTML_FILE", "CONTRASTS_HEATMAPS_OUT_DIR",
    "CONTRASTS_DIMRED_PLOTS_BASE_OUT_DIR", "CONTRASTS_TABLES_OUT_DIR", "CONTRASTS_PLOTS_BASE_OUT_DIR"
  ))

  return(cfg)
}

#' @param cell_annotation_sources A named list.
#' @param cell_annotation_default_params A named list with default parameters.
#'
#' @rdname process_config
.prepare_cell_annotation_sources_params <- function(cell_annotation_sources, cell_annotation_default_params) {
  cell_annotation_sources <- .get_dict_param(cell_annotation_sources, not_empty = FALSE, empty_to_null = TRUE)
  .check_duplicated_list_names(cell_annotation_sources, "norm_clustering", "CELL_ANNOTATION_SOURCES")

  if (is_empty(cell_annotation_sources)) {
    return(list())
  }

  assert_that_(
    !is_empty(cell_annotation_default_params),
    msg = str_space(
      "Error in norm_clustering config parameter {.field CELL_ANNOTATION_SOURCES_DEFAULTS}:",
      "must not be empty. You can copy this parameter from the default config."
    )
  )

  assert_that_(
    all(names(cell_annotation_default_params) %in% c("TRAIN_PARAMS", "CLASSIFY_PARAMS", "PRUNE_SCORE_PARAMS", "DIAGNOSTICS_PARAMS")),
    msg = str_space(
      "Error in norm_clustering config parameter {.field CELL_ANNOTATION_SOURCES_DEFAULTS}:",
      "Names must be {.val {c('TRAIN_PARAMS', 'CLASSIFY_PARAMS', 'PRUNE_SCORE_PARAMS', 'DIAGNOSTICS_PARAMS')}}"
    )
  )

  cell_annotation_default_params <- set_names(
    cell_annotation_default_params,
    names(cell_annotation_default_params) %>% stringr::str_to_lower()
  )
  cell_annotation_default_params <- purrr::map(
    cell_annotation_default_params,
    ~ set_names(., names(.) %>% stringr::str_to_lower())
  )

  lapply(names(cell_annotation_sources), function(cell_annotation_source_name) {
    cell_annotation_source <- scdrake_list(cell_annotation_sources[[cell_annotation_source_name]])
    cell_annotation_source$name <- cell_annotation_source_name
    label_subsets <- as.character(cell_annotation_source$label_subsets)

    if (is_empty(label_subsets)) {
      label_subsets <- NA
    } else {
      assert_that_(
        length(label_subsets) > 1,
        msg = "{.var CELL_ANNOTATION_SOURCES$label_subsets} must contain zero or two and more elements."
      )
      label_subsets <- list(label_subsets)
    }

    cell_annotation_source$label_subsets <- label_subsets

    param_list_names <- c("train_params", "classify_params", "prune_score_params", "diagnostics_params")
    param_lists <- lapply(param_list_names, function(param_list_name) {
      default_params <- scdrake_list(cell_annotation_default_params[[param_list_name]])

      if (param_list_name %in% names(cell_annotation_source)) {
        params <- cell_annotation_source[[param_list_name]]
      } else {
        return(default_params)
      }

      not_set_params_names <- setdiff(names(default_params), names(params))

      if (!is_empty(not_set_params_names)) {
        params[not_set_params_names] <- default_params[not_set_params_names]
      }

      params <- params[names(default_params)] %>% scdrake_list()

      return(params)
    }) %>%
      set_names(param_list_names)

    cell_annotation_source[param_list_names] <- param_lists

    return(cell_annotation_source)
  }) %>%
    set_names(names(cell_annotation_sources))
}
