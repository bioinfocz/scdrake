#' @title Check for valid `INTEGRATION_SOURCES` parameter.
#' @param integration_methods A named list.
#' @param method_names A character vector: valid names of integration methods.
#' @param params A character vector: names of valid parameters for each integration method.
#' @return Invisibly `TRUE` if all checks are valid.
.check_integration_methods <- function(integration_methods,
                                       method_names = c("uncorrected", "rescaling", "regression", "mnn"),
                                       params = c("pca_selection_method", "pca_forced_pcs", "tsne_perp", "tsne_max_iter")) {
  integration_methods <- purrr::keep(integration_methods, ~ .$name %in% method_names)

  assert_that_(
    length(integration_methods) >= 2,
    msg = str_space(
      "{.field INTEGRATION_METHODS}: must contain at least two methods.",
      "Currently there are methods: {.val {str_c(names(integration_methods), collapse = ', ')}"
    )
  )

  assert_that_(
    all(names(integration_methods) %in% method_names),
    msg = "{.field INTEGRATION_METHODS}: contains unsupported integration method(s)."
  )

  assert_that_(
    "uncorrected" %in% names(integration_methods),
    msg = "{.field INTEGRATION_METHODS}: must contain the {.val uncorrected} integration method."
  )

  other_methods <- method_names[method_names != "uncorrected"]

  assert_that_(
    any(names(integration_methods) %in% other_methods),
    msg = "{.field INTEGRATION_METHODS}: must contain at least one method other than {.val uncorrected}"
  )

  lapply(integration_methods, function(integration_method) {
    name <- integration_method$name
    integration_method <- integration_method[params]
    assert_that_(
      all(names(integration_method) %in% params),
      msg = str_space(
        "{.field INTEGRATION_METHODS} ({.val {name}}): some parameters are missing or excessing.",
        "Valid parameters are {.val {str_c(params, collapse = ', ')}}"
      )
    )
  })

  invisible(TRUE)
}

#' @rdname process_config
.process_integration_config <- function(cfg, other_variables) {
  INTEGRATION_SOURCES <- .get_dict_param(cfg$INTEGRATION_SOURCES, not_empty = FALSE, empty_to_null = TRUE)
  .check_duplicated_list_names(INTEGRATION_SOURCES, "integration")
  cfg$INTEGRATION_SOURCES <- list_names_to_values(INTEGRATION_SOURCES) %>% scdrake_list()

  INTEGRATION_METHODS <- .get_dict_param(cfg$INTEGRATION_METHODS, not_empty = TRUE)
  .check_duplicated_list_names(INTEGRATION_METHODS, "integration")
  INTEGRATION_METHODS <- list_names_to_values(INTEGRATION_METHODS)
  .check_integration_methods(INTEGRATION_METHODS)
  cfg$INTEGRATION_METHODS <- scdrake_list(INTEGRATION_METHODS)

  cfg <- .hereize_paths(cfg, c("SELECTED_MARKERS_INT_FILE", "INTEGRATION_REPORT_RMD_FILE"))
  cfg <- .paths_to_base_dir(cfg, other_variables$BASE_OUT_DIR, "INTEGRATION_BASE_OUT_DIR")
  cfg <- .paths_to_base_dir(
    cfg, cfg$INTEGRATION_BASE_OUT_DIR, c("INTEGRATION_SELECTED_MARKERS_OUT_DIR", "INTEGRATION_REPORT_HTML_FILE")
  )

  return(cfg)
}

#' @rdname process_config
.process_int_clustering_config <- function(cfg, other_variables) {
  cfg$CELL_GROUPINGS <- .get_dict_param(cfg$CELL_GROUPINGS, not_empty = FALSE, empty_to_null = FALSE)
  INT_CLUSTERING_REPORT_DIMRED_PLOTS_OTHER <- .get_dict_param(
    cfg$INT_CLUSTERING_REPORT_DIMRED_PLOTS_OTHER,
    not_empty = FALSE, empty_to_null = TRUE
  )

  if (!is_null(INT_CLUSTERING_REPORT_DIMRED_PLOTS_OTHER)) {
    INT_CLUSTERING_REPORT_DIMRED_PLOTS_OTHER <- purrr::map(
      names(INT_CLUSTERING_REPORT_DIMRED_PLOTS_OTHER),
      ~ list(name = ., label = INT_CLUSTERING_REPORT_DIMRED_PLOTS_OTHER[[.]])
    ) %>%
      set_names(names(INT_CLUSTERING_REPORT_DIMRED_PLOTS_OTHER))
  }

  cfg$INT_CLUSTERING_REPORT_DIMRED_PLOTS_OTHER <- INT_CLUSTERING_REPORT_DIMRED_PLOTS_OTHER

  cfg <- .hereize_paths(cfg, "INT_CLUSTERING_REPORT_RMD_FILE")
  cfg <- .paths_to_base_dir(cfg, other_variables$BASE_OUT_DIR, "INT_CLUSTERING_BASE_OUT_DIR")
  cfg <- .paths_to_base_dir(cfg, cfg$INT_CLUSTERING_BASE_OUT_DIR, "INT_CLUSTERING_REPORT_HTML_FILE")

  return(cfg)
}
