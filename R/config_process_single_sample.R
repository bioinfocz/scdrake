#' @rdname process_config
.process_input_qc_config <- function(cfg, other_variables) {
  assert_that_(
    !is_null(cfg$INPUT_DATA$type), cfg$INPUT_DATA$type %in% c("cellranger", "table", "sce"),
    msg = "{.var input_data$type} must be {.field 'cellranger'}, {.field 'table'}, or {.field 'sce'}"
  )

  assert_that_(
    !is_null(cfg$INPUT_DATA$path),
    msg = "{.field INPUT_DATA$type} is not set, data cannot be loaded later."
  )
  cfg$INPUT_DATA$path <- here(cfg$INPUT_DATA$path)
  cfg <- .hereize_paths(cfg, "INPUT_QC_REPORT_RMD_FILE")
  cfg <- .paths_to_base_dir(cfg, other_variables$BASE_OUT_DIR, "INPUT_QC_BASE_OUT_DIR")
  cfg <- .paths_to_base_dir(cfg, cfg$INPUT_QC_BASE_OUT_DIR, "INPUT_QC_REPORT_HTML_FILE")

  return(cfg)
}

#' @rdname process_config
.process_norm_clustering_config <- function(cfg, other_variables) {
  if (is_empty(cfg$SCT_VARS_TO_REGRESS)) {
    cfg <- add_item_to_list(cfg, "SCT_VARS_TO_REGRESS")
  }

  cfg$CELL_GROUPINGS <- .get_dict_param(cfg$CELL_GROUPINGS, not_empty = FALSE, empty_to_null = FALSE)
  NORM_CLUSTERING_REPORT_DIMRED_PLOTS_OTHER <- .get_dict_param(
    cfg$NORM_CLUSTERING_REPORT_DIMRED_PLOTS_OTHER,
    not_empty = FALSE, empty_to_null = TRUE
  )

  if (!is_null(NORM_CLUSTERING_REPORT_DIMRED_PLOTS_OTHER)) {
    NORM_CLUSTERING_REPORT_DIMRED_PLOTS_OTHER <- purrr::map(
      names(NORM_CLUSTERING_REPORT_DIMRED_PLOTS_OTHER),
      ~ list(name = ., label = NORM_CLUSTERING_REPORT_DIMRED_PLOTS_OTHER[[.]])
    ) %>%
      set_names(names(NORM_CLUSTERING_REPORT_DIMRED_PLOTS_OTHER))
  }

  cfg$NORM_CLUSTERING_REPORT_DIMRED_PLOTS_OTHER <- NORM_CLUSTERING_REPORT_DIMRED_PLOTS_OTHER

  if (cfg$NORMALIZATION_TYPE == "scran") {
    assert_that_(
      cfg$HVG_METRIC != "sctransform",
      msg = "{.field HVG_METRIC} of {.val sctransform} can be used only if {.field NORMALIZATION_TYPE} is {.val sctransform}"
    )
  }

  cfg$CELL_ANNOTATION_SOURCES <- .prepare_cell_annotation_sources_params(
    cfg$CELL_ANNOTATION_SOURCES, cfg$CELL_ANNOTATION_SOURCES_DEFAULTS
  )

  input_files <- c("NORM_CLUSTERING_REPORT_RMD_FILE", "NORM_CLUSTERING_REPORT_SIMPLE_RMD_FILE")
  if (!is_null(cfg$SELECTED_MARKERS_FILE)) {
    input_files <- c("SELECTED_MARKERS_FILE", input_files)
  }

  cfg <- .hereize_paths(cfg, input_files)
  cfg <- .paths_to_base_dir(cfg, other_variables$BASE_OUT_DIR, "NORM_CLUSTERING_BASE_OUT_DIR")
  cfg <- .paths_to_base_dir(
    cfg, cfg$NORM_CLUSTERING_BASE_OUT_DIR, c(
      "NORM_CLUSTERING_SELECTED_MARKERS_OUT_DIR", "NORM_CLUSTERING_CELL_ANNOTATION_OUT_DIR",
      "NORM_CLUSTERING_REPORT_HTML_FILE", "NORM_CLUSTERING_REPORT_SIMPLE_HTML_FILE"
    )
  )

  return(cfg)
}
