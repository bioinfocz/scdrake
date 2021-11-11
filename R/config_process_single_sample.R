#' @rdname process_config
.process_input_qc_config <- function(cfg, other_variables) {
  cfg <- .hereize_paths(cfg, c("INPUT_10X_DIR", "INPUT_QC_REPORT_RMD_FILE"))
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

  cfg <- .hereize_paths(cfg, c("SELECTED_MARKERS_FILE", "NORM_CLUSTERING_REPORT_RMD_FILE", "NORM_CLUSTERING_REPORT_SIMPLE_RMD_FILE"))
  cfg <- .paths_to_base_dir(cfg, other_variables$BASE_OUT_DIR, "NORM_CLUSTERING_BASE_OUT_DIR")
  cfg <- .paths_to_base_dir(
    cfg, cfg$NORM_CLUSTERING_BASE_OUT_DIR, c(
      "NORM_CLUSTERING_SELECTED_MARKERS_OUT_DIR", "NORM_CLUSTERING_REPORT_HTML_FILE", "NORM_CLUSTERING_REPORT_SIMPLE_HTML_FILE"
    )
  )

  return(cfg)
}