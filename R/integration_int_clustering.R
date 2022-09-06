selected_markers_plots_int_final_fn <- function(selected_markers_plots_int_df,
                                                selected_markers_plots_int_files,
                                                integration_final_method,
                                                integration_final_method_rm_cc) {
  if (!is_null(selected_markers_plots_int_df) && !is_null(selected_markers_plots_int_files)) {
    dplyr::bind_cols(
      selected_markers_plots_int_df,
      selected_markers_plots_files = selected_markers_plots_int_files
    ) %>%
      dplyr::filter(name == .env$integration_final_method, hvg_rm_cc_genes == .env$integration_final_method_rm_cc)
  } else {
    return(NULL)
  }
}
