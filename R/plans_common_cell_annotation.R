#' @title Get a subplan for cell annotation via `SingleR`
#' @param sce_target_name,sce_dimred_plots_target_name
#'   A character scalar: name of target representing a SCE object that will be used for cell annotation or dimred plots.
#' @param cell_annotation_sources A tibble with cell annotation parameters.
#' @param cell_annotation_out_dir A character scalar: output directory for diagnostic plots.
#' @param report_dimred_names A character vector: dimred names for plots.
#' @param dimred_plots_out_dir A character scalar: output directory for dimred plots.
#' @param do_heatmaps_ A logical scalar: if `TRUE`, make heatmaps annotated by predicted cell types and cluster memberships.
#' @return [drake::drake_plan()]
#'
#' @concept get_subplan_cell_annotation
#' @export
get_cell_annotation_subplan <- function(sce_target_name,
                                        sce_dimred_plots_target_name,
                                        cell_annotation_sources,
                                        cell_annotation_out_dir,
                                        report_dimred_names,
                                        dimred_plots_out_dir,
                                        spatial,
                                        do_heatmaps_) {
  if (!is.null(cell_annotation_sources)) {
    plan_cell_annotation <- drake::drake_plan(
      cell_annotation_params = cell_annotation_params_fn(
        !!cell_annotation_sources
        # biomart_dataset = !!cfg_main$BIOMART_DATASET
      ),
      cell_annotation = target(
        cell_annotation_fn(cell_annotation_params, !!sym(sce_target_name), BPPARAM = ignore(BiocParallel::bpparam())),

        dynamic = map(cell_annotation_params)
      ),
      cell_annotation_labels = cell_annotation_labels_fn(cell_annotation),
      cell_annotation_diagnostic_plots = cell_annotation_diagnostic_plots_fn(
        cell_annotation = cell_annotation,
        cell_data = cell_data,
        sce = !!sym(sce_target_name),
        base_out_dir = !!cell_annotation_out_dir,
        cluster_cols_regex = "^cluster_",
        do_heatmaps = !!do_heatmaps_
      ),
      cell_annotation_diagnostic_plots_files = target(
        cell_annotation_diagnostic_plots_files_fn(cell_annotation_diagnostic_plots),

        dynamic = map(cell_annotation_diagnostic_plots),
        format = "file"
      ),
      dimred_plots_cell_annotation_params_df = dimred_plots_cell_annotation_params_df_fn(
        dimred_names = !!report_dimred_names,
        cell_annotation_params = cell_annotation_params,
        out_dir = !!dimred_plots_out_dir
      ),
      dimred_plots_cell_annotation = target(
        dimred_plots_from_params_df(
          !!sym(sce_dimred_plots_target_name),
          dimred_plots_params_df = dimred_plots_cell_annotation_params_df,
          spatial= !!spatial
        ),

        dynamic = map(dimred_plots_cell_annotation_params_df)
      ),
      dimred_plots_cell_annotation_files = dplyr::select(dimred_plots_cell_annotation, -plot),
      dimred_plots_cell_annotation_out = target(
        c(dimred_plots_cell_annotation_files$out_pdf_file, dimred_plots_cell_annotation_files$out_png_file),

        format = "file"
      )
    )
  } else {
    plan_cell_annotation <- drake::drake_plan(
      cell_annotation_params = NULL,
      cell_annotation = NULL,
      cell_annotation_labels = NULL,
      cell_annotation_diagnostic_plots = NULL,
      cell_annotation_diagnostic_plots_files = NULL,
      dimred_plots_cell_annotation_params_df = NULL,
      dimred_plots_cell_annotation = NULL,
      dimred_plots_cell_annotation_files = NULL,
      dimred_plots_cell_annotation_out = NULL
    )
  }

  plan_cell_annotation
}
