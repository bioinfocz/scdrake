## -- Common functions related to cluster markers.

#' @title Load a list of cluster markers sources into a `tibble`.
#' @description Cluster markers sources are defined in `cluster_markers.yaml` config file in `CLUSTER_MARKERS_SOURCES` parameter.
#' @param cluster_markers_sources A list of cluster markers sources.
#' @param cell_data A list of `colData()` columns.
#' @return A `tibble`. *Output target*: `cluster_markers_params`
#'
#' @concept sc_cluster_markers
#' @export
cluster_markers_params_fn <- function(cluster_markers_sources, cell_data) {
  test_params_names <- list(params_t = "t", params_wilcox = "wilcox", params_binom = "binom")
  to_get <- c(
    "lfc_direction", "lfc_test", "pval_type", "min_prop", "std_lfc", "test_label", "test_dirname",
    "top_n_heatmap", "top_n_wt_heatmap", "top_n_plot", "top_n_wt_plot"
  )

  res <- lapply(names(cluster_markers_sources), FUN = function(cluster_marker_source_name) {
    cluster_markers_source <- cluster_markers_sources[[cluster_marker_source_name]]
    common_params <- cluster_markers_source$common_params
    source_column <- cluster_markers_source$source_column
    block_column <- common_params$block_column
    assert_column_in_cell_data(source_column, cell_data, cluster_marker_source_name)

    res_test <- lapply(names(test_params_names), FUN = function(test_params_name) {
      params <- cluster_markers_source[[test_params_name]]
      assert_that(!is_null(params))
      params <- params[to_get]
      params$test_params_name <- test_params_name
      params$test_type <- test_params_names[[test_params_name]]
      groups <- cell_data[[source_column]]
      assert_that_(
        is.factor(groups) || is_character(groups) || is_integer(groups),
        msg = str_space(
          "Error in cluster_markers config {.field CLUSTER_MARKERS_SOURCES} / {.field {cluster_marker_source_name}}:",
          "The selected source_column {.val {source_column}} is not of numeric, factor or integer type."
        )
      )
      params$groups <- list(groups)

      return(params)
    })

    params_to_add <- list(
      description = cluster_markers_source$description,
      plot_dimreds = list(common_params$plot_dimreds),
      name = cluster_marker_source_name,
      source_column = source_column
    )

    if (!is_null(block_column)) {
      assert_column_in_cell_data(block_column, cell_data, cluster_marker_source_name)
      blocks <- cell_data[[block_column]]
      assert_that_(
        is.factor(blocks) || is_character(blocks) || is_integer(blocks),
        msg = str_space(
          "Error in cluster_markers config {.field CLUSTER_MARKERS_SOURCES} / {.field {cluster_marker_source_name}}:",
          "The selected block_column {.val {block_column}} is not of numeric, factor or integer type."
        )
      )
      blocks <- list(blocks)
    } else {
      block_column <- NA
      blocks <- list(NULL)
    }

    params_to_add$block_column <- block_column
    params_to_add$blocks <- blocks

    res_test <- purrr::map(res_test, ~ c(., params_to_add)) %>%
      replace_list_nulls() %>%
      lists_to_tibble()

    return(res_test)
  })

  res <- dplyr::bind_rows(res)
  return(res)
}

#' @title Extract columns with certain parameters from cluster markers sources `tibble`.
#' @description Statistical test-related parameters.
#' @param cluster_markers_params (*input target*) A `tibble`.
#' @return A `tibble`. *Output target*: `cluster_markers_test_params`
#'
#' @concept sc_cluster_markers
#' @rdname cluster_markers_params
#' @export
cluster_markers_test_params_fn <- function(cluster_markers_params) {
  cluster_markers_params %>%
    dplyr::select(dplyr::all_of(c(
      "block_column", "blocks", "groups", "lfc_direction", "lfc_test", "min_prop", "std_lfc", "name", "pval_type", "source_column",
      "test_params_name", "test_type"
    )))
}

#' @description Plotting-related parameters.
#' @return A `tibble`. *Output target*: `cluster_markers_plot_params`
#'
#' @concept sc_cluster_markers
#' @rdname cluster_markers_params
#' @export
cluster_markers_plot_params_fn <- function(cluster_markers_params) {
  cluster_markers_params %>%
    dplyr::select(dplyr::all_of(c(
      "groups", "name", "plot_dimreds", "source_column", "test_dirname", "test_label", "test_params_name",
      "test_type", "top_n_plot", "top_n_wt_plot"
    ))) %>%
    tidyr::unnest(.data$plot_dimreds)
}

#' @description Heatmap-related parameters.
#' @return A `tibble`. *Output target*: `cluster_markers_heatmap_params`
#'
#' @concept sc_cluster_markers
#' @rdname cluster_markers_params
#' @export
cluster_markers_heatmap_params_fn <- function(cluster_markers_params) {
  cluster_markers_params %>%
    dplyr::select(dplyr::all_of(c(
      "block_column", "description", "groups", "name", "source_column", "test_dirname", "test_label",
      "test_params_name", "test_type", "top_n_heatmap", "top_n_wt_heatmap"
    )))
}

#' @title Add LFC summaries for Wilcox tests, obtained from t-test results.
#' @param cluster_markers_raw (*input target*) A `tibble`.
#' @return A `tibble`. *Output target*: `cluster_markers`
#'
#' @concept sc_cluster_markers
#' @export
cluster_markers_fn <- function(cluster_markers_raw) {
  wilcox_tests <- cluster_markers_raw$test_type == "wilcox"
  markers_names <- names(cluster_markers_raw$markers)

  wilcox_markers <- lapply(purrr::transpose(cluster_markers_raw[wilcox_tests, ]), FUN = function(row) {
    res_t_test <- cluster_markers_raw %>%
      dplyr::filter(.data$name == row$name, .data$test_type == "t", .data$group_level == row$group_level) %>%
      dplyr::pull(.data$markers) %>%
      .[[1]]

    row$markers$lfc_summary <- res_t_test$lfc_summary

    return(row$markers)
  })

  cluster_markers_raw[wilcox_tests, "markers"] <- list(wilcox_markers)
  names(cluster_markers_raw$markers) <- markers_names

  return(cluster_markers_raw)
}

#' @title Add additional summary columns of cluster markers for comparisons with other group levels.
#' @param cluster_markers (*input target*) A `tibble`.
#' @return A `tibble`. **Output target**: `cluster_markers_processed`
#'
#' @concept sc_cluster_markers
#' @export
cluster_markers_processed_fn <- function(cluster_markers) {
  cluster_markers_processed <- cluster_markers %>%
    dplyr::mutate(
      markers = purrr::pmap(
        list(.data$markers, .data$id, .data$test_type, .data$groups, .data$group_level),
        function(markers, id, test_type, groups, group_level) {
          if (test_type == "wilcox") {
            stats_columns_metric_name <- ".AUC"
            stats_columns <- str_c("auc_", setdiff(levels(groups), group_level))
          } else {
            stats_columns_metric_name <- ".logFC"
            stats_columns <- str_c("lfc_", setdiff(levels(groups), group_level))
          }

          stats_df <- markers[, stats_columns] %>% as.data.frame()

          if (length(levels(groups)) > 2) {
            stats_columns_metric <- str_c(stats_columns, stats_columns_metric_name)
          } else {
            stats_columns_metric <- if (test_type == "wilcox") "AUC" else "logFC"
          }

          markers[, stats_columns] <- stats_df[, stats_columns_metric]

          return(markers)
        }
      )
    )

  return(cluster_markers_processed)
}

#' @title Create a final `dataframe` of cluster markers.
#' @param cluster_markers_processed (*input target*) A `tibble`.
#' @return A `tibble`. *Output target*: `cluster_markers_out`
#'
#' @concept sc_cluster_markers
#' @export
cluster_markers_out_fn <- function(cluster_markers_processed) {
  cluster_markers_out <- cluster_markers_processed %>%
    dplyr::mutate(
      markers = lapply(.data$markers, function(markers) {
        markers %>%
          as.data.frame() %>%
          dplyr::select(
            dplyr::all_of(c("ENSEMBL", "ENTREZID", "SYMBOL", "GENENAME")),
            top = dplyr::any_of("Top"),
            dplyr::any_of("auc_summary"),
            .data$lfc_summary,
            pval = .data$p.value,
            fdr = .data$FDR,
            avg_self = .data$self.average,
            avg_other = .data$other.average,
            pct_self = .data$self.detected,
            pct_other = .data$other.detected,
            dplyr::matches("^lfc_|auc_")
          ) %>%
          dplyr::mutate(dplyr::across(c(.data$pct_self, .data$pct_other), scales::percent, suffix = "", accuracy = 1))
      })
    )

  return(cluster_markers_out)
}

#' @title Create a final `tibble` holding parameters for cluster markers heatmaps generation.
#' @description
#' Compared to the `cluster_markers_heatmap_params` target, the output `tibble` is expanded to each marker test type etc.
#' @param cluster_markers_processed (*input target*) A `tibble`.
#' @param cluster_markers_heatmap_params (*input target*) A `tibble`.
#' @param out_dir A character scalar: path to output directory for heatmap PDFs.
#' @return A `tibble`. *Output target*: `cluster_markers_heatmaps_df`
#'
#' @concept sc_cluster_markers
#' @export
cluster_markers_heatmaps_df_fn <- function(cluster_markers_processed, cluster_markers_heatmap_params, out_dir) {
  dplyr::left_join(
    dplyr::select(cluster_markers_processed, dplyr::all_of(c(
      "id", "name", "test_type", "markers",
      "group_level", "lfc_direction", "lfc_test", "min_prop", "pval_type"
    ))),
    cluster_markers_heatmap_params,
    by = c("name", "test_type")
  ) %>%
    dplyr::mutate(
      out_file = fs::path(.env$out_dir, .data$name, .data$test_dirname, glue("heatmap_cluster_markers_{id}.pdf"))
    )
}
