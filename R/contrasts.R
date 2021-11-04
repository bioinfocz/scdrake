## -- Common functions related to contrasts (differential expression).

#' @title Prepare a `tibble` with parameters for contrasts tests.
#' @param contrasts_sources A named list.
#' @param cell_data A named list: items are columns from `colData()`. This is used to assign cells to groups or blocks.
#' @return A `tibble`. *Output target*: `contrasts_params`
#'
#' @concept sc_contrasts
#' @rdname contrasts_params
#' @export
contrasts_params_fn <- function(contrasts_sources, cell_data) {
  res <- lapply(names(contrasts_sources), FUN = function(contrast_source_name) {
    contrast_source <- contrasts_sources[[contrast_source_name]]
    common_params <- contrast_source$common_params
    source_column <- contrast_source$source_column
    block_column <- common_params$block_column
    contrasts <- contrast_source$contrasts

    if (is_null(contrasts)) {
      return(NULL)
    }

    assert_column_in_cell_data(source_column, cell_data, contrast_source_name)
    groups <- cell_data[[source_column]]
    assert_that_(
      is_character(groups) || is_integer(groups),
      msg = str_space(
        "Error in contrasts config {.field CONTRASTS_SOURCES} / {.field {contrast_source_name}}:",
        "The selected {.field source_column} {.val {source_column}} is not of character or integer type."
      )
    )

    if (!is.factor(groups)) {
      groups <- factor(groups)
    }

    if (!is_null(block_column)) {
      assert_column_in_cell_data(block_column, cell_data, contrast_source_name)
      blocks <- cell_data[[block_column]]
      assert_that_(
        is_character(blocks) || is_integer(blocks),
        msg = str_space(
          "Error in contrasts config {.field CONTRASTS_SOURCES} / {.field {contrast_source_name}}:",
          "The selected block_column {.val {block_column}} is not of character or integer type."
        )
      )

      if (!is.factor(blocks)) {
        blocks <- factor(blocks)
      }
    } else {
      block_column <- NA
      blocks <- NULL
    }

    group_levels <- levels(groups)

    if (is_list(contrasts)) {
      res <- lapply(contrasts, FUN = function(contrast) {
        target <- contrast$target
        reference <- contrast$reference

        assert_that_(
          !is_null(contrast$target), !is_null(contrast$reference),
          msg = str_space(
            "Error in contrasts config parameter {.field CONTRASTS_SOURCES} / {.field {contrast_source_name}}:",
            "Some of the contrasts is missing target or reference values."
          )
        )

        assert_that_(
          target %in% group_levels, reference %in% group_levels,
          msg = str_space(
            "Error in contrasts config parameter {.field CONTRASTS_SOURCES} / {.field {contrast_source_name}}:",
            "{.val {target}} or {.val {reference}} level not found in",
            "{.val {source_column}} column of {.code cell_data}. Valid levels are: {.val str_comma(group_levels)}}"
          )
        )

        contrast$contrast_name <- contrast$name

        if (is_null(contrast$contrast_name)) {
          contrast$contrast_name <- gluec("{target}_vs_{reference}")
        }

        return(contrast)
      })
    } else if (contrasts == "all") {
      combinations <- utils::combn(group_levels, 2) %>% as.data.frame()
      res <- lapply(combinations, FUN = function(combination) {
        target <- combination[1]
        reference <- combination[2]

        list(
          target = target,
          reference = reference,
          contrast_name = gluec("{target}_vs_{reference}")
        )
      })
    } else {
      cli_abort(str_space(
        "Error in contrasts config parameter {.field CONTRASTS_SOURCES} / {.field {contrast_source_name}}:",
        "Unknown contrasts."
      ))
    }

    other_data <- list(
      source_column = source_column, groups = list(groups),
      block_column = block_column, blocks = list(blocks),
      description = contrast_source$description,
      plot_dimreds = list(common_params$plot_dimreds)
    )

    test_params_name <- c("params_t", "params_wilcox", "params_binom")
    test_type <- c("t", "wilcox", "binom")

    test_data <- lists_to_tibble(contrast_source[test_params_name]) %>%
      dplyr::mutate(test_type = .env$test_type, test_params_name = .env$test_params_name)

    res <- purrr::map(res, ~ c(., other_data)) %>%
      lists_to_tibble() %>%
      tidyr::crossing(test_type = .env$test_type) %>%
      dplyr::left_join(test_data, by = "test_type") %>%
      dplyr::mutate(
        name = contrast_source_name,
        id = str_c(.data$name, .data$test_type, .data$contrast_name, sep = "_")
      )

    return(res)
  }) %>% purrr::discard(is_null)

  res <- dplyr::bind_rows(res)

  duplicated_contrast_names <- janitor::get_dupes(res, .data$name, .data$contrast_name, .data$test_type)

  assert_that_(
    nrow(duplicated_contrast_names) == 0,
    msg = "Error in cluster_markers config parameter {.field CONTRASTS_SOURCES}: Names of contrasts must be unique."
  )

  return(res)
}

#' @description Extract heatmap-related parameters.
#' @param contrasts_params (*input target*) A `tibble`.
#' @param contrasts_test_params (*input target*) A `tibble`.
#' @return A `tibble`. *Output target*: `contrasts_heatmap_params`
#'
#' @concept sc_contrasts
#' @rdname contrasts_params
#' @export
contrasts_heatmap_params_fn <- function(contrasts_params, contrasts_test_params) {
  contrasts_params <- contrasts_params %>%
    dplyr::select(dplyr::all_of(c(
      "id", "name", "source_column", "contrast_name", "target", "reference", "description", "block_column", "groups",
      "test_dirname", "test_label", "test_params_name", "test_type", "top_n_heatmap", "top_n_wt_heatmap"
    ))
  )

  contrasts_test_params <- dplyr::select(contrasts_test_params, .data$name, .data$test_type)
  res <- dplyr::left_join(contrasts_test_params, contrasts_params, by = c("name", "test_type"))
  return(res)
}

#' @description Extract plotting-related parameters.
#' @param contrasts_params (*input target*) A `tibble`.
#' @return A `tibble`. *Output target*: `contrasts_plot_params`
#'
#' @concept sc_contrasts
#' @rdname contrasts_params
#' @export
contrasts_plot_params_fn <- function(contrasts_params) {
  contrasts_params %>%
    dplyr::distinct(name, .data$test_type, .keep_all = TRUE) %>%
    dplyr::select(dplyr::all_of(c(
      "name", "groups", "plot_dimreds", "source_column", "test_dirname", "test_label", "test_params_name", "test_type",
      "top_n_plot", "top_n_wt_plot"
    ))) %>%
    tidyr::unnest(.data$plot_dimreds)
}

#' @title Extract contrast statistics from cluster markers tests.
#' @param contrasts_raw (*input target*) A `tibble`.
#' @param contrasts_params (*input target*) A `tibble`.
#' @return A `tibble`. *Output target*: `contrasts`
#'
#' @concept sc_contrasts
#' @export
contrasts_fn <- function(contrasts_raw, contrasts_params) {
  to_keep <- c(
    "id", "name", "target", "reference", "contrast_name", "source_column",
    "groups", "block_column", "blocks", "description", "test_type", "lfc_test",
    "test_label", "test_dirname", "test_params_name"
  )

  fn <- function(test_type, name, source_column, target, reference) {
    if (test_type == "wilcox") {
      stats_field <- "auc_"
    } else {
      stats_field <- "lfc_"
    }

    res_target <- dplyr::filter(
      contrasts_raw,
      .data$name == .env$name, .data$source_column == .env$source_column,
      .data$test_type == .env$test_type, .data$group_level == .env$target
    )$markers[[1]]
    res_reference <- dplyr::filter(
      contrasts_raw,
      .data$name == .env$name, .data$source_column == .env$source_column,
      .data$test_type == .env$test_type, .data$group_level == .env$reference
    )$markers[[1]]
    res_target_vs_reference <- res_target[[str_c(stats_field, reference)]]
    res_target_vs_reference$p.value <- exp(res_target_vs_reference$log.p.value)
    res_target_vs_reference$FDR <- exp(res_target_vs_reference$log.FDR)
    res_target_vs_reference$log.p.value <- NULL
    res_target_vs_reference$log.FDR <- NULL
    stats_reference <- res_reference[, c("self.average", "self.detected")]
    colnames(stats_reference) <- glue("other.{new}", new = c("average", "detected"))

    cols_to_omit <- c(
      which(colnames(res_target) %in% c(
        "other.average", "other.detected", "Top", "p.value", "FDR", "lfc_summary", "auc_summary")
      ),
      stringr::str_which(colnames(res_target), "^lfc_|auc_", negate = FALSE)
    )

    markers <- cbind(
      stats_reference,
      res_target[, -cols_to_omit],
      res_target_vs_reference
    )

    return(markers)
  }

  contrasts_params <- contrasts_params %>%
    dplyr::mutate(
      markers = purrr::pmap(list(.data$test_type, .data$name, .data$source_column, .data$target, .data$reference), fn)
    )

  return(contrasts_params)
}

#' @title Format a `tibble` with contrast results.
#' @param contrasts (*input target*) A `tibble`.
#' @return A `tibble`. *Output target*: `contrasts_out`
#'
#' @concept sc_contrasts
#' @export
contrasts_out_fn <- function(contrasts) {
  contrasts <- contrasts %>%
    dplyr::select(dplyr::all_of(c(
      "id", "name", "target", "reference", "contrast_name", "source_column", "block_column", "lfc_test", "std_lfc",
      "markers", "test_params_name", "test_type"
    ))) %>%
    dplyr::mutate(
      markers = lapply(.data$markers, function(markers) {
        markers %>%
          as.data.frame() %>%
          dplyr::select(
            dplyr::all_of(c("ENSEMBL", "ENTREZID", "SYMBOL", "GENENAME")),
            lfc = dplyr::any_of("logFC"),
            auc = dplyr::any_of("AUC"),
            pval = .data$p.value,
            fdr = .data$FDR,
            avg_self = .data$self.average,
            avg_other = .data$other.average,
            pct_self = .data$self.detected,
            pct_other = .data$other.detected
          ) %>%
          dplyr::mutate(dplyr::across(c(.data$pct_self, .data$pct_other), scales::percent, suffix = "", accuracy = 1))
      })
    )

  return(contrasts)
}

#' @title Create a `tibble` holding contrast results and heatmap parameters.
#' @param contrasts (*input target*) A `tibble`.
#' @param contrasts_heatmap_params (*input target*) A `tibble`.
#' @param out_dir A character scalar: path to base directory for saving heatmaps.
#' @return A `tibble`. *Output target*: `contrasts_heatmaps_df`
#'
#' @concept sc_contrasts
#' @export
contrasts_heatmaps_df_fn <- function(contrasts, contrasts_heatmap_params, out_dir) {
  dplyr::left_join(
    dplyr::select(contrasts, .data$id, .data$markers),
    contrasts_heatmap_params,
    by = "id"
  ) %>%
    dplyr::mutate(
      out_file = fs::path(.env$out_dir, .data$name, .data$test_dirname, glue("heatmap_contrast_{id}.pdf"))
    )
}
