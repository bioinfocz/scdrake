## -- Common functions related to RMarkdown & knitr & HTML output in general.

#' @title Render RMarkdown document for a stage of the pipeline.
#' @param rmd_file A character scalar: path to Rmd file to render.
#' @param out_html_file_name A character scalar: name of the output HTML file.
#' @param output_dir A character scalar: path to directory in which `out_html_file_name` will be created.
#'   This directory will be created if it does not exist.
#' @param css_file A character scalar: path to CSS file which will be included in the resulting HTML.
#' @param params A list of RMarkdown document parameters:
#'   passed to [rmarkdown::render()] together with `css_file` and `drake_cache_dir` items.
#' @param message,warning,echo A logical scalar: passed to `knitr::opts_chunk$set()`.
#' @param quiet A logical scalar: if `TRUE`, do not be verbose during rendering. Passed to [rmarkdown::render()].
#' @param other_deps A list of symbols (unquoted): used inside [drake::plan()] to mark other target dependencies,
#'   which are not loaded ([drake::loadd()]) or read ([drake::loadd()]) inside the `rmd_file`.
#' @param drake_cache_dir A character scalar: path to `drake` cache directory.
#' @param ... Passed to [rmarkdown::render()].
#' @return Invisibly `NULL`.
#'
#' @details If you are currently working within a `scdrake` project, you can review the Rmd files in the `Rmd/` directory.
#'
#' This is a list of Rmd files currently bundled with the `scdrake` package:
#'
#' ```{r}
#' fs::dir_tree(system.file("Rmd", package = "scdrake"))
#' ```
#'
#' A short description of these Rmd files:
#'
#' - Common to single-sample and integration analyses:
#'   - Cluster markers: `common/cluster_markers.Rmd` (signpost),
#'     `common/cluster_markers_table_template.Rmd` (actual table with markers)
#'   - Contrasts: `common/cluster_markers.Rmd`, `common/cluster_markers_table_template.Rmd`
#' - Single-sample analysis:
#'   - Input QC: `single_sample/01_input_qc.Rmd`.
#'     Input data overview, empty droplets detection, cell and gene filtering visualization.
#'   - Normalization and clustering: `single_sample/02_norm_clustering.Rmd`,
#'     `single_sample/02_norm_clustering_simple.Rmd` (only clustering visualization - dimred plots).
#'     Overview of normalization and highly variable genes selection, doublet score plots, selection of principal components,
#'     clustering diagnostics, dimred plots of clusterings and other variables.
#' - Integration analysis:
#'   - Integration: `integration/01_integration.Rmd`.
#'     Overview of input datasets, highly variable genes selection, selection of principal components, dimred plots
#'     of integration, integration diagnostics.
#'   - Clustering: `integration/02_int_clustering.Rmd`.
#'     Clustering diagnostics, dimred plots of clusterings and other variables.
#'
#' All Rmd files are also accompanied with a list of config parameters and runtime information.
#'
#' @section Working with Rmd files outside of a plan:
#' Each Rmd file for a stage can be rendered (with `generate_stage_report()`, not `rmarkdown::render()` - see below), or
#' run interactively in RStudio, both outside of a `drake` plan. However, this assumes:
#'
#' - Your working directory is in the root of a `scdrake` project.
#' - For interactive usage in RStudio, set `Options -> R Markdown -> Basic -> Evaluate chunks in directory` to `"Project"`.
#'   Otherwise the working directory will be in the location of a Rmd file while running R code chunks.
#'   Also, all Rmd files are parametrized, i.e. they are referencing a `params` list, which is passed to
#'   `rmarkdown::render()` by `generate_stage_report()`. In order for code chunks to work properly,
#'   you have to create a variable named `params_` (it must be available in the global environment) containing a named list.
#'   Currently, this list must contain only a single item: `drake_cache_dir` with path to `drake` cache which will be
#'   used to load targets inside the Rmd files. Example: `params_ <- list(drake_cache_dir = ".drake_my_analysis")`
#'
#' NOTE: It is not recommended to call `rmarkdown::render()` directly on a stage's Rmd file,
#' because `generate_stage_report()` also sets some temporary options and passes a proper `params` parameter.
#'
#' @examples
#' \dontrun{
#' generate_stage_report(
#'   rmd_file = here("Rmd/single_sample/01_input_qc.Rmd"),
#'   out_html_file_name = "01_input_qc.html",
#'   output_dir = here("my_output"),
#'   drake_cache_dir = here(".drake_cache")
#' )
#' }
#'
#' @concept misc_html
#' @export
generate_stage_report <- function(rmd_file,
                                  out_html_file_name,
                                  output_dir = fs::path_dir(out_html_file_name),
                                  css_file = here("Rmd/common/stylesheet.css"),
                                  params = list(),
                                  message = TRUE,
                                  warning = TRUE,
                                  echo = TRUE,
                                  quiet = TRUE,
                                  other_deps = NULL,
                                  drake_cache_dir = here(".drake"),
                                  ...) {
  withr::defer({
    knitr::opts_chunk$restore()
    knitr::opts_knit$restore()
  })

  assert_that_(check_pandoc())

  ## -- Locally allow evaluation of R expressions inside YAML, which is also a part of Rmd files.
  withr::local_options(yaml.eval.expr = TRUE)

  css_file <- here(css_file)
  assert_that_(fs::file_exists(css_file), msg = "{.var css_file} {.file {css_file}} not found.")

  fs::dir_create(output_dir)

  knitr::opts_chunk$set(
    message = message,
    warning = warning,
    echo = echo
  )

  knitr::opts_knit$set(root.dir = here())

  rmarkdown::render(
    rmd_file,
    output_dir = output_dir,
    output_file = out_html_file_name,
    params = c(list(css_file = css_file, drake_cache_dir = drake_cache_dir), params),
    envir = new.env(),
    knit_root_dir = here(),
    quiet = quiet,
    ...
  )
}

#' @title Render a dataframe-like object using [knitr::kable()] and [kableExtra::kable_styling()].
#' @param df A dataframe-like object.
#' @param bootstrap_options,full_width,position Passed to [kableExtra::kable_styling()].
#' @param row.names Passed to [knitr::kable()].
#' @param ... Passed to [knitr::kable()] and [kableExtra::kable_styling()].
#' @return An object of class `kableExtra` and `knitr_kable`.
#'
#' @concept misc_html
#' @export
render_bootstrap_table <- function(df,
                                   bootstrap_options = c("striped", "hover", "condensed"),
                                   full_width = TRUE,
                                   position = "center",
                                   row.names = TRUE,
                                   ...) {
  df_kable <- knitr::kable(df, row.names = row.names, ...)

  rlang::exec(
    kableExtra::kable_styling,
    kable_input = df_kable, bootstrap_options = bootstrap_options, full_width = full_width, position = position,
    ...
  )
}

#' @title Generate a Markdown header.
#' @param text A character scalar: text of the header.
#' @param heading A numeric scalar: level of the header, that is, the result will be `"#"` times `heading`.
#' @param extra A character scalar: content to put after `text`, e.g. `"{.tabset}"` for tabbed heading.
#' @param do_cat A logical scalar: if `TRUE`, print (using `cat()`) the result before returning.
#' @return A character scalar.
#'
#' @concept misc_html
#' @export
md_header <- function(text, heading, extra = "", do_cat = TRUE) {
  heading <- rep("#", heading) %>%
    str_c(collapse = "") %>%
    str_space(text, extra)
  if (do_cat) {
    catg0("\n\n{heading}\n\n")
    return(invisible(heading))
  } else {
    return(heading)
  }
}

#' @title Generate a HTML link as `<a></a>` or in the form of image as `<a><img /></a>`.
#' @param href A character scalar or vector (for `create_img_link()`): URL.
#' @param text A character scalar or vector (for `create_img_link()`): text of the link.
#' @param href_rel_start A character scalar: relative start of `href`. See the section *Relative links*.
#' @param target A character scalar: `target` parameter of `<a>` tag.
#' @param do_cat A logical scalar: if `TRUE`, print (using `cat()`) the result before returning.
#' @param ... Named parameters to put in the resulting `<a>` tag.
#' @return A character scalar.
#'
#' @section Relative links:
#' Sometimes URL in the supplied `href` parameter is absolute, or not relative to the output in which will be displayed.
#' For such cases use the `href_rel_start` parameter, which allows to make `href` relative to something.
#'
#' For example, you are saving a plot to a file relative to working directory, `output/plots/plot.pdf`, and you want
#' to include link to this file in your RMarkdown output, `output/report.html`.
#' From the point of view of this HTML file, `output/plots/plot.pdf` obviously does not exist.
#' However, you can use `href_rel_start = "output"` to specify that the `href` is relative to this directory,
#' resulting in a link to `plots/plot.pdf`.
#' Thanks to [fs::path_rel()], which is used underhood, even non-children `href` can be used, e.g.
#' if HTML is located in `output/reports/report.html`, than `href_rel_start = "output/reports"` will result in a link
#' to `../plots/plot.pdf`.
#'
#' @examples
#' create_a_link("google.com", "Google")
#' # If you want to reference a file relative to HTML saved in "output/report.html"
#' create_a_link("output/plots/plot.pdf", "Link to plot", href_rel_start = "output")
#' @rdname html_links
#' @concept misc_html
#' @export
create_a_link <- function(href, text, href_rel_start = NULL, target = "_blank", do_cat = FALSE, ...) {
  other_param_names <- list(...) %>% names()
  other_param_values <- list(...) %>%
    unlist() %>%
    unname()
  other_params <- glue("{other_param_names}='{other_param_values}'") %>% str_space()

  if (!is_null(href_rel_start)) {
    href <- fs::path_rel(href, href_rel_start)
  }

  out <- gluec("<a href='{href}' target='{target}' {other_params}>{text}</a>")

  if (do_cat) {
    cat(out)
    return(invisible(out))
  }

  out
}

#' @param img_src A character scalar: path to image.
#' @param img_src_rel_start A character scalar: same as `href_rel_start`, but for the image.
#' @param img_width,img_height A character scalar: image size.
#'
#' @rdname html_links
#' @concept misc_html
#' @export
create_img_link <- function(href,
                            img_src,
                            href_rel_start = NULL,
                            img_src_rel_start = NULL,
                            img_width = "250px",
                            img_height = "",
                            target = "_blank",
                            do_cat = FALSE,
                            ...) {
  out <- purrr::map2_chr(href, img_src, function(href, img_src) {
    other_param_names <- list(...) %>% names()
    other_param_values <- list(...) %>%
      unlist() %>%
      unname()
    other_params <- glue("{other_param_names}='{other_param_values}'") %>% str_c(collapse = " ")

    if (!is_null(href_rel_start)) {
      href <- fs::path_rel(href, href_rel_start)
    }

    if (!is_null(img_src_rel_start)) {
      img_src <- fs::path_rel(img_src, img_src_rel_start)
    }

    out <- glue(str_c(
      "<a href='{href}' target='{target}' {other_params}>",
      "<img src='{img_src}' width='{img_width}' height='{img_height}' />",
      "</a>"
    )) %>% as.character()

    if (do_cat) {
      cat(out)
    }

    out
  })

  if (do_cat) {
    return(invisible(out))
  }

  out
}

#' @title Print a HTML of table collapsible by button.
#' @description The table is generated by [render_bootstrap_table()].
#' @param df A dataframe-like object.
#' @param id A character scalar: id attribute of the table. Used to match the button and collapsible content (the table).
#' @param label A character scalar: label of the button.
#' @return Invisibly `NULL`.
#'
#' @concept misc_html
#' @export
cells_per_cluster_table_collapsed_html <- function(df,
                                                   id = get_random_strings(1, 10),
                                                   label = "Show cells per cluster table") {
  catg0(
    '
<button class="btn btn-sm btn-success" type="button" data-toggle="collapse" data-target="#{id}" aria-expanded="false" aria-controls="{id}">
  {label}
</button>

<div class="collapse" id="{id}">
  <div>

  {render_bootstrap_table(df, full_width = FALSE, position = "left")}

  </div>
</div>
'
  )

  invisible(NULL)
}

#' @title Generate a section of clustering dimensionality reduction plots in an RMarkdown document.
#' @description The hierarchy is composed of tabsets:
#'
#' - Dimred (UMAP, t-SNE, PCA)
#'   - Clustering parameters (resolution, `k`), if applicable
#'
#' @param dimred_plots_clustering_files A `tibble`.
#' @param dimred_plots_clustering_united_files A `tibble`.
#' @param algorithm_category,algorithm Character vectors to filter by `dimred_plots_clustering_files` and `dimred_plots_clustering_united_files`.
#' @param rel_start_dir A character scalar: path to HTML file in which will be links to PDF files.
#'   See the *Relative links* section in [create_a_link()].
#' @param header_level An integer scalar: the first header level.
#' @return Invisibly `NULL`, Markdown elements are directly printed to stdout.
#'
#' @concept misc_html
#' @export
generate_dimred_plots_clustering_section <- function(dimred_plots_clustering_files,
                                                     dimred_plots_clustering_united_files,
                                                     algorithm_category,
                                                     algorithm,
                                                     rel_start_dir,
                                                     header_level) {
  dimred_plots_clustering_files <- dimred_plots_clustering_files %>%
    dplyr::filter(algorithm_category %in% .env$algorithm_category, algorithm %in% .env$algorithm)

  assert_that_(nrow(dimred_plots_clustering_files) > 0)

  res <- dimred_plots_clustering_files %>%
    dplyr::group_by(.data$dimred_name) %>%
    dplyr::group_map(function(data, key) {
      md_header(str_to_upper(key$dimred_name), header_level, extra = "{.tabset}")

      dimred_plots_clustering_united_files <- dimred_plots_clustering_united_files %>%
        dplyr::filter(
          algorithm_category %in% .env$algorithm_category,
          algorithm %in% .env$algorithm,
          dimred_name == key$dimred_name
        )

      n_unique_clusters <- length(unique(data$n_clusters))

      if (n_unique_clusters > 1) {
        create_a_link(
          dimred_plots_clustering_united_files$dimred_plot_out_pdf_file,
          glue("**PDF with all plots**"),
          href_rel_start = rel_start_dir,
          do_cat = TRUE
        )
      }

      cat("\n\n")

      lapply_rows(data, FUN = function(par) {
        if (n_unique_clusters > 1) {
          if (par$algorithm_category == "graph") {
            md_header(glue("r = {par$resolution}"), header_level + 1)
          } else {
            if (par$algorithm == "kbest") {
              md_header(glue("k = {par$k} (best K)"), header_level + 1)
            } else {
              md_header(glue("k = {par$k}"), header_level + 1)
            }
          }
        }

        create_img_link(
          href = par$dimred_plot_out_pdf_file,
          img_src = par$dimred_plot_out_png_file,
          href_rel_start = rel_start_dir,
          img_width = "500px",
          do_cat = TRUE
        )

        cat("\n\n")

        list()
      })
    })

  invisible(NULL)
}

#' @title Generate a section with dimred plots used in some RMarkdown files.
#' @description The hierarchy is composed of tabsets:
#'
#' - Dimred (UMAP, t-SNE, PCA)
#'   - Dimred plots of selected variables
#'
#' @param dimred_plots_other_vars_files A `tibble`.
#' @param selected_markers_plots_files A `tibble`.
#' @param dimred_plots_rel_start,selected_markers_files_rel_start
#'   A character scalar: relative start of path to directory with dimred plots or selected markers.
#'   See the *Relative links* section in [create_a_link()].
#' @param main_header A character scalar: text of the main header.
#' @param main_header_level A numeric scalar: level of the main header.
#' @return Invisibly `NULL`, Markdown elements are directly printed to stdout.
#'
#' @concept misc_html
#' @export
generate_dimred_plots_section <- function(dimred_plots_other_vars_files,
                                          selected_markers_plots_files,
                                          dimred_plots_rel_start = ".",
                                          selected_markers_files_rel_start = ".",
                                          main_header = "Dimensionality reduction",
                                          main_header_level = 1) {
  md_header(main_header, main_header_level, extra = "{.tabset}")
  dimred_header_level <- main_header_level + 1
  dimred_subheader_level <- main_header_level + 2

  dimred_plots_other_vars_files %>%
    dplyr::group_by(.data$dimred_name) %>%
    dplyr::group_map(function(data, key) {
      dimred_name <- key$dimred_name

      md_header(glue("{str_to_upper(dimred_name)}"), dimred_header_level, extra = "{.tabset}")
      if (!is_null(selected_markers_plots_files)) {
        selected_markers_plots_files <- dplyr::filter(selected_markers_plots_files, dimred_name == .env$dimred_name)

        cat("\n\n")
        create_a_link(
          selected_markers_plots_files$out_pdf_file,
          "**Selected markers PDF**",
          href_rel_start = selected_markers_files_rel_start,
          do_cat = TRUE
        )
        cat("\n\n")
      }

      lapply(purrr::transpose(data), FUN = function(row) {
        md_header(row$source_column, dimred_subheader_level)
        create_img_link(
          href = row$out_pdf_file,
          img_src = row$out_png_file,
          href_rel_start = dimred_plots_rel_start,
          img_width = "500px",
          do_cat = TRUE
        )
      })
    })

  invisible(NULL)
}

#' @title Generate a section with dimred plots used in some RMarkdown files.
#' @description The hierarchy is composed of tabsets:
#'
#' - Selected annotation reference
#'   - Dimred plots (UMAP, t-SNE, PCA)
#'
#' @param dimred_plots_cell_annotation_files A `tibble`.
#' @param cell_annotation_diagnostic_plots A `tibble` or `NULL`.
#' @param dimred_plots_rel_start,cell_annotation_diagnostic_plots_rel_start
#'   A character scalar: relative start of path to directory with dimred plots, selected markers or cell annotation
#'   diagnostic plots PDFs. See the *Relative links* section in [create_a_link()].
#' @param main_header A character scalar: text of the main header.
#' @param main_header_level A numeric scalar: level of the main header.
#' @param text A character text: additional text to be displayed on top of the section.
#' @return Invisibly `NULL`.
#'
#' @concept misc_html
#' @export
generate_cell_annotation_plots_section <- function(dimred_plots_cell_annotation_files,
                                                   cell_annotation_diagnostic_plots,
                                                   dimred_plots_rel_start = ".",
                                                   cell_annotation_diagnostic_plots_rel_start = ".",
                                                   main_header = "Cell annotation",
                                                   main_header_level = 1,
                                                   text = NULL) {
  md_header(main_header, main_header_level, extra = "{.tabset}")

  if (!is.null(text)) {
    cat(text)
  }

  cell_annotation_source_header_level <- main_header_level + 1
  dimred_header_level <- cell_annotation_source_header_level + 1

  res <- dimred_plots_cell_annotation_files %>%
    dplyr::group_by(source_column) %>%
    dplyr::group_map(function(data, key) {
      name <- data$name[1]
      label <- data$label[1]

      md_header(name, cell_annotation_source_header_level, extra = "{.tabset}")
      glue0("\n\n{label}\n\n") %>% cat()

      cell_annotation_diagnostic_plots_filtered <- dplyr::filter(cell_annotation_diagnostic_plots, name == .env$name)

      score_heatmaps_out_file <- cell_annotation_diagnostic_plots_filtered$score_heatmaps_out_file
      marker_heatmaps_out_file <- cell_annotation_diagnostic_plots_filtered$marker_heatmaps_out_file
      delta_distribution_plot_out_file <- cell_annotation_diagnostic_plots_filtered$delta_distribution_plot_out_file

      create_a_link(
        score_heatmaps_out_file,
        "Score heatmaps PDF",
        href_rel_start = cell_annotation_diagnostic_plots_rel_start,
        do_cat = TRUE
      )

      cat(" | ")

      if (!is_null(cell_annotation_diagnostic_plots_filtered$marker_heatmaps) && !is_na(cell_annotation_diagnostic_plots_filtered$marker_heatmaps)) {
        create_a_link(
          marker_heatmaps_out_file,
          "Marker heatmaps PDF",
          href_rel_start = cell_annotation_diagnostic_plots_rel_start,
          do_cat = TRUE
        )
        cat(" | ")
      }

      create_a_link(
        delta_distribution_plot_out_file,
        "Delta distribution PDF",
        href_rel_start = cell_annotation_diagnostic_plots_rel_start,
        do_cat = TRUE
      )
      cat("\n\n")

      lapply(purrr::transpose(data), FUN = function(row) {
        md_header(glue("{str_to_upper(row$dimred_name)}"), dimred_header_level, extra = "{.tabset}")

        create_img_link(
          href = row$out_pdf_file,
          img_src = row$out_png_file,
          href_rel_start = dimred_plots_rel_start,
          img_width = "500px",
          do_cat = TRUE
        )
      })
    })

  invisible(NULL)
}

#' @title Use the `{downlit}` package to generate a Markdown list of autolinked functions wrapped inside `<details>` tags.
#' @param functions A character scalar: functions to be autolinked, e.g. `scran::clusterCells()`.
#' @param do_cat A logical scalar: if `TRUE`, cat the result to stdout.
#' @return A character scalar. Invisibly if `do_cat` is `TRUE`.
#'
#' @examples
#' format_used_functions(c("scran::clusterCells()", "Seurat::DimPlot()"), do_cat = TRUE)
#'
#' @concept misc_html
#' @export
format_used_functions <- function(functions, do_cat = FALSE) {
  function_links <- purrr::map_chr(functions, ~ gluec("- `{.}`") %>% downlit::downlit_md_string() %>% stringr::str_trim())
  out <- str_line(
    "\n<details>",
    "  <summary class='used-functions'>Show used functions \u25be</summary>\n",
    "  <div class='used-functions-content'>",
    str_line(function_links),
    "\n  </div>",
    "</details>\n\n"
  )

  if (do_cat) {
    cat(out)
    return(invisible(out))
  } else {
    return(out)
  }
}
