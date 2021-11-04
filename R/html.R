## -- Common functions related to RMarkdown & knitr & HTML output in general.

#' @title Render RMarkdown document for a stage of the pipeline.
#' @param rmd_file A character scalar: path to Rmd file to render.
#' @param out_html_file_name A character scalar: name of the output HTML file.
#' @param output_dir A character scalar: path to directory in which `out_html_file_name` will be created.
#'   This directory will be created if it does not exist.
#' @param css_file A character scalar: path to CSS file which will be included in the resulting HTML.
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
    params = list(css_file = css_file, drake_cache_dir = drake_cache_dir),
    envir = new.env(),
    knit_root_dir = here(),
    quiet = quiet,
    ...
  )
}

#' @title Render a dataframe-like object using [knitr::kable()] and [kableExtra::kable_styling()].
#' @param df A dataframe-like object.
#' @param bootstrap_options,full_width,position Passed to [kableExtra::kable_styling()].
#' @param ... Passed to [knitr::kable()] and [kableExtra::kable_styling()].
#' @return An object of class `kableExtra` and `knitr_kable`.
#'
#' @concept misc_html
#' @export
render_bootstrap_table <- function(df,
                                   bootstrap_options = c("striped", "hover", "condensed"),
                                   full_width = TRUE,
                                   position = "center",
                                   ...) {
  df_kable <- knitr::kable(df, ...)

  rlang::exec(
    kableExtra::kable_styling,
    kable_input = df_kable, bootstrap_options = bootstrap_options, full_width = full_width, position = position,
    ...
  )
}

#' @title Generate a Markdown header.
#'
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

#' @title Generate a HTML link (`<a></a>`).
#' @param href A character scalar: URL.
#' @param text A character scalar: text of the link.
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
  }

  return(out)
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
                                                   id = stringi::stri_rand_strings(1, 10),
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

#' @title Generate a section with dimred plots used in some RMarkdown files.
#' @param dimred_names A character vector: names of dimreds to output.
#' @param clustering_names A character vector: names of clusterings to output.
#' @param selected_markers A `tibble`.
#' @param dimred_plots_clustering A `tibble`.
#' @param dimred_plots_other_vars A `tibble`.
#' @param selected_markers_files_rel_start A character scalar: relative start of path to selected markers PDF.
#'   See the *Relative links* section in [create_a_link()].
#' @param main_header A character scalar: text of the main header.
#' @param main_header_level A numeric scalar: level of the main header.
#' @return Invisibly `NULL`.
#'
#' @concept misc_html
#' @export
generate_dimred_plots_section <- function(dimred_names, clustering_names,
                                          selected_markers, dimred_plots_clustering, dimred_plots_other_vars,
                                          selected_markers_files_rel_start = ".",
                                          main_header = "Dimensionality reduction", main_header_level = 1) {
  md_header("Dimensionality reduction", main_header_level, extra = "{.tabset}")
  dimred_header_level <- main_header_level + 1
  dimred_subheader_level <- main_header_level + 2

  invisible(lapply(dimred_names, FUN = function(dimred_name) {
    md_header(glue("{str_to_upper(dimred_name)}"), dimred_header_level, extra = "{.tabset}")
    selected_markers_filtered <- dplyr::filter(selected_markers, dimred_name == !!dimred_name)
    selected_markers_plots_files <- selected_markers_filtered$selected_markers_plots_files

    if (!is_null(selected_markers_plots_files)) {
      cat("\n\n")
      create_a_link(
        selected_markers_plots_files,
        "**Selected markers PDF**",
        href_rel_start = selected_markers_files_rel_start,
        do_cat = TRUE
      )
      cat("\n\n")
    }

    lapply(clustering_names, FUN = function(clustering_name) {
      md_header(clustering_name, dimred_subheader_level)
      plots <- dplyr::filter(
        dimred_plots_clustering, dimred_name == !!dimred_name, clustering_name == !!clustering_name
      )$plot_list
      print(patchwork::wrap_plots(plots[[1]], ncol = 2, labels = "AUTO") + patchwork::plot_annotation(tag_levels = "A"))
    })

    dimred_plots_other_filtered <- dplyr::filter(dimred_plots_other_vars, dimred_name == !!dimred_name)

    lapply(purrr::transpose(dimred_plots_other_filtered), FUN = function(row) {
      md_header(row$source_column, dimred_subheader_level)
      print(row$plot)
    })
  }))

  invisible(NULL)
}
