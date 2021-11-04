#' @keywords internal
#'
#' @import patchwork
#'
#' @importFrom methods as is
#' @importFrom magrittr %>% set_rownames set_names
#' @importFrom glue glue
#' @importFrom stringr str_c str_to_upper str_subset fixed regex
#' @importFrom ggplot2 ggplot aes labs theme ggtitle guides element_text element_blank
#' @importFrom tibble tibble
#' @importFrom rlang sym arg_match is_null is_empty is_na is_character is_integer is_list is_na is_scalar_character
#' @importFrom rlang is_true is_false is_bare_numeric
#' @importFrom rlang %||% := !! !!! .data .env
#' @importFrom cli cli cli_alert_info cli_alert_danger cli_alert_success cli_alert_warning cli_code cli_ul format_error
#' @importFrom cli cli_abort
#' @importFrom here here
#' @importFrom assertthat assert_that are_equal
#'
#' @importFrom drake file_in file_out knitr_in target trigger ignore
#'
#' @importFrom S4Vectors DataFrame metadata
#' @import BiocGenerics
#' @import SummarizedExperiment
#' @import SingleCellExperiment
#'
#' @section Package options: See [get_scdrake_default_options()].
#' @section Included datasets:
#' For pipeline testing two datasets from 10x Genomics are included:
#' - [PBMC 1k](https://support.10xgenomics.com/single-cell-gene-expression/datasets/3.0.0/pbmc_1k_v3?)
#'   (v3 chemistry, Cell Ranger 3.0.0)
#' - [PBMC 3k](https://support.10xgenomics.com/single-cell-gene-expression/datasets/1.1.0/pbmc3k)
#'   (v1 chemistry, Cell Ranger 1.1.0). This dataset is used for testing of integration pipeline.
#'
#' **All credits for these datasets go to 10x Genomics.**
#' Visit <https://www.10xgenomics.com/resources/datasets> for more information.
#'
#' Directories with dataset files can be retrieved by
#' `system.file("extdata", "pbmc1k", package = "scdrake", mustWork = TRUE)` (or `"pbmc3k"`, respectively).
"_PACKAGE"

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
## usethis namespace: end
NULL
