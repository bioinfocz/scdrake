.download_raw_feature_bc_matrix <- function(url, out_dir) {
  tmp_file_dest <- fs::file_temp()
  utils::download.file(url, destfile = tmp_file_dest)
  tmp_dir <- fs::file_temp()
  utils::untar(tmp_file_dest, exdir = tmp_dir)
  files <- fs::dir_ls(tmp_dir, recurse = TRUE, type = "file")
  filenames <- fs::path_file(files)
  fs::dir_create(out_dir)
  fs::file_move(files, out_dir)
  return(fs::dir_ls(out_dir, regexp = glue("({str_c(filenames, collapse = '|')})")))
}

.show_postdownload_info <- function(dataset, url, files, out_dir) {
  cli({
    cli_alert_success("Downloaded {dataset} raw feature barcode matrix files from {.url {url}}:")
    cli_ul(files)
    cli_alert_info(
      "Now you can use the data directory in the {.field INPUT_DATA} parameter in the {.file 01_input_qc.yaml} config:"
    )
    cli_code(
      "INPUT_DATA:",
      '  type: "cellranger"',
      glue('  path: "{out_dir}"')
    )
    cli_alert_info(
      "All credits for the dataset go to 10x Genomics. Visit {.url https://www.10xgenomics.com/resources/datasets} for more information."
    )
  })
}

#' @title Download PBMC example data from 10x Genomics.
#' @description Those data are files for raw feature barcode matrices.
#' - `download_pbmc1k()`:
#'   [PBMC 1k dataset](https://www.10xgenomics.com/resources/datasets/1-k-pbm-cs-from-a-healthy-donor-v-3-chemistry-3-standard-3-0-0)
#'   (v3 chemistry, Cell Ranger 3.0.0)
#' - `download_pbmc3k()`:
#'   [PBMC 3k dataset](https://www.10xgenomics.com/resources/datasets/3-k-pbm-cs-from-a-healthy-donor-1-standard-1-1-0)
#'   (v2 chemistry, Cell Ranger 1.1.0)
#'
#' **All credits go to 10x Genomics.**
#' Visit <https://www.10xgenomics.com/resources/datasets> for more information.
#'
#' @param out_dir A character scalar: path to directory to which will be data downloaded and extracted.
#'   Will be automatically created if it does not exist.
#' @param ask A logical scalar: if `TRUE`, ask before download.
#' @inheritParams verbose1_param
#' @return A vector of downloaded files (invisibly).
#'
#' @concept download_example_data
#' @name download_example_data
NULL

#' @rdname download_example_data
#' @export
download_pbmc1k <- function(out_dir, ask = TRUE, verbose = getOption("scdrake_verbose")) {
  url <- "https://cf.10xgenomics.com/samples/cell-exp/3.0.0/pbmc_1k_v3/pbmc_1k_v3_raw_feature_bc_matrix.tar.gz"
  if (ask) {
    cli_alert_info("Going to download PBMC 1k dataset from 10x Genomics located at {.url {url}}")
    if (!.confirm_menu()) {
      cli_abort("Interrupting the download.")
    }
  }
  files <- .download_raw_feature_bc_matrix(url, out_dir)
  verbose %&&% .show_postdownload_info("PBMC 1k", url, files, out_dir)

  invisible(files)
}

#' @rdname download_example_data
#' @export
download_pbmc3k <- function(out_dir, ask = TRUE, verbose = getOption("scdrake_verbose")) {
  url <- "https://cf.10xgenomics.com/samples/cell-exp/1.1.0/pbmc3k/pbmc3k_raw_gene_bc_matrices.tar.gz"
  if (ask) {
    cli_alert_info("Going to download PBMC 3k dataset from 10x Genomics located at {.url {url}}")
    if (!.confirm_menu()) {
      cli_abort("Interrupting the download.")
    }
  }
  files <- .download_raw_feature_bc_matrix(url, out_dir)
  verbose %&&% .show_postdownload_info("PBMC 3k", url, files, out_dir)

  invisible(files)
}
