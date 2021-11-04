#' @title Download the binary of the `yq` tool (version 3.4.1).
#' @description This function has side-effects by default, see the `set_as_default` argument.
#'
#' @param url An URL from which binary will be downloaded.
#' If `NULL`, it will be created from `os`, `arch`, and `yq_version`.
#' @param os A character scalar specifying the current OS (`"linux"`, `"windows"`, or `"darwin"` for macOS).
#' If `NULL`, it will be determined from `.Platform$OS.type`.
#' @param arch A character scalar specifying the current CPU architecture (`32` or `64`).
#' If `NULL`, it will be determined from `.Machine$sizeof.pointer` (`4 -> 32`, `8 -> 64`).
#' @param yq_version A character scalar of `yq` tool version.
#' @param destfile A character scalar specifying the destination file.
#' @param set_as_default If `TRUE`, set `options(scdrake_yq_binary = destfile)`.
#' @param do_check If `TRUE`, functionality of the downloaded (or present) `yq` binary will be checked.
#' @param ask A logical scalar: if `TRUE`, ask before `yq` binary is downloaded.
#' @param dry A logical scalar: if `TRUE`, do not download the `yq` binary and just return its URL.
#' @param overwrite If `TRUE`, overwrite the existing `yq` binary if exists.
#' @inheritParams verbose
#' @return A character scalar: URL to `yq` binary.
#' If `!overwrite && fs::file_exists(destfile)`, then `NULL` invisibly.
#'
#' @concept yq_tool
#' @export
download_yq <- function(url = NULL,
                        os = NULL,
                        arch = NULL,
                        yq_version = "3.4.1",
                        destfile = fs::path(fs::path_home(), ".local/bin/yq"),
                        set_as_default = TRUE,
                        do_check = TRUE,
                        ask = TRUE,
                        overwrite = FALSE,
                        dry = FALSE,
                        verbose = getOption("scdrake_verbose")) {
  if (!overwrite && fs::file_exists(destfile)) {
    cli_alert_info("{.file {destfile}} exists and {.code overwrite = FALSE} -> skipping the download")
    if (do_check) {
      check_yq(yq_binary = destfile, repair_executable = TRUE, verbose = TRUE)
    }
    return(invisible(NULL))
  }

  if (is_null(url)) {
    os <- os %||% .get_os()

    valid_os <- c("linux", "windows", "darwin")
    assert_that_(
      os %in% valid_os,
      msg = "Unsupported os: '{os}'. The valid ones are {str_comma(valid_os)}"
    )

    valid_arch <- c(32L, 64L)
    if (is_null(arch)) {
      pointer_size <- .Machine$sizeof.pointer

      arch <- dplyr::case_when(
        pointer_size == 4 ~ 32L,
        pointer_size == 8 ~ 64L,
        TRUE ~ NA_integer_
      )

      assert_that_(
        !is_na(arch),
        msg = "Cannot determine the arch based on the pointer size ({.code .Machine$sizeof.pointer}): {.val {pointer_size}}"
      )
    }

    assert_that_(
      arch %in% valid_arch,
      msg = "Unsupported CPU architecture: {.val {arch}}. The valid ones are {.val {str_comma(valid_arch)}}"
    )

    if (os == "darwin") {
      assert_that_(arch != "386", msg = "the yq tool is not available for {.val darwin} OS with {.val 386} architecture.")
    }

    arch <- dplyr::if_else(arch == 32L, "386", "amd64")
    url <- glue(
      "https://github.com/mikefarah/yq/releases/download/{yq_version}/yq_{os}_{arch}{exe}",
      exe = dplyr::if_else(os == "windows", ".exe", "")
    )

    cli_alert_info(str_space(
      "{.code yq} v{yq_version} binary for OS {.val {os}} and arch {.val {arch}} is going to be downloaded as {.file {destfile}}",
      "from {.url {url}}"
    ))
  } else {
    cli_alert_info("{.code yq} binary from {.url {url}} is going to be downloaded as {.file {destfile}}")
  }

  url <- as.character(url)

  if (!dry) {
    cli_alert_info("Also, directory {.file {fs::path_dir(destfile)}} will be created if it does not exist.")

    if (ask) {
      continue <- .confirm_menu()
      if (continue != 1L) {
        cli_abort("Interrupting the download.")
      }
    }

    fs::dir_create(fs::path_dir(destfile), recurse = TRUE)
    utils::download.file(url, destfile)
    fs::file_chmod(destfile, "+x")

    if (set_as_default) {
      options(scdrake_yq_binary = destfile)
    }

    if (do_check) {
      check_yq(yq_binary = destfile, repair_executable = FALSE, verbose = TRUE)
    }
  }

  return(url)
}

#' @title Check the availability and version of the `yq` tool.
#' @param yq_binary A character scalar: path to the `yq` tool's binary.
#' @param repair_executable A logical scalar: if `TRUE`, make the binary executable.
#' @inheritParams verbose
#' @return `TRUE` if checks pass.
#'
#' @details When R is run from the command line, the `PATH` environment variable is inherited from the current shell
#' session. But when R is run within the RStudio Server, user's `PATH` is not used. To modify the environment in which
#' R is run within your project, place `.Renviron` file to the root of your project, e.g.:
#'
#' ```bash
#' echo 'PATH=${PATH}:"/path/to/dir/with/yq"' >> .Renviron
#' ```
#'
#' Environment variables defined in `.Renviron` will be available in the R session run within the RStudio project.
#'
#' Alternatively in R after `scdrake` package is loaded, you can set a path to the `yq` tool's binary with
#' `options(scdrake_yq_binary = "/path/to/yq/binary")`.
#'
#' @concept yq_tool
#' @export
check_yq <- function(yq_binary = getOption("scdrake_yq_binary"),
                     repair_executable = TRUE,
                     verbose = getOption("scdrake_verbose")) {
  err_message <- c(
    "The {.code yq} tool was not found. You can:",
    i = str_space(
      "Automatically download its binary with {.code download_yq()}.",
      "This will also automatically set {.code options(scdrake_yq_binary = ...)}."
    ),
    i = str_space(
      "If the binary is already downloaded, set its path with {.code options(scdrake_yq_binary = ...)}",
      "or put it to the {.envvar PATH} environment variable, such as {.code yq} will be available in shell",
      "(this is expected by default)."
    ),
    i = str_space(
      "Download the latest v3.4.1 version manually here: {.url https://github.com/mikefarah/yq/releases/tag/3.4.1}",
      "and follow the point above."
    )
  )
  yq_link <- "You can download the latest v3.4.1 version here: https://github.com/mikefarah/yq/releases/tag/3.4.1"
  yq_binary <- fs::path_expand(yq_binary)
  yq_exists <- fs::file_exists(yq_binary)

  assert_that_(
    yq_exists,
    msg = err_message
  )

  if (!fs::file_access(yq_binary, mode = "execute")) {
    if (repair_executable) {
      fs::file_chmod(yq_binary, "+x")
    } else {
      cli_abort(str_space(
        "The yq tool's binary is not executable.",
        "You can run this function again with {.code repair_executable = TRUE}"
      ))
    }
  }

  yq_version <- system2(yq_binary, "-V", stdout = TRUE)
  assert_that(
    stringr::str_detect(yq_version, fixed("yq version 3")),
    msg = cli({
      cli_alert_danger("The yq tool's version is not 3 ('{yq_version}'). You can:")
      cli::cli_ul(err_message)
    })
  )

  verbose %&&% cli_alert_success("The yq tool v3 is ready to use.")

  invisible(TRUE)
}

#' @title Merge two YAML files using the `yq` tool.
#' @param f1,f2 A character scalar: path to YAML file. `f2` is the file whose parameters will overwrite those in `f1`.
#' @param stdout A character or logical scalar, or `NULL`: passed to [base::system2()].
#' - For character scalar: path to file into which the resulting merged YAML file will be written.
#'   If empty string (`""`), the result will be written to R console.
#' - For logical scalar `TRUE`: return the stdout as a character scalar.
#' - For logical scalar `FALSE`, or `NULL`: discard the stdout.
#' @param check_yq A logical scalar: if `TRUE`, check `yq` tool presence with [check_yq()] prior to merging.
#' @param check_exit A logical scalar: if `TRUE` and `stdout` is `FALSE`,
#'   check exit code of shell command used for merging.
#' @param yq_binary A character scalar: path to `yq` tool's binary.
#' @return An output from [base::system2()] - depends on the `stdout` argument.
#'
#' @concept yq_tool
#' @export
yq_merge_cmd <- function(f1,
                         f2,
                         stdout = TRUE,
                         check_yq = TRUE,
                         check_exit = TRUE,
                         yq_binary = getOption("scdrake_yq_binary")) {
  f1 <- fs::path_expand(f1)
  f2 <- fs::path_expand(f2)
  assert_that_(fs::file_exists(f1), msg = "File {.file {f1}} not found.")
  assert_that_(fs::file_exists(f2), msg = "File {.file {f2}} not found.")

  if (is_scalar_character(stdout) && stdout != "") {
    stdout <- fs::path_expand(stdout)
  }

  if (check_yq) {
    check_yq(yq_binary, verbose = FALSE)
  }

  yq_params <- c("merge", "-a", "overwrite", "-x", f1, f2)

  res <- system2(
    yq_binary,
    yq_params,
    stdout = stdout
  )

  if (check_exit && !is_true(stdout)) {
    assert_that_(res == 0, msg = "Error in shell command: {.code {yq_binary} {str_space(yq_params)}}")
  }

  return(str_line(res))
}
