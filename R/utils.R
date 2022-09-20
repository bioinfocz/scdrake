#' @title Read an environment variable and if needed, assign a default value.
#' @param var A character scalar: name of environment variable.
#' @param default A default value (different from `NULL`) when environment variable is unset (empty string).
#' @param type A character scalar: to which R type will be the variable coerced.
#' @inheritParams verbose
#' @return A character scalar if the environment variable is set, `default` if not, or empty character scalar when
#' `default = NULL` and the variable is unset.
#'
#' @examples
#' get_sys_env("PATH")
#' get_sys_env("NON_EXISTING")
#' get_sys_env("NON_EXISTING", default = "default_value")
#' @concept misc_utils
#' @export
get_sys_env <- function(var,
                        default = NULL,
                        type = c("character", "double", "integer", "logical"),
                        verbose = getOption("scdrake_verbose")) {
  type <- arg_match(type)
  val <- Sys.getenv(var)

  if (!is_null(default) && val == "") {
    verbose %&&% cli_alert_warning(
      "Environment variable {.envvar {var}} is not set -> using default value {.val {default}} ({typeof(default)})"
    )
    val <- default
  } else {
    val_as <- as(val, type)
    # assert_that_(
    #   !is_na(val_as),
    #   msg = "Error when converting environment variable {.var {var}} with value '{val}' to {type}: the result is NA"
    # )
    val <- val_as
    verbose %&&% cli_alert_info("Using environment variable {.envvar {var}} with value {.val {val}} ({type})")
  }

  return(val)
}

#' @title A short-circuit evaluation-like function similar to `bash`'s `&&` operator.
#' @description The right-hand side (`y`) is only evaluated if the left-hand side (`x`) is identical to `TRUE`.
#' Useful for verbosity (`verbose %&&% message("Hello world!")`).
#' @param x A logical scalar: left-hand side argument.
#' @param y A right-hand side expression.
#' @return `y` if `x` is `TRUE`, else `invisible(NULL)`
#'
#' @examples
#' TRUE %&&% message("printed")
#' FALSE %&&% message("not printed")
#' @concept misc_utils
#' @export
`%&&%` <- function(x, y) {
  if (is_true(x)) {
    return(y)
  } else {
    invisible(x)
  }
}

#' @title A wrapper around [assertthat::assert_that()].
#' @description Provides a nicer error message generated through [cli::cli_alert_danger()].
#' @param ... Passed to [assertthat::assert_that()].
#' @param env Passed to [assertthat::assert_that()] and [cli::cli_alert_danger()]. Defaults to caller env.
#' @param msg Passed to [cli::cli_alert_danger()].
#' @return `TRUE` if assertion is `TRUE` :)
#'
#' @concept misc_utils
#' @export
assert_that_ <- function(..., env = parent.frame(), msg = NULL) {
  if (is_null(msg)) {
    assert_that(..., env = env)
  } else {
    withr::with_options(
      list(warning.length = 8170L),
      assert_that(..., env = env, msg = cli::format_error(msg, .envir = env))
    )
  }
}

#' @title Replace `NULL` items in a defined depth of a list.
#' @param l A list.
#' @param replacement A replacement for `NULL` items.
#' @param depth An integer scalar: depth of `l` to search for and replace `NULL` items.
#' @return The list `l` with `NULL` items replaced by `replacement` in the defined `depth`.
#'
#' @examples
#' replace_list_nulls(list(a = list(b = 1, c = NULL)))
#' @concept misc_utils
#' @export
replace_list_nulls <- function(l, replacement = NA, depth = 2L) {
  purrr::map_depth(l, depth, ~ ifelse(is_null(.), replacement, .))
}

#' @title Replace `NA`s in a list by `NULL`s.
#' @param l A list.
#' @return The list `l` with `NA`s replaced by `NULL`s.
#'
#' @examples
#' replace_list_nas_with_nulls(list(NA, a = 1, b = NA))
#' @concept misc_utils
#' @export
replace_list_nas_with_nulls <- function(l) {
  lapply(l, FUN = function(x) {
    if (is_na(x)) {
      return(NULL)
    }

    return(x)
  })
}

#' @title Convert list of lists to tibble.
#' @param l A list of lists.
#' @param do_replace_list_nulls A logical scalar: if `TRUE`, first replace `NULL` items in each sublist by `NA`s.
#' @param do_bind_rows A logical scalar: if `TRUE`, bind all tibbles by rows, otherwise return a list of tibbles.
#' @return See the `do_bind_rows` argument.
#'
#' @examples
#' lists_to_tibble(list(list(a = 1, b = 2), list(b = 3, a = 4, c = 5)))
#' lists_to_tibble(list(list(a = 1, b = 2), list(b = 3, a = 4, c = 5)), do_bind_rows = FALSE)
#' @concept misc_utils
#' @export
lists_to_tibble <- function(l, do_replace_list_nulls = TRUE, do_bind_rows = TRUE) {
  if (do_replace_list_nulls) {
    l <- replace_list_nulls(l)
  }

  dfs <- purrr::map(l, tibble::as_tibble)

  if (do_bind_rows) {
    dfs <- dplyr::bind_rows(dfs)
  }

  return(dfs)
}

#' @title Append names of a list of lists as values in each sublist.
#' @param l A named list of lists.
#' @param key A character scalar: name of item under which names of outer list will be appended.
#' @return A named list of lists.
#'
#' @examples
#' list_names_to_values(list(a = list(1), b = list(c = 3)))
#' @concept misc_utils
#' @export
list_names_to_values <- function(l, key = "name") {
  purrr::map(names(l), ~ c(l[[.]], list(.) %>% set_names(key))) %>%
    set_names(names(l))
}

#' @title Add named item (including `NULL`) to a list.
#' @param l A list.
#' @param key A character scalar: name under which `value` will be placed.
#' @param value Any object.
#' @return A list.
#'
#' @examples
#' add_item_to_list(list(1, a = 2), "b")
#' @concept misc_utils
#' @export
add_item_to_list <- function(l, key, value = NULL) {
  if (is_null(value)) {
    l[key] <- list(NULL)
  } else {
    l[[key]] <- value
  }

  return(l)
}

#' @title Capture output from `print()` of an object.
#' @param object An object to be printed.
#' @param collapse A character scalar: used to collapse the output lines.
#' @return A character scalar for `save_print()`,
#' `list(str = save_print(object), dim = dim(object))` for `save_object_info()`.
#'
#' @examples
#' save_print(lm(mpg ~ cyl, datasets::mtcars))
#' @concept misc_utils
#' @rdname save_print
#' @export
save_print <- function(object, collapse = "\n") {
  out <- utils::capture.output(print(object))

  if (!is_null(collapse)) {
    out <- str_c(out, collapse = collapse)
  }

  return(out)
}

#' @rdname save_print
#' @export
save_object_info <- function(object) {
  list(str = save_print(object), dim = dim(object))
}

#' @title Return list with removed `NULL` elements.
#' @param l A list.
#' @return A list without `NULL` elements.
#'
#' @examples
#' filter_nulls(list(a = 1, b = NULL, c = 3))
#' @concept misc_utils
#' @export
filter_nulls <- function(l) {
  Filter(Negate(is_null), l)
}

#' @title Locally change a `future` plan.
#' @description https://github.com/HenrikBengtsson/future/issues/263
#' @param expr An expression to evaluate using a `future` plan.
#' @param ... Passed to [future::plan()]. Pass a plan here, e.g. `future::sequential`.
#' @param envir An environment in which to evaluate `expr`.
#' @return Value returned by `expr`.
#'
#' @examples
#' with_plan(1 + 1, future::sequential)
#' @concept misc_utils
#' @export
with_plan <- function(expr, ..., envir = parent.frame()) {
  expr <- substitute(expr)
  oplan <- future::plan("list")
  on.exit(future::plan(oplan))
  future::plan(...)
  eval(expr, envir = envir)
}

#' @title Return `NA` if an object is empty.
#' @param x An object.
#' @return `NA` if `x` is empty (`length(x) == 0`), otherwise `x`.
#'
#' @examples
#' na_empty(integer())
#' @concept misc_utils
#' @export
na_empty <- function(x) {
  if (!length(x)) {
    return(NA)
  } else {
    return(x)
  }
}

#' @title Apply a function over rows of a `data.frame`-like object and concatenate the results back to `tibble` or `data.frame`.
#' @description Inside a `FUN`, a row of `df` will be available as a list or [scdrake_list()] (default).
#' The `FUN` must return a named list for a proper concatenation of the results.
#' The named list can also contain elements of a length other than one, which are then wrapped in `list()`.
#' @param df A `data.frame`-like object.
#' @param as_scdrake_list A logical scalar: if `TRUE`, each row-list in `FUN` will be converted to [scdrake_list()].
#' @param return_tibble If `TRUE`, a `tibble` with concatenated results is returned.
#' Otherwise results are coerced to `data.frame` and original rownames are set.
#' @param FUN A function to apply over `df` rows.
#' @param ... Additional arguments passed to `FUN`.
#' @return `tibble` or `data.frame` according to the `return_tibble` parameter.
#'
#' @examples
#' library(magrittr)
#'
#' fn <- function(row_list) {
#'   row_list$cyl_2 <- row_list$cyl**2
#'   row_list$colors <- c("red", "green", "blue")
#'   row_list$sublist <- mtcars[1:5, 1:5]
#'   return(row_list)
#' }
#'
#' df <- lapply_rows(mtcars, FUN = fn)
#' head(df)
#'
#' df2 <- lapply_rows(mtcars, return_tibble = FALSE, FUN = fn)
#' head(df2)
#' @concept misc_utils
#' @export
lapply_rows <- function(df, as_scdrake_list = TRUE, return_tibble = TRUE, FUN, ...) {
  FUN <- match.fun(FUN)
  df_rownames <- rownames(df)
  to_iter <- purrr::transpose(df)

  if (as_scdrake_list) {
    to_iter <- lapply(to_iter, scdrake_list)
  }

  res <- lapply(to_iter, FUN = FUN, ...) %>%
    purrr::map_depth(2, function(x) {
      if (length(x) != 1 || is(x, "scdrake_list")) {
        return(list(x))
      } else {
        return(x)
      }
    }) %>%
    unclass()

  assert_that_(typeof(res) == "list", msg = "{.var FUN} must return a list.")
  res <- dplyr::bind_rows(res)

  if (!return_tibble) {
    res <- as.data.frame(res)
    rownames(res) <- df_rownames
  }

  return(res)
}

#' @title Check if a package is installed and display an informative message.
#' @param pkg A character scalar: name of package.
#' @param msg A character scalar: additional message to be displayed.
#' @return `TRUE` if package is installed, `FALSE` otherwise.
#'
#' @examples
#' check_pkg_installed("utils")
#' check_pkg_installed("zzz")
#' check_qs_installed()
#' @concept misc_utils
#' @rdname check_pkg_installed
#' @export
check_pkg_installed <- function(pkg, msg = "") {
  pkg_installed <- rlang::is_installed(pkg)

  if (!pkg_installed) {
    cli({
      cli_alert_warning("Package {.pkg {pkg}} is not installed. {msg}")
      cli_alert_info('Please, consider its installation with\n{.code BiocManager::install("{pkg}")}')
    })
  }

  return(pkg_installed)
}

#' @rdname check_pkg_installed
#' @export
check_qs_installed <- function() {
  check_pkg_installed(
    "qs",
    "It is used to store intermediate pipeline results, and it is much faster than base R's Rds format."
  )
}

#' @rdname check_pkg_installed
#' @export
check_future_installed <- function() {
  check_pkg_installed("future", "It is used for drake's parallelism (alternative to {.pkg clustermq}).")
}

#' @rdname check_pkg_installed
#' @export
check_clustermq_installed <- function() {
  # check_pkg_installed("clustermq", "It is used for {.pkg drake}'s parallelism.")
  clustermq_installed <- rlang::is_installed("clustermq")

  if (clustermq_installed) {
    clustermq_version <- utils::packageVersion("clustermq")
  } else {
    clustermq_version <- numeric_version(0)
  }

  if (!clustermq_installed || clustermq_version > "0.8.8") {
    cli({
      cli_alert_warning(str_space(
        "Package {.pkg clustermq} used for drake's parallelism (alternative to {.pkg future}) is not installed, or its version",
        "({.field {clustermq_version}}) is different than {.field 0.8.8} and may cause {.pkg drake}'s parallel",
        "execution to hang."
      ))
      cli_alert_info(str_space("
        Please, consider installing the required version with\n",
        '{.field remotes::install_version("clustermq", version = "0.8.8")}'
      ))
    })

    return(FALSE)
  }

  return(TRUE)
}

#' @rdname check_pkg_installed
#' @export
check_future.callr_installed <- function() {
  check_pkg_installed("future.callr", "It is used as a backend for {.pkg drake}'s future parallelism.")
}

#' @rdname check_pkg_installed
#' @export
check_sc3_version <- function() {
  sc3_source <- sessioninfo::package_info(pkgs = "installed") %>%
    dplyr::filter(.data$package == "SC3") %>%
    dplyr::pull(.data$source)

  if (is_empty(sc3_source) || !stringr::str_detect(sc3_source, stringr::fixed("gorgitko/SC3"))) {
    cli_alert_danger(str_space(
      "The {.pkg SC3} package is not installed from the GitHub repository {.field gorgitko/SC3}",
      "which has enhanced parallel execution compared to the original Bioconductor version. This can happen when you update",
      "all packages with {.field BiocManager::install()}."
    ))
    cli_alert_info(str_space(
      "Please, consider installing the modified {.pkg SC3} version with\n",
      '{.field BiocManager::install("gorgitko/SC3")}'
    ))
    return("bioconductor")
  }

  if (is_empty(sc3_source)) {
    return(FALSE)
  }

  return("github")
}

#' @rdname check_pkg_installed
#' @export
check_scdrake_packages <- function() {
  cli::cli_h3("{.pkg scdrake} package checks")
  check_qs_installed()
  check_clustermq_installed()
  check_future_installed()
  if (!parallelly::supportsMulticore()) {
    check_future.callr_installed()
  }
  check_sc3_version()
  cli::cli_h3("Done")

  invisible(NULL)
}

#' @title Set `rstudio_drake_cache` option.
#' @description Value of this option is used internally by `drake` to "loadd target under cursor"
#' (it can be set in `Tools -> Modify Keyboard Shortcuts`).
#' This shortcut will call `loadd(<name of target under cursor>, cache = getOptions("rstudio_drake_cache"))`.
#' @param dir A character scalar: path to directory with `drake` cache.
#' @inheritParams verbose
#' @return Invisibly `TRUE` if `dir` exists, `FALSE` otherwise.
#'
#' @concept misc_utils
#' @export
set_rstudio_drake_cache <- function(dir, verbose = TRUE) {
  if (!fs::dir_exists(dir)) {
    verbose %&&% cli_alert_warning("Failed to set {.var rstudio_drake_cache} option: the directory {.file {dir}} does not exist.")
    return(invisible(FALSE))
  } else {
    options(rstudio_drake_cache = drake::drake_cache(path = dir))
    verbose %&&% cli_alert_success("Set {.var rstudio_drake_cache} option: the directory {.file {dir}} will be used.")
    return(invisible(TRUE))
  }
}

.confirm_menu <- function(choices = c("Yes.", "No"), title = "Do you want to continue?", .choice = 1L) {
  if (interactive()) {
    utils::menu(choices, title = title)
  } else {
    return(.choice)
  }
}

## -- https://conjugateprior.org/2015/06/identifying-the-os-from-r/
.get_os <- function() {
  sysinf <- Sys.info()

  if (!is_null(sysinf)) {
    os <- sysinf["sysname"]
    if (os == "unix") {
      os <- "linux"
    }
  } else {
    os <- .Platform$OS.type

    if (stringr::str_detect(R.version$os, "^darwin")) {
      os <- "darwin"
    }

    if (stringr::str_detect(R.version$os, "linux-gnu")) {
      os <- "linux"
    }
  }

  return(stringr::str_to_lower(os))
}

#' @title Save a list of plots to multipage PDF.
#' @param plots A list of plots.
#' @param output_file A character scalar: path to output PDF file.
#'   If file's directory doesn't exist, it will be created recursively.
#' @param width,height A numeric scalar: default width and height of graphics region in inches. Defaults to 7.
#' @param make_thumbnail A logical scalar: if `TRUE`, a PNG file will be created from the first plot in `plots`.
#' @param stop_on_error A logical scalar: if `TRUE`, the function will stop when there is an error during the
#'   saving of the plot. Otherwise a dummy PDF/PNG thumbnail file with error description will be created instead.
#' @return A named list with the following items:
#' - `success`: a logical scalar indicating whether the plot saving succeeded (`TRUE`) or not (`FALSE`).
#' - `error`: a character scalar with error message if `success` is `FALSE`, `NULL` otherwise.
#' - `error_plot`: a `ggplot2` object with error plot if `success` is `FALSE`, `NULL` otherwise.
#' - `output_file`: a character scalar, identical to `output_file` parameter.
#' - `thumbnail_file`: a character scalar, path to thumbnail PNG file if `make_thumbnail` is `TRUE`, `NULL` otherwise.
#'
#' Note that `success` of `FALSE` and the accompanying error message and plot are only possible when `stop_on_error` is `FALSE`.
#'
#' @concept misc_utils
#' @export
save_pdf <- function(plots, output_file, width = NULL, height = NULL, make_thumbnail = FALSE, stop_on_error = FALSE) {
  fs::dir_create(fs::path_dir(output_file), recurse = TRUE)
  res <- tryCatch(
    {
      grDevices::pdf(output_file, width = width, height = height, useDingbats = FALSE)
      for (p in plots) {
        print(p)
      }
      grDevices::dev.off()
      list(plot = NULL, error = NULL)
    },
    error = function(e) {
      msg <- "Error saving {.file {output_file}}: {as.character(e)}"
      grDevices::dev.off()
      fs::file_delete(output_file)
      if (stop_on_error) {
        cli::cli_abort(msg)
      } else {
        cli_alert_danger(msg)
        grDevices::pdf(output_file, width = width, height = height, useDingbats = FALSE)
        p <- create_dummy_plot(glue("Error when saving the plot: {as.character(e)}") %>% stringr::str_wrap())
        print(p)
        grDevices::dev.off()
      }

      return(list(plot = p, error = as.character(e)))
    }
  )

  success <- is_null(res$plot)

  if (make_thumbnail) {
    if (success) {
      p_thumbnail <- plots[[1]]
    } else {
      p_thumbnail <- res$plot
    }
    thumbnail_file <- fs::path_ext_set(output_file, "png")
    grDevices::png(thumbnail_file)
    print(p_thumbnail)
    grDevices::dev.off()
  } else {
    thumbnail_file <- NULL
  }

  return(list(
    success = success,
    error = res$error,
    error_plot = res$plot,
    output_file = output_file,
    thumbnail_file = thumbnail_file
  ))
}

#' @title Create a blank `ggplot` with label.
#' @param label A character scalar: text to display in the plot.
#' @return A `ggplot2` object.
#'
#' @concept misc_utils
#' @export
create_dummy_plot <- function(label) {
  ggplot() +
    ggplot2::theme_void() +
    ggplot2::geom_text(aes(x = 0, y = 0, label = label))
}
