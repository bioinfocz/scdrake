## -- Functions related to text processing and printing.

#' @title Various utils for joining of text.
#' @param ... Character vectors or scalars. Passed to [stringr::str_c()].
#'   For [catn()], character vectors or scalars to join.
#' @param collapse A logical scalar: if `TRUE`, join also character vectors passed to `...`.
#'
#' @concept misc_text
#' @name text_utils_joining
#' @return A character scalar.
NULL

.get_collapse <- function(collapse, sep) {
  if (collapse) {
    sep
  } else {
    NULL
  }
}

#' @description Join character vectors by commas and spaces (optionally).
#' @param space A logical scalar: if `TRUE`, add space after commas.
#'
#' @rdname text_utils_joining
#' @export
str_comma <- function(..., space = TRUE, collapse = TRUE) {
  if (space) {
    sep <- ", "
  } else {
    sep <- ","
  }

  str_c(..., sep = sep, collapse = .get_collapse(collapse, sep))
}

#' @description Join character vectors by newlines (`\n`).
#'
#' @rdname text_utils_joining
#' @export
str_line <- function(..., collapse = TRUE) {
  str_c(..., sep = "\n", collapse = .get_collapse(collapse, "\n"))
}

#' @description Join character vector by spaces.
#'
#' @rdname text_utils_joining
#' @export
str_space <- function(..., collapse = TRUE) {
  str_c(..., sep = " ", collapse = .get_collapse(collapse, " "))
}

#' @title Various wrappers around [glue::glue()] and [cat()].
#' @param ...,.envir Passed to [glue::glue()].
#'
#' @concept misc_text
#' @name text_utils_glue
NULL

#' @description Call `glue()` without trimming.
#'
#' @rdname text_utils_glue
#' @export
glue0 <- function(..., .envir = parent.frame()) {
  glue(..., .trim = FALSE, .envir = .envir)
}

#' @description Call `glue()` without trimming and `cat()` the result.
#'
#' @rdname text_utils_glue
#' @export
catg0 <- function(..., .envir = parent.frame()) {
  cat(glue0(..., .envir = .envir))
}

#' @description Call `glue()` and coerce the result to character.
#'
#' @rdname text_utils_glue
#' @export
gluec <- function(..., .envir = parent.frame()) {
  as.character(glue(..., .envir = .envir))
}

#' @description Call `glue()` without trimming and coerce the result to character.
#'
#' @rdname text_utils_glue
#' @export
glue0c <- function(..., .envir = parent.frame()) {
  as.character(glue0(..., .envir = .envir))
}

#' @description Join a character vector by newlines and cat the result.
#'
#' @rdname text_utils_glue
#' @export
catn <- function(...) {
  cat(str_line(...))
}

#' @title Generate random strings compounded from alphabetical characters by default.
#' @description This is a wrapper around `stringi::stri_rand_strings()`.
#' @param n,length,pattern Passed to `stringi::stri_rand_strings()`.
#' @return A character vector.
#'
#' @concept misc_text
#' @export
get_random_strings <- function(n, length, pattern = "[A-Za-z]") {
  stringi::stri_rand_strings(n, length, pattern = pattern)
}
