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

#' @title Format a shell command as a Markdown codeblock.
#' @description The shell command will be formatted such that each item of `lines` is put onto a separate line suffixed
#' by ` \`.
#' @param lines A character vector: command lines.
#' @param language A character scalar: codeblock language.
#' @param wrap A logical scalar: if `TRUE`, run `wrap_code()` onto the formatted command.
#' @return A character vector.
#'
#' @rdname format_shell_command
#' @concept misc_text
#' @export
format_shell_command <- function(lines, language = "bash", wrap = TRUE) {
  if (length(lines) > 1) {
    ## -- Which lines to append "\" to.
    to_wrap_lines_i <- seq_len(length(lines) - 1)
    ## -- Which lines to indent.
    to_indent_lines_i <- seq(2, length(lines))

    lines <- purrr::map2_chr(seq_len(length(lines)), lines, function(i, line) {
      if (i %in% to_wrap_lines_i) {
        line <- glue0c("{line} \\")
      }

      if (i %in% to_indent_lines_i) {
        line <- glue0c("  {line}")
      }

      return(line)
    }) %>%
      paste(collapse = "\n")
  }

  if (wrap) {
    lines <- wrap_code(lines, language = language)
  }

  lines
}

#' @title Wrap lines into a Markdown codeblock.
#' @param lines A character vector: code lines.
#' @param collapse A logical scalar: if `TRUE`, collapse the `lines` by newline (`\n`).
#' @param language A character scalar: codeblock language.
#'
#' @rdname format_shell_command
#' @concept misc_text
#' @export
wrap_code <- function(lines, collapse = TRUE, language = "bash") {
  if (collapse) {
    lines <- paste(lines, collapse = "\n")
  }
  glue0c("```{language}\n{lines}\n```")
}

#' @param commands A list of character vectors with command lines passed into `format_shell_command()`.
#'
#' @rdname format_shell_command
#' @concept misc_text
#' @export
format_shell_commands <- function(commands, language = "bash", wrap = TRUE) {
  res <- purrr::map_chr(commands, format_shell_command, wrap = FALSE) %>%
    paste(collapse = "\n")

  if (wrap) {
    res <- wrap_code(res, language = language)
  }

  res
}
