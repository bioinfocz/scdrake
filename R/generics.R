#' @param x An object of class `scdrake_list`.
#' @param name,names_ A character scalar or vector: name / names of element(s) to extract.
#' @param check A logical scalar: if `TRUE`, throw an error when non-existing names or subscripts are present.
#' @name scdrake_list_generics_parameters
NULL

#' @rdname scdrake_list
#' @export
`$.scdrake_list` <- function(x, name) {
  x_exprs <- deparse(substitute(x))
  assert_that_(name %in% names(x), msg = "Variable {.field {name}} not found in {.code {x_exprs}}")
  get(name, x)
}

#' @rdname scdrake_list
#' @export
`[.scdrake_list` <- function(x, names_, check = TRUE) {
  x_exprs <- deparse(substitute(x))
  x <- unclass(x)

  if (check) {
    if (is_character(names_)) {
      names_exist <- names_ %in% names(x)
      assert_that_(
        all(names_exist),
        msg = str_space(
          "{if (sum(!names_exist) == 1) 'Variable' else 'Variables'}",
          "{.field {names_[!names_exist]}} not found in {.code {x_exprs}}"
        )
      )
    } else if (is_bare_numeric(names_)) {
      subscripts <- as.integer(names_)
      subscripts_exist <- subscripts %in% seq_along(x)
      assert_that_(
        all(subscripts_exist),
        msg = str_space(
          "{if (sum(!subscripts_exist) == 1) 'Subscript' else 'Subscripts'}",
          "{.field {subscripts[!subscripts_exist]}}",
          "{if (sum(!subscripts_exist) == 1) 'is' else 'are'}",
          "out of bounds for {.code {x_exprs}}"
        )
      )
    }
  } else {
    if (is_character(names_)) {
      for (name in names_) {
        if (!name %in% names(x)) {
          x[name] <- list(NULL)
        }
      }
    }
  }

  structure(x[names_], class = "scdrake_list")
}

#' @rdname scdrake_list
#' @export
`[[.scdrake_list` <- function(x, name, check = TRUE) {
  x_exprs <- deparse(substitute(x))
  x <- unclass(x)

  if (check) {
    if (is_bare_numeric(name)) {
      possible_subscripts <- seq_along(x)
      assert_that_(name %in% possible_subscripts, msg = "Subscript {.field {name}} is out of bounds for {.code {x_exprs}}")
      x <- x[[name]]
    } else {
      assert_that_(name %in% names(x), msg = "Variable {.field {name}} not found in {.code {x_exprs}}")
      x <- get(name, x)
    }
  } else {
    x <- x[[name]]
  }

  return(x)
}
