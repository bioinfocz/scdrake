#' @param x An object of class `scdrake_list`.
#' @param i A character scalar or vector: name / names of element(s) to extract.
#' @param check A logical scalar: if `TRUE`, throw an error when non-existing names or subscripts are present.
#' @name scdrake_list_generics_params
NULL

#' @rdname scdrake_list
#' @export
`$.scdrake_list` <- function(x, i) {
  x_exprs <- deparse(substitute(x))
  assert_that_(i %in% names(x), msg = "Variable {.field {i}} not found in {.code {x_exprs}}")
  get(i, x)
}

#' @rdname scdrake_list
#' @export
`[.scdrake_list` <- function(x, i, check = TRUE) {
  x_exprs <- deparse(substitute(x))
  x <- unclass(x)

  if (check) {
    if (is_character(i)) {
      names_exist <- i %in% names(x)
      assert_that_(
        all(names_exist),
        msg = str_space(
          "{if (sum(!names_exist) == 1) 'Variable' else 'Variables'}",
          "{.field {i[!names_exist]}} not found in {.code {x_exprs}}"
        )
      )
    } else if (is_bare_numeric(i)) {
      subscripts <- as.integer(i)
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
    if (is_character(i)) {
      for (name in i) {
        if (!name %in% names(x)) {
          x[name] <- list(NULL)
        }
      }
    }
  }

  structure(x[i], class = c("scdrake_list", "list"))
}

#' @rdname scdrake_list
#' @export
`[[.scdrake_list` <- function(x, i, check = TRUE) {
  x_exprs <- deparse(substitute(x))
  x <- unclass(x)

  if (check) {
    if (is_bare_numeric(i)) {
      possible_subscripts <- seq_along(x)
      assert_that_(i %in% possible_subscripts, msg = "Subscript {.field {i}} is out of bounds for {.code {x_exprs}}")
      x <- x[[i]]
    } else {
      assert_that_(i %in% names(x), msg = "Variable {.field {i}} not found in {.code {x_exprs}}")
      x <- get(i, x)
    }
  } else {
    x <- x[[i]]
  }

  return(x)
}
