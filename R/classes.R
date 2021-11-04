#' @title A `scdrake`'s list with overloaded, strict access operators.
#' @description
#' It behaves like a normal list, but, by default, it has more strict rules for non-existing elements:
#' `$`, `[`, and `[[` operators are overloaded for this class.
#' They try to extract element(s) from a `scdrake_list` object, but throw an error when the element(s) does not exist.
#' This behaviour can be avoided for `[` and `[[` by using the `check = FALSE` parameter, e.g. `l[name, check = FALSE]`.
#' Also, in that case and for `[`, a `scdrake_list` with non-existing names and `NULL` values is returned instead of putting
#' `NA_character_` for them, which is the default behaviour for a base list, e.g.
#'
#' ```r
#' > list(a = 2)["b"]
#' $<NA>
#' NULL
#' ```
#'
#' compared to
#'
#' ```r
#' > scdrake_list(list(a = 2))["b", check = FALSE]
#' $b
#' NULL
#'
#' attr(,"class")
#' [1] "scdrake_list"
#' ```
#'
#' However, this does not work for numeric subscripts.
#'
#' @param l A list.
#' @inheritParams scdrake_list_generics_parameters
#' @return An object of class `scdrake_list`.
#'
#' @examples
#' cfg <- scdrake_list(list(var_1 = 1, var_2 = 2))
#' # The standard list behavior.
#' cfg$var_1
#' cfg[["var_2"]]
#' cfg["var_1"]
#' cfg[c("var_1", "var_2")]
#' \dontrun{
#' # Strict rules for non-existing elements - throws an error.
#' cfg$var_3
#' cfg[["var_3"]]
#' cfg[c("var_1", "var_3")]
#' }
#'
#' @rdname scdrake_list
#' @export
scdrake_list <- function(l = NULL) {
  if (is_null(l)) {
    l <- list()
  } else {
    assert_that_(is_list(l), msg = "{.var l} must be a list")
  }

  structure(l, class = "scdrake_list")
}
