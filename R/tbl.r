#' Create a table from a data source
#'
#' This is a generic method that dispatches based on the first argument.
#'
#' @param src A data source
#' @param ... Other arguments passed on to the individual methods
#' @export
tbl <- function(src, ...) {
  UseMethod("tbl")
}

#' Create a "tbl" object
#'
#' `tbl()` is the standard constructor for tbls. `as.tbl()` coerces,
#' and `is.tbl()` tests.
#'
#' @keywords internal
#' @export
#' @param subclass name of subclass. "tbl" is an abstract base class, so you
#'   must supply this value. `tbl_` is automatically prepended to the
#'   class name
#' @param object to test/coerce.
#' @param ... For `tbl()`, other fields used by class. For `as.tbl()`,
#'   other arguments passed to methods.
#' @examples
#' as.tbl(mtcars)
make_tbl <- function(subclass, ...) {
  subclass <- paste0("tbl_", subclass)
  structure(list(...), class = c(subclass, "tbl"))
}

#' @rdname tbl
#' @export
is.tbl <- function(x) inherits(x, "tbl")

#' @export
#' @rdname tbl
#' @param x an object to coerce to a `tbl`
as.tbl <- function(x, ...) UseMethod("as.tbl")

#' @export
as.tbl.tbl <- function(x, ...) x

#' @export
as.tbl.data.frame <- function(x, ...) {
  as_tibble(x)
}

tbl_vars_dispatch <- function(x) {
  UseMethod("tbl_vars")
}

new_sel_vars <- function(vars, group_vars) {
  structure(
    vars,
    groups = group_vars,
    class = c("dplyr_sel_vars", "character")
  )
}

#' List variables provided by a tbl.
#'
#' `tbl_vars()` returns all variables while `tbl_nongroup_vars()`
#' returns only non-grouping variables. The `groups` attribute
#' of the object returned by `tbl_vars()` is a character vector of the
#' grouping columns.
#'
#' @export
#' @param x A tbl object
#' @seealso [group_vars()] for a function that returns grouping
#'   variables.
#' @keywords internal
tbl_vars <- function(x) {
  return(new_sel_vars(tbl_vars_dispatch(x), group_vars(x)))

  # For roxygen and static analysis
  UseMethod("tbl_vars")
}

#' @export
tbl_vars.data.frame <- function(x) {
  names(x)
}

#' @rdname tbl_vars
#' @export
tbl_nongroup_vars <- function(x) {
  setdiff(tbl_vars(x), group_vars(x))
}

is_sel_vars <- function(x) {
  inherits(x, "dplyr_sel_vars")
}

#' @export
print.dplyr_sel_vars <- function(x, ...) {
  cat("<dplyr:::vars>\n")
  print(unstructure(x))

  groups <- attr(x, "groups")
  if (length(groups)) {
    cat("Groups:\n")
    print(groups)
  }

  invisible(x)
}
