#' Filter rows with missing values
#'
#' Remove rows with missing values in variables that are selected.
#'
#' @param .data A tbl. Since this function generates expressions using
#'    \code{\link{filter}} it should support all dplyr backends.
#' @param ... Comma separated list of unquoted expressions. Select variables
#'   which to test for missing values using the same semantics as
#'   \code{\link[dplyr]{select}}.
#' @param .all If \code{TRUE}, then remove rows in which all columns are
#'   missing, else select rows in which any column has a missing value.
#' @param .dots use \code{filter_na_()} to do standard evaluation. See
#'   \code{vignette("nse")} for details.
#' @return An object of the same class as \code{.data}.
#' @export
filter_na <- function(.data, ..., .all = FALSE) {
  filter_na_(.data, .dots = lazyeval::lazy_dots(...), .all = .all)
}

#' @rdname filter_na
#' @export
filter_na_ <- function(.data, ..., .dots, .all = FALSE) {
  dots <- lazyeval::all_dots(.dots, ...)
  if (length(dots) == 0) {
    dots <- list(~ everything())
  }
  vars <- select_vars_(names(.data), dots)
  # construct expression for filter
  # !is.na(var1), !is.na(var2), ...
  args <- lapply(vars, function(i) call("!", call("is.na", as.name(i))))
  if (.all) {
    # !is.na(var1) | !is.na(var2) | ...
    if (length(args) > 1) {
      args <- Reduce(function(x, y) call("|", x, y),
                     args[-1], args[[1]])
    }
  }
  args <- unname(args)
  filter_(.data, .dots = args)
}
