#' Defunct functions
#'
#' @description
#' `r lifecycle::badge("defunct")`
#'
#' These function were all deprecated for at least two years and are now
#' defunct. Executing them to tell you which function to use instead.
#'
#' @keywords internal
#' @name defunct
NULL

#' @usage # Deprecated in 0.5.0 -------------------------------------
#' @name defunct
NULL

#' @export
#' @rdname defunct
id <- function(.variables, drop = FALSE) {
  lifecycle::deprecate_stop("0.5.0", "id()", "vctrs::vec_group_id()")
}

#' @usage # Deprecated in 0.7.0 -------------------------------------
#' @name defunct
NULL

#' @export
#' @rdname defunct
failwith <- function(default = NULL, f, quiet = FALSE) {
  lifecycle::deprecate_stop("0.7.0", "failwith()", "purrr::possibly()")
}

#' @usage # Deprecated in 0.8.* -------------------------------------
#' @name defunct
NULL

#' @export
#' @rdname defunct
funs <- function(..., .args = list()) {
  lifecycle::deprecate_stop("0.8.0", "funs()", details = paste_line(
    "Please use a list of either functions or lambdas: ",
    "",
    "  # Simple named list: ",
    "  list(mean = mean, median = median)",
    "",
    "  # Auto named with `tibble::lst()`: ",
    "  tibble::lst(mean, median)",
    "",
    "  # Using lambdas",
    "  list(~ mean(., trim = .2), ~ median(., na.rm = TRUE))"
  ))
}
