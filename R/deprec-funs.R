#' Create a list of function calls
#'
#' @description
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("deprecated")}
#'
#' `funs()` is deprecated; please use `list()` instead. We deprecated this
#' function because it provided a unique way of specifying anonymous functions,
#' rather than adopting the conventions used by purrr and other packages
#' in the tidyverse.
#'
#' @param ... <[`data-masking`][dplyr_data_masking]> A list of functions
#'   specified by:
#'
#'  - Their name, `"mean"`
#'  - The function itself, `mean`
#'  - A call to the function with `.` as a dummy argument,
#'    `mean(., na.rm = TRUE)`
#'
#'  The following notations are **not** supported, see examples:
#'
#'  - An anonymous function, `function(x) mean(x, na.rm = TRUE)`
#'  - An anonymous function in \pkg{purrr} notation, `~mean(., na.rm = TRUE)`
#'
#' @param .args,args A named list of additional arguments to be added to all
#'   function calls. As `funs()` is being deprecated, use other methods to
#'   supply arguments: `...` argument in [scoped verbs][summarise_at()] or make
#'   own functions with [purrr::partial()].
#' @export
#' @keywords internal
#' @examples
#' funs("mean", mean(., na.rm = TRUE))
#' # ->
#' list(mean = mean, mean = ~ mean(.x, na.rm = TRUE))
#'
#' funs(m1 = mean, m2 = "mean", m3 = mean(., na.rm = TRUE))
#' # ->
#' list(m1 = mean, m2 = "mean", m3 = ~ mean(.x, na.rm = TRUE))
funs <- function(..., .args = list()) {
  lifecycle::deprecate_warn("1.0.0", "funs()", details = paste_line(
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

  dots <- enquos(...)
  default_env <- caller_env()

  funs <- map(dots, function(quo) as_fun(quo, default_env, .args))
  new_funs(funs)
}
new_funs <- function(funs) {
  attr(funs, "have_name") <- any(names2(funs) != "")

  # Workaround until rlang:::label() is exported
  temp <- map(funs, function(fn) node_car(quo_get_expr(fn)))
  temp <- exprs_auto_name(temp)
  names(funs) <- names(temp)

  class(funs) <- "fun_list"
  funs
}
