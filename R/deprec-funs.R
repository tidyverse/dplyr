#' Create a list of functions calls.
#'
#' \Sexpr[results=rd, stage=render]{dplyr:::lifecycle("soft-deprecated")}
#'
#' @description
#'
#' `funs()` provides a flexible way to generate a named list of
#' functions for input to other functions like [summarise_at()].
#'
#' @param ... A list of functions specified by:
#'
#'  - Their name, `"mean"`
#'  - The function itself, `mean`
#'  - A call to the function with `.` as a dummy argument,
#'    `mean(., na.rm = TRUE)`
#'
#'  These arguments are automatically [quoted][rlang::quo]. They
#'  support [unquoting][rlang::quasiquotation] and splicing. See
#'  `vignette("programming")` for an introduction to these concepts.
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
#' @examples
#' funs(mean, "mean", mean(., na.rm = TRUE))
#'
#' # Override default names
#' funs(m1 = mean, m2 = "mean", m3 = mean(., na.rm = TRUE))
#'
#' # If you have function names in a vector, use funs_
#' fs <- c("min", "max")
#' funs_(fs)
#'
#' # Not supported
#' \dontrun{
#' funs(function(x) mean(x, na.rm = TRUE))
#' funs(~mean(x, na.rm = TRUE))}
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
