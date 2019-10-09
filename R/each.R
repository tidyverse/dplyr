
#' @importFrom tidyselect peek_vars vars_select
#' @export
each <- function(fun, ..., .name = "{var}") {
  colwise(fun, .name)(pick(...))
}

#' @export
at <- function(select, fun, .name = "{var}") {
  colwise(fun, .name)(pick({{select}}))
}

#' @export
over <- at

#' @export
mapping <- function(df, fun, ..., .name = "{var}") {
  colwise(fun, .name)(df, ...)
}

#' @export
pick <- function(...) {
  vars <- vars_select(peek_vars(), ...)
  peek_mask()$pick(vars)
}

#' @export
colwise <- function(fun, .name = "{var}") {
  force(fun)
  force(.name)

  function(df, ...) {
    out <- map(df, fun, ...)
    names(out) <- glue::glue(.name, var = names(out), idx = seq_len(ncol(df)))
    tibble(!!!out)
  }
}

#' @export
mapping <- function(vars, fun, ..., .name = "{var}") {
  fun <- as_function(fun)
  names(vars) <- glue::glue(.name, var = names(vars), idx = seq_along(vars))

  quo <- quo(tibble(!!!map(vars, function(.x) expr((!!fun)(!!sym(.x), ...)))))
  peek_mask()$internal_eval(quo)
}

#' @export
pick <- function(...) {
  vars <- vars_select(peek_vars(), ...)
  quo <- quo(tibble(!!!syms(vars)))
  peek_mask()$internal_eval(quo)
}

#' @export
colwise <- function(fun, .name = "{var}") {
  fun <- as_function(fun)
  function(df, ...) {
    out <- map(df, fun, ...)
    names(out) <- glue::glue(.name, var = names(out), idx = seq_len(ncol(df)))
    tibble(!!!out)
  }
}

