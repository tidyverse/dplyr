
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
  fun <- as_function(fun)
  force(.name)

  function(df, ...) {
    out <- map(df, fun, ...)
    names(out) <- glue::glue(.name, var = names(out), idx = seq_len(ncol(df)))
    tibble(!!!out)
  }
}

#' @export
mapping <- function(df, fun, ..., .name = "{var}") {
  colwise(fun, .name)(df, ...)
}

#' @export
pick <- function(...) {
  vars <- vars_select(peek_vars(), ...)
  peek_mask()$pick(vars)
}

