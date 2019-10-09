
#' @importFrom tidyselect peek_vars vars_select
#' @export
each <- function(fun, ..., .name = "{var}") {
  vars <- vars_select(peek_vars(), ...)
  fun <- as_function(fun)
  names(vars) <- glue::glue(.name, var = names(vars), idx = seq_along(vars))

  quo <- quo(tibble(!!!map(vars, function(.x) expr((!!fun)(!!sym(.x))))))
  peek_mask()$internal_eval(quo)
}

#' @export
mapping <- function(df, fun, ..., .name = "{var}") {
  colwise(fun, .name)(df, ...)
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

