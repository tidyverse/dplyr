
#' @importFrom tidyselect peek_vars vars_select
#' @export
each <- function(fun, ..., .name = "{var}") {
  vars <- vars_select(peek_vars(), ...)
  fun <- as_function(fun)
  names(vars) <- glue::glue(.name, var = names(vars), idx = seq_along(vars))

  quo <- quo(tibble(!!!map(vars, function(.x) expr((!!fun)(!!sym(.x))))))
  peek_mask()$internal_eval(quo)
}
