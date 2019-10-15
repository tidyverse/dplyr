
#' @importFrom tidyselect peek_vars vars_select
#' @export
by_column <- function(df, ..., .name = NULL) {
  colwise(..., .name = .name)(df)
}

#' @export
over <- function(select, ..., .name = NULL) {
  by_column(pick({{select}}), ..., .name = .name)
}

#' @export
pick <- function(...) {
  vars <- vars_select(peek_vars(), ...)
  peek_mask()$pick(vars)
}

#' @export
colwise <- function(..., .name = NULL) {
  funs <- map(list(...), as_function)
  if (is.null(names(funs))) {
    names(funs) <- paste0("fn", seq_along(funs))
  }

  function(df) {
    if (is.null(.name)) {
      if (length(funs) == 1) {
        .name <- "{var}"
      } else if(ncol(df) == 1) {
        .name <- "{fun}"
      } else {
        .name <- "{fun}_{var}"
      }
    }

    results <- map2(funs, names(funs), function(f, name) {
      out <- map(df, f)
      names(out) <- glue::glue(.name, var = names(out), fun = name)
      out
    })

    tibble(!!!flatten(unname(results)))
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

