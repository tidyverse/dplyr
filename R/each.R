
#' @importFrom tidyselect peek_vars vars_select
#' @export
by_column <- function(df, funs = identity, .name = NULL) {
  colwise(funs, .name = .name)(df)
}

#' @export
over <- function(select, funs = identity, .name = NULL) {
  by_column(pick({{select}}), funs, .name = .name)
}

#' @export
accross <- over

#' @export
pick <- function(...) {
  vars <- vars_select(peek_vars(), ...)
  peek_mask()$pick(vars)
}

#' @export
current_key <- function() {
  peek_mask()$current_key()
}

#' @export
colwise <- function(funs = identity, .name = NULL) {
  if (is.function(funs) || is_formula(funs)) {
    funs = list(fun = funs)
  }
  funs <- map(funs, as_function)
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
