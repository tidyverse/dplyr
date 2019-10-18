
#' @importFrom tidyselect peek_vars vars_select
#' @export
by_column <- function(df, funs = identity, .name = NULL, .unpack = TRUE) {
  colwise(funs, .name = .name, .unpack = .unpack)(df)
}

#' @export
over <- function(select, funs = identity, .name = NULL, .unpack = TRUE) {
  by_column(pick({{select}}), funs, .name = .name, .unpack = .unpack)
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
colwise <- function(funs = identity, .name = NULL, .unpack = TRUE) {
  single_function <- is.function(funs) || is_formula(funs)
  if (single_function) {
    funs <- list(fun = funs)
  } else {
    if (is.null(names(funs))) {
      abort("funs should be a single function, a single formula, or a named list of functions or formulas")
    }
  }
  funs <- map(funs, as_function)

  function(df) {
    if (is.null(.name)) {
      if (!.unpack || single_function) {
        .name <- "{var}"
      } else if(ncol(df) == 1) {
        .name <- "{fun}"
      } else {
        .name <- "{fun}_{var}"
      }
    }

    if (.unpack) {
      results <- map2(funs, names(funs), function(f, name) {
        out <- map(df, f)
        names(out) <- glue::glue(.name, var = names(out), fun = name)
        out
      })
      tibble(!!!flatten(unname(results)))
    } else {
      results <- map2(funs, names(funs), function(f, name) {
        out <- map(df, f)
        names(out) <- glue::glue(.name, var = names(out), fun = name)
        tibble(!!!out)
      })
      tibble(!!!results)
    }
  }
}
