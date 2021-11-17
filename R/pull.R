#' Extract a single column
#'
#' `pull()` is similar to `$`. It's mostly useful because it looks a little
#' nicer in pipes, it also works with remote data frames, and it can optionally
#' name the output.
#'
#' @inheritParams arrange
#' @inheritParams tidyselect::vars_pull
#' @param name An optional parameter that specifies the column to be used
#'   as names for a named vector. Specified in a similar manner as \code{var}.
#' @param ... For use by methods.
#' @return A vector the same size as `.data`.
#' @section Methods:
#' This function is a **generic**, which means that packages can provide
#' implementations (methods) for other classes. See the documentation of
#' individual methods for extra arguments and differences in behaviour.
#'
#' The following methods are currently available in loaded packages:
#' \Sexpr[stage=render,results=rd]{dplyr:::methods_rd("pull")}.
#' @export
#' @examples
#' mtcars %>% pull(-1)
#' mtcars %>% pull(1)
#' mtcars %>% pull(cyl)
#' @examplesIf requireNamespace("dbplyr", quietly = TRUE) && requireNamespace("RSQLite", quietly = TRUE)
#'
#' # Also works for remote sources
#' df <- dbplyr::memdb_frame(x = 1:10, y = 10:1, .name = "pull-ex")
#' df %>%
#'   mutate(z = x * y) %>%
#'   pull()
#' @examples
#'
#' # Pull a named vector
#' starwars %>% pull(height, name)
pull <- function(.data, var = -1, name = NULL, ...) {
  check_dots_used()
  UseMethod("pull")
}
#' @export
pull.data.frame <- function(.data, var = -1, name = NULL, ...) {
  var <- tidyselect::vars_pull(names(.data), !!enquo(var))
  name <- enquo(name)
  if (quo_is_null(name)) {
    return(.data[[var]])
  }
  name <- tidyselect::vars_pull(names(.data), !!name)
  set_names(.data[[var]], nm = .data[[name]])
}

find_var <- function(expr, vars) {
  var_env <- set_names(as.list(seq_along(vars)), vars)
  var <- eval_tidy(expr, var_env)

  if (!is.numeric(var) || length(var) != 1) {
    abort("`var` must evaluate to a single number.")
  }

  var <- as.integer(var)
  n <- length(vars)

  if (is.na(var) || abs(var) > n || var == 0L) {
    abort("`var` must be a value between {-n} and {n} (excluding zero), not {var}.")
  }

  if (var < 0) {
    var <- var + n + 1
  }

  vars[[var]]
}
