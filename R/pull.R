#' Pull out a single variable
#'
#' This works like `[[` for local data frames, and automatically collects
#' before indexing for remote data tables.
#'
#' @param .data A table of data
#' @inheritParams tidyselect::vars_pull
#' @param name An optional parameter that specifies the column to be used
#'   as names for a named vector. Specified in a similar manner as \code{var}.
#' @return A vector the same size as `.data`.
#' @export
#' @examples
#' mtcars %>% pull(-1)
#' mtcars %>% pull(1)
#' mtcars %>% pull(cyl)
#'
#' # Also works for remote sources
#' if (requireNamespace("dbplyr", quietly = TRUE)) {
#' df <- dbplyr::memdb_frame(x = 1:10, y = 10:1, .name = "pull-ex")
#' df %>%
#'   mutate(z = x * y) %>%
#'   pull()
#' }
#'
#' # Pull a named vector
#' starwars %>% pull(height, name)
#'
pull <- function(.data, var = -1, name = NULL) {
  UseMethod("pull")
}
#' @export
pull.data.frame <- function(.data, var = -1, name = NULL) {
  var <- tidyselect::vars_pull(names(.data), !!enquo(var))
  name <- enquo(name)
  if (quo_is_null(name)) {
    return(.data[[var]])
  }
  name <- tidyselect::vars_pull(names(.data), !!name)
  set_names(.data[[var]], nm = .data[[name]])
}

# FIXME: remove this once dbplyr uses vars_pull()
find_var <- function(expr, vars) {
  var_env <- set_names(as.list(seq_along(vars)), vars)
  var <- eval_tidy(expr, var_env)

  if (!is.numeric(var) || length(var) != 1) {
    bad_args("var", "must evaluate to a single number")
  }

  var <- as.integer(var)
  n <- length(vars)

  if (is.na(var) || abs(var) > n || var == 0L) {
    bad_args("var", "must be a value between {-n} and {n} (excluding zero), not {var}")
  }

  if (var < 0) {
    var <- var + n + 1
  }

  vars[[var]]
}
