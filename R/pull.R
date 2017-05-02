#' Pull out a single variable
#'
#' This works like `[[` for local data frames, and automatically collects
#' before indexing for remote data tables.
#'
#' @param .data A table of data
#' @param var A variable specified as:
#'   * a literal variable name
#'   * a positive integer, giving the position counting from the left
#'   * a negative integer, giving the position counting from the right.
#'
#'   The default returns the last column (on the assumption that's the
#'   column you've created most recently).
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
pull <- function(.data, var = -1) {
  UseMethod("pull")
}

#' @export
pull.data.frame <- function(.data, var = -1) {
  expr <- enquo(var)
  var <- find_var(expr, names(.data))
  .data[[var]]
}

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
