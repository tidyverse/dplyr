#' Pull out a single variable
#'
#' This works like `[[` for local data frames, and automatically collects
#' before indexing for remote data tables.
#'
#' @param .data A table of data
#' @param var A variable specified as:
#'   * a single string, the variable name
#'   * a positive integer, giving the position counting from the left
#'   * a negative integer, giving the position counting from the right.
#'
#'   The default returns the last column (on the assumption that's the
#'   column you've created most recently).
#' @export
#' @examples
#' mtcars %>% pull(-1)
#' mtcars %>% pull(1)
#' mtcars %>% pull("cyl")
#'
#' # Also works for remote sources
#' if (requireNamespace("RSQLite", quietly = TRUE)) {
#' df <- memdb_frame(x = 1:10, y = 10:1, .name = "pull-ex")
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
  var <- find_var(var, names(.data))
  .data[[var]]
}

#' @export
pull.tbl_sql <- function(.data, var = -1) {
  var <- find_var(var, tbl_vars(.data))

  .data <- select(.data, !! symbol(var))
  .data <- collect(.data)
  .data[[1]]
}

find_var <- function(var, vars) {
  if (is_string(var)) {
    if (!var %in% vars) {
      abort(glue("Unknown variable '{var}'", var = var))
    }
    var
  } else if (is.numeric(var) && length(var) == 1) {
    var <- as.integer(var)
    n <- length(vars)

    if (is.na(var) || abs(var) > n || var == 0L) {
      abort(glue("`var` must take a value between -{n} and {n}", n = n))
    }

    if (var < 0) {
      var <- var + n + 1
    }

    vars[[var]]

  } else {
    abort("`var` must be a numeric or character vector of length 1")
  }
}
