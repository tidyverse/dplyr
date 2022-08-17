#' Change column order
#'
#' Use `relocate()` to change column positions, using the same syntax as
#' `select()` to make it easy to move blocks of columns at once.
#'
#' @inheritParams arrange
#' @param ... <[`tidy-select`][dplyr_tidy_select]> Columns to move.
#' @param .before,.after <[`tidy-select`][dplyr_tidy_select]> Destination of
#'   columns selected by `...`. Supplying neither will move columns to the
#'   left-hand side; specifying both is an error.
#' @return
#' An object of the same type as `.data`. The output has the following
#' properties:
#'
#' * Rows are not affected.
#' * The same columns appear in the output, but (usually) in a different place
#'   and possibly renamed.
#' * Data frame attributes are preserved.
#' * Groups are not affected.
#' @section Methods:
#' This function is a **generic**, which means that packages can provide
#' implementations (methods) for other classes. See the documentation of
#' individual methods for extra arguments and differences in behaviour.
#'
#' The following methods are currently available in loaded packages:
#' \Sexpr[stage=render,results=rd]{dplyr:::methods_rd("relocate")}.
#' @export
#' @examples
#' df <- tibble(a = 1, b = 1, c = 1, d = "a", e = "a", f = "a")
#' df %>% relocate(f)
#' df %>% relocate(a, .after = c)
#' df %>% relocate(f, .before = b)
#' df %>% relocate(a, .after = last_col())
#'
#' # relocated columns can change name
#' df %>% relocate(ff = f)
#'
#' # Can also select variables based on their type
#' df %>% relocate(where(is.character))
#' df %>% relocate(where(is.numeric), .after = last_col())
#' # Or with any other select helper
#' df %>% relocate(any_of(c("a", "e", "i", "o", "u")))
#'
#' # When .before or .after refers to multiple variables they will be
#' # moved to be immediately before/after the selected variables.
#' df2 <- tibble(a = 1, b = "a", c = 1, d = "a")
#' df2 %>% relocate(where(is.numeric), .after = where(is.character))
#' df2 %>% relocate(where(is.numeric), .before = where(is.character))
relocate <- function(.data, ..., .before = NULL, .after = NULL) {
  UseMethod("relocate")
}

#' @export
relocate.data.frame <- function(.data, ..., .before = NULL, .after = NULL) {
  loc <- eval_relocate(
    expr = expr(c(...)),
    data = .data,
    before = enquo(.before),
    after = enquo(.after),
    before_arg = ".before",
    after_arg = ".after"
  )

  out <- dplyr_col_select(.data, loc)
  out <- set_names(out, names(loc))

  out
}

eval_relocate <- function(expr,
                          data,
                          ...,
                          before = NULL,
                          after = NULL,
                          before_arg = "before",
                          after_arg = "after",
                          env = caller_env(),
                          error_call = caller_env()) {
  # `eval_relocate()` returns a named integer vector of size `ncol(data)`
  # describing how to rearrange `data`. Each location in the range
  # `seq2(1L, ncol(data))` is represented once. The names are the new names to
  # assign to those columns. They are typically the same as the original names,
  # but `expr` does allow for renaming.

  check_dots_empty0(...)

  sel <- tidyselect::eval_select(
    expr = expr,
    data = data,
    env = env,
    error_call = error_call
  )

  # Enforce the invariant that relocating can't change the number of columns by
  # retaining only the last instance of a column that is renamed multiple times
  # TODO: https://github.com/r-lib/vctrs/issues/1442
  # `sel <- vec_unique(sel, which = "last")`
  loc_last <- which(!duplicated(sel, fromLast = TRUE))
  sel <- vec_slice(sel, loc_last)

  n <- length(data)

  before <- as_quosure(before, env = env)
  after <- as_quosure(after, env = env)

  has_before <- !quo_is_null(before)
  has_after <- !quo_is_null(after)

  if (has_before && has_after) {
    message <- glue("Can't supply both `{before_arg}` and `{after_arg}`.")
    abort(message, call = error_call)
  }

  if (has_before) {
    # TODO: Use `allow_rename = FALSE`. https://github.com/r-lib/tidyselect/issues/221
    where <- tidyselect::eval_select(before, data, env = env, error_call = error_call)
    where <- unname(where)

    if (length(where) == 0L) {
      # Empty `before` selection pushes `sel` to the front
      where <- 1L
    } else {
      where <- min(where)
    }
  } else if (has_after) {
    # TODO: Use `allow_rename = FALSE`. https://github.com/r-lib/tidyselect/issues/221
    where <- tidyselect::eval_select(after, data, env = env, error_call = error_call)
    where <- unname(where)

    if (length(where) == 0L) {
      # Empty `after` selection pushes `sel` to the back
      where <- n
    } else {
      where <- max(where)
    }

    where <- where + 1L
  } else {
    # Defaults to `before = everything()` if neither `before` nor `after` are supplied
    where <- 1L
  }

  lhs <- seq2(1L, where - 1L)
  rhs <- seq2(where, n)

  lhs <- setdiff(lhs, sel)
  rhs <- setdiff(rhs, sel)

  names <- names(data)

  names(lhs) <- names[lhs]
  names(rhs) <- names[rhs]

  sel <- vec_c(lhs, sel, rhs)

  sel
}
