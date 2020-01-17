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
#' An object of the same type as `.data`.
#' * Rows are not affected.
#' * The same columns appear in the output, but (usually) in a different place.
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
#' # Can also select variables based on their type
#' df %>% relocate(is.character)
#' df %>% relocate(is.numeric, .after = last_col())
#' # Or with any other select helper
#' df %>% relocate(any_of(c("a", "e", "i", "o", "u")))
#'
#' # When .before or .after refers to multiple variables they will be
#' # moved to be immediately before/after the selected variables.
#' df2 <- tibble(a = 1, b = "a", c = 1, d = "a")
#' df2 %>% relocate(is.numeric, .after = is.character)
#' df2 %>% relocate(is.numeric, .before = is.character)
relocate <- function(.data, ..., .before, .after) {
  UseMethod("relocate")
}

#' @export
relocate.data.frame <- function(.data, ..., .before, .after) {
  to_move <- tidyselect::eval_select(expr(c(...)), .data)

  if (!missing(.before) && !missing(.after)) {
    abort("Must supply only one of `.before` and `.after`")
  } else if (!missing(.before) && missing(.after)) {
    where <- tidyselect::eval_select(enexpr(.before), .data)
    to_move <- c(setdiff(to_move, where), where)
  } else if (missing(.before) && !missing(.after)) {
    where <- tidyselect::eval_select(enexpr(.after), .data)
    to_move <- c(where, setdiff(to_move, where))
  } else {
    where <- 1L
    to_move <- union(to_move, where)
  }

  lhs <- setdiff(seq2(1, min(where) - 1), to_move)
  rhs <- setdiff(seq2(max(where) + 1, ncol(.data)), to_move)

  .data[vec_unique(c(lhs, to_move, rhs))]
}
