#' Efficiently bind multiple data frames by row and column
#'
#' This is an efficient implementation of the common pattern of
#' `do.call(rbind, dfs)` or `do.call(cbind, dfs)` for binding many
#' data frames into one.
#'
#' The output of `bind_rows()` will contain a column if that column
#' appears in any of the inputs.
#'
#' @param ... Data frames to combine.
#'
#'   Each argument can either be a data frame, a list that could be a data
#'   frame, or a list of data frames.
#'
#'   When row-binding, columns are matched by name, and any missing
#'   columns will be filled with NA.
#'
#'   When column-binding, rows are matched by position, so all data
#'   frames must have the same number of rows. To match by value, not
#'   position, see [join].
#' @param .id Data frame identifier.
#'
#'   When `.id` is supplied, a new column of identifiers is
#'   created to link each row to its original data frame. The labels
#'   are taken from the named arguments to `bind_rows()`. When a
#'   list of data frames is supplied, the labels are taken from the
#'   names of the list. If no names are found a numeric sequence is
#'   used instead.
#' @return `bind_rows()` and `bind_cols()` return the same type as
#'   the first input, either a data frame, `tbl_df`, or `grouped_df`.
#' @examples
#' one <- mtcars[1:4, ]
#' two <- mtcars[11:14, ]
#'
#' # You can supply data frames as arguments:
#' bind_rows(one, two)
#'
#' # The contents of lists are spliced automatically:
#' bind_rows(list(one, two))
#' bind_rows(split(mtcars, mtcars$cyl))
#' bind_rows(list(one, two), list(two, one))
#'
#'
#' # In addition to data frames, you can supply vectors. In the rows
#' # direction, the vectors represent rows and should have inner
#' # names:
#' bind_rows(
#'   c(a = 1, b = 2),
#'   c(a = 3, b = 4)
#' )
#'
#' # You can mix vectors and data frames:
#' bind_rows(
#'   c(a = 1, b = 2),
#'   tibble(a = 3:4, b = 5:6),
#'   c(a = 7, b = 8)
#' )
#'
#'
#' # Note that for historical reasons, lists containing vectors are
#' # always treated as data frames. Thus their vectors are treated as
#' # columns rather than rows, and their inner names are ignored:
#' ll <- list(
#'   a = c(A = 1, B = 2),
#'   b = c(A = 3, B = 4)
#' )
#' bind_rows(ll)
#'
#' # You can circumvent that behaviour with explicit splicing:
#' bind_rows(!!!ll)
#'
#'
#' # When you supply a column name with the `.id` argument, a new
#' # column is created to link each row to its original data frame
#' bind_rows(list(one, two), .id = "id")
#' bind_rows(list(a = one, b = two), .id = "id")
#' bind_rows("group 1" = one, "group 2" = two, .id = "groups")
#'
#' # Columns don't need to match when row-binding
#' bind_rows(data.frame(x = 1:3), data.frame(y = 1:4))
#' \dontrun{
#' # Rows do need to match when column-binding
#' bind_cols(data.frame(x = 1), data.frame(y = 1:2))
#' }
#'
#' bind_cols(one, two)
#' bind_cols(list(one, two))
#' @name bind
NULL

#' @export
#' @rdname bind
bind_rows <- function(..., .id = NULL) {
  x <- flatten_bindable(dots_values(...))

  if (!is_null(.id)) {
    if (!(is_string(.id))) {
      bad_args(".id", "must be a scalar string, ",
        "not {friendly_type_of(.id)} of length {length(.id)}"
      )
    }
    if (!all(have_name(x) | map_lgl(x, is_empty))) {
      x <- compact(x)
      names(x) <- seq_along(x)
    }
  }

  bind_rows_check(x)
  x <- keep(x, function(.x) {
    is.data.frame(.x) || vec_size(.x) > 0
  })

  result <- vec_rbind(!!!x, .names_to = .id)
  if (length(x) && is_tibble(first <- x[[1L]])) {
    if (is_grouped_df(first)) {
      result <- grouped_df(result, group_vars(first), group_by_drop_default(first))
    } else {
      class(result) <- class(first)
    }
  }
  result
}

#' @export
#' @rdname bind
bind_cols <- function(...) {
  x <- keep(flatten_bindable(dots_values(...)), function(.x) !is.null(.x))
  out <- cbind_all(x)
  tibble::repair_names(out)
}

#' Combine vectors
#'
#' \Sexpr[results=rd, stage=render]{dplyr:::lifecycle("questioning")}
#'
#' @description
#' `combine()` acts like [c()] or
#' [unlist()] but uses consistent dplyr coercion rules.
#'
#' If `combine()` it is called with exactly one list argument, the list is
#' simplified (similarly to `unlist(recursive = FALSE)`). `NULL` arguments are
#' ignored. If the result is empty, `logical()` is returned.
#' Use [vctrs::vec_c()] if you never want to unlist.
#'
#' @param ... Vectors to combine.
#'
#' @seealso
#' `bind_rows()` and `bind_cols()` in [bind].
#'
#' @export
#' @examples
#' # combine applies the same coercion rules as bind_rows()
#' f1 <- factor("a")
#' f2 <- factor("b")
#' c(f1, f2)
#' unlist(list(f1, f2))
#'
#' combine(f1, f2)
#' combine(list(f1, f2))
combine <- function(...) {
  signal_soft_deprecated(paste_line(
    "combine() is deprecated. ",
    "Please use vctrs::vec_c() instead"
  ))

  args <- list2(...)
  if (length(args) == 1 && is.list(args[[1]])) {
    args <- args[[1]]
  }
  args <- keep(args, function(.x) !is.null(.x))
  names(args) <- NULL
  if (length(args) == 0) {
    logical()
  } else {
    vec_c(!!!args)
  }
}
