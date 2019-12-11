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
  dots <- dots_values(...)
  if (length(dots) == 1 && is.list(dots[[1]]) && !is.data.frame(dots[[1]])) {
    dots <- dots[[1]]
  }
  dataframe_ish <- function(.x) {
    is.data.frame(.x) || (vec_is(.x) && !is.null(names(.x)))
  }
  dots <- keep(
    flatten_if(dots, function(.x) is.list(.x) && !is.data.frame(.x)),
    function(.x) !is.null(.x)
  )

  dots <- keep(dots, function(.x) !is.null(.x))
  dots <- flatten_if(dots, function(.x) is.list(.x) && !dataframe_ish(.x))

  if (!is_null(.id)) {
    if (!(is_string(.id))) {
      bad_args(".id", "must be a scalar string, ",
        "not {friendly_type_of(.id)} of length {length(.id)}"
      )
    }
    if (!all(have_name(dots) | map_lgl(dots, is_empty))) {
      dots <- compact(dots)
      names(dots) <- seq_along(dots)
    }
  }
  if (!is.null(names(dots)) && !all(map_lgl(dots, dataframe_ish))) {
    dots <- list(as_tibble(dots))
  }

  for (i in seq_along(dots)) {
    .x <- dots[[i]]
    if (!is.data.frame(.x) && !vec_is(.x)) {
      abort(glue("Argument {i} must be a data frame or a named atomic vector"))
    }

    if (is.null(names(.x))) {
      abort(glue("Argument {i} must have names"))
    }
  }

  dots <- map(dots, function(.x) if(is.data.frame(.x)) .x else tibble(!!!as.list(.x)))
  result <- vec_rbind(!!!dots, .names_to = .id)
  if (length(dots) && is_tibble(first <- dots[[1L]])) {
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
  dots <- dots_values(...)
  not_null <- function(.x) !is.null(.x)
  dots <- keep(dots, not_null)

  # nothing to bind, return a dummy tibble
  if (!length(dots)) {
    return(tibble())
  }

  # Before things are squashed, we need
  # some information about the "first" data frame
  if (is.data.frame(dots[[1]]) || !is.list(dots[[1]])) {
    first <- dots[[1]]
  } else {
    first <- dots[[1]][[1]]
  }

  dots <- squash_if(dots, function(.x) is.list(.x) && !is.data.frame(.x))
  dots <- keep(dots, not_null)
  if (!length(dots)) {
    return(tibble())
  }

  res <- vec_cbind(!!!dots)
  if (length(dots)) {
    if (is_grouped_df(first)) {
      res <- grouped_df(res, group_vars(first), group_by_drop_default(first))
    } else if(inherits(first, "rowwise_df")){
      res <- rowwise(res)
    } else if(is_tibble(first) || !is.data.frame(first)) {
      res <- as_tibble(res)
    }
  }
  res
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
