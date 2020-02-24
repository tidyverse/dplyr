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
#'   position, see [mutate-joins].
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
#' one <- starwars[1:4, ]
#' two <- starwars[9:12, ]
#'
#' # You can supply data frames as arguments:
#' bind_rows(one, two)
#'
#' # The contents of lists are spliced automatically:
#' bind_rows(list(one, two))
#' bind_rows(split(starwars, starwars$homeworld))
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
#' # When you supply a column name with the `.id` argument, a new
#' # column is created to link each row to its original data frame
#' bind_rows(list(one, two), .id = "id")
#' bind_rows(list(a = one, b = two), .id = "id")
#' bind_rows("group 1" = one, "group 2" = two, .id = "groups")
#'
#' # Columns don't need to match when row-binding
#' bind_rows(tibble(x = 1:3), tibble(y = 1:4))
#' \dontrun{
#' # Rows do need to match when column-binding
#' bind_cols(tibble(x = 1:3), tibble(y = 1:2))
#' }
#'
#' bind_cols(one, two)
#' bind_cols(list(one, two))
#' @name bind
NULL

#' @export
#' @rdname bind
bind_rows <- function(..., .id = NULL) {
  dots <- list2(...)

  # bind_rows() has weird legacy squashing behaviour
  is_flattenable <- function(x) is.list(x) && !is_named(x) && !is.data.frame(x)
  if (length(dots) == 1 && is_bare_list(dots[[1]])) {
    dots <- dots[[1]]
  }
  dots <- flatten_if(dots, is_flattenable)
  dots <- discard(dots, is.null)

  if (is_named(dots) && !all(map_lgl(dots, dataframe_ish))) {
    # This is hit by map_dfr() so we can't easily deprecate
    return(as_tibble(dots))
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

  if (!is_null(.id)) {
    if (!is_string(.id)) {
      bad_args(".id", "must be a scalar string, ",
        "not {friendly_type_of(.id)} of length {length(.id)}"
      )
    }
    if (!is_named(dots)) {
      names(dots) <- seq_along(dots)
    }
  }

  dots <- map(dots, function(.x) if (is.data.frame(.x)) .x else tibble(!!!.x))

  out <- vec_rbind(!!!dots, .names_to = .id)
  if (length(dots) && is_tibble(first <- dots[[1L]])) {
    out <- dplyr_reconstruct(out, first)
  }
  out
}

#' @export
#' @rdname bind
bind_cols <- function(...) {
  dots <- list2(...)

  is_flattenable <- function(x) is.list(x) && !is.data.frame(x)
  dots <- squash_if(dots, is_flattenable)
  dots <- discard(dots, is.null)

  # Strip names off of data frame components so that vec_cbind() unpacks them
  is_data_frame <- map_lgl(dots, is.data.frame)
  names(dots)[is_data_frame] <- ""

  out <- vec_cbind(!!!dots)
  if (length(dots) && is_tibble(first <- dots[[1L]])) {
    out <- dplyr_reconstruct(out, first)
  }
  out
}

# helpers -----------------------------------------------------------------

dataframe_ish <- function(.x) {
  is.data.frame(.x) || (vec_is(.x) && is_named(.x))
}
