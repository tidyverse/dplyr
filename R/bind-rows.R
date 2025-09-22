#' Bind multiple data frames by row
#'
#' Bind any number of data frames by row, making a longer result. This is
#' similar to `do.call(rbind, dfs)`, but the output will contain all columns
#' that appear in any of the inputs.
#'
#' @param ... Data frames to combine. Each argument can either be a data frame,
#'   a list that could be a data frame, or a list of data frames. Columns are
#'   matched by name, and any missing columns will be filled with `NA`.
#' @param .id The name of an optional identifier column. Provide a string to
#'   create an output column that identifies each input. The column will use
#'   names if available, otherwise it will use positions.
#' @returns A data frame the same type as the first element of `...`.
#' @aliases bind
#' @export
#' @examples
#' df1 <- tibble(x = 1:2, y = letters[1:2])
#' df2 <- tibble(x = 4:5, z = 1:2)
#'
#' # You can supply individual data frames as arguments:
#' bind_rows(df1, df2)
#'
#' # Or a list of data frames:
#' bind_rows(list(df1, df2))
#'
#' # When you supply a column name with the `.id` argument, a new
#' # column is created to link each row to its original data frame
#' bind_rows(list(df1, df2), .id = "id")
#' bind_rows(list(a = df1, b = df2), .id = "id")
bind_rows <- function(..., .id = NULL) {
  dots <- list2(...)

  # bind_rows() has weird legacy squashing behaviour
  is_flattenable <- function(x) !is_named(x)
  if (length(dots) == 1 && is_bare_list(dots[[1]])) {
    dots <- dots[[1]]
  }
  dots <- list_flatten(dots, fn = is_flattenable)
  dots <- discard(dots, is.null)

  # Used to restore type
  if (length(dots) == 0) {
    first <- NULL
  } else {
    first <- dots[[1L]]
  }

  if (is_named(dots) && !all(map_lgl(dots, dataframe_ish))) {
    # This is hit by map_dfr() so we can't easily deprecate
    return(as_tibble(dots))
  }

  for (i in seq_along(dots)) {
    .x <- dots[[i]]
    if (!dataframe_ish(.x)) {
      abort(glue("Argument {i} must be a data frame or a named atomic vector."))
    }

    if (obj_is_list(.x)) {
      dots[[i]] <- vctrs::data_frame(!!!.x, .name_repair = "minimal")
    }
  }

  if (!is_null(.id)) {
    check_string(.id)

    if (!is_named(dots)) {
      # Replace `NA` or `""` names with their index,
      # but leave existing names in place (#7100)
      dots_with_names <- have_name(dots)
      dots_without_names <- which(!dots_with_names)
      names(dots)[dots_without_names] <- as.character(dots_without_names)
    }
  } else {
    # Don't let `vec_rbind(.id = NULL)` promote input names to row names
    names(dots) <- NULL
  }

  out <- vec_rbind(!!!dots, .names_to = .id, .error_call = current_env())

  # Override vctrs coercion rules and instead derive class from first input
  if (is.data.frame(first)) {
    out <- dplyr_reconstruct(out, first)
  } else {
    out <- as_tibble(out)
  }
  out
}

# helpers -----------------------------------------------------------------

dataframe_ish <- function(.x) {
  is.data.frame(.x) || (vec_is(.x) && is_named(.x))
}
