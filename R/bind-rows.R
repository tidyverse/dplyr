#' Bind multiple data frames by row
#'
#' Bind any number of data frames by row, making a longer result. This is
#' similar to `do.call(rbind, dfs)`, but the output will contain all columns
#' that appear in any of the inputs.
#'
#' @param ... Data frames to combine. Each argument can either be a data frame,
#'   a list that could be a data frame, or a list of data frames. Columns are
#'   matched by name, and any missing columns will be filled with `NA`.
#'
#' @param .id The name of an optional identifier column. Provide a string to
#'   create an output column that identifies each input. The column will use
#'   names if available, otherwise it will use positions.
#'
#' @returns A data frame the same type as the first element of `...`.
#'
#' @aliases bind
#'
#' @section Methods:
#' This function is a **generic**, which means that packages can provide
#' implementations (methods) for other classes. See the documentation of
#' individual methods for extra arguments and differences in behaviour.
#'
#' Methods available in currently loaded packages:
#' \Sexpr[stage=render,results=rd]{dplyr:::methods_rd("bind_rows")}.
#'
#' `bind_rows()` is a special S3 generic due to the fact that it dispatches on
#' `...`. S3 methods for `bind_rows()` are written as:
#'
#' ```r
#' #' @importFrom dplyr bind_rows
#' #' @export
#' bind_rows.my_subclass <- function(..., .id = NULL) {
#'   dots <- dplyr::bind_rows_dots(...)
#'   # Implementation
#' }
#' ```
#'
#' Dispatch is performed on the _first_ argument, so order matters when data
#' frames of different types are being combined.
#'
#' The very first thing you must do in your S3 method is call
#' `dplyr::bind_rows_dots()`. This does the following:
#'
#' - Applies all legacy flattening behavior.
#'
#' - Removes all `NULL` values.
#'
#' - Removes all named arguments with a name that begins with a `.`, which are
#'   seen as unknown optional arguments rather than as named inputs to bind.
#'
#' If you add additional optional arguments to your S3 method, then they _must_
#' begin with a `.` prefix, otherwise the dispatch done in the generic will be
#' incorrect. For example, a dbplyr implementation might look like:
#'
#' ```r
#' #' @importFrom dplyr bind_rows
#' #' @export
#' bind_rows.tbl_lazy <- function(..., .copy = FALSE) {
#'   dots <- dplyr::bind_rows_dots(...)
#'   # Implementation
#' }
#' ```
#'
#' Notice how this implementation both adds a new argument, `.copy`, and
#' refuses to implement the `.id` argument from the data frame method. Both of
#' these are allowed as long as `bind_rows_dots()` is called.
#'
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
bind_rows <- function(...) {
  # Let `bind_rows_dots()` perform all legacy flattening along with discarding
  # of `NULL` so we know what to dispatch on. But most importantly it also
  # discards any named inputs that are named with a `.` prefix. We consider
  # these to be optional arguments rather than inputs to bind, and are not
  # part of determining what to dispatch on.
  dots <- bind_rows_dots(...)

  if (length(dots) == 0L) {
    # If we have nothing to dispatch on, we return an empty tibble for lack of
    # anything better to return.
    return(new_tibble(x = list(), nrow = 0L))
  }

  UseMethod("bind_rows", dots[[1L]])
}

#' @rdname bind_rows
#' @export
bind_rows.data.frame <- function(..., .id = NULL) {
  bind_rows_impl(..., .id = .id)
}

#' @export
bind_rows.default <- function(..., .id = NULL) {
  # We register `bind_rows.data.frame()` like we do for all other dplyr verbs,
  # like `mutate.data.frame()`. We also register `bind_rows.default()` because
  # legacy behavior of `bind_rows()` allows it to work directly on atomic
  # vectors, so we need a default fallthrough. We could just register
  # `bind_rows.default()` and not register `bind_rows.data.frame()`, but we felt
  # it was correct to "own" the data.frame method as well.
  bind_rows_impl(..., .id = .id)
}

bind_rows_impl <- function(..., .id = NULL, .error_call = caller_env()) {
  dots <- bind_rows_dots(...)

  if (length(dots) == 0L) {
    abort("Should have at least 1 input from dispatch.", .internal = TRUE)
  }

  # Used to restore type
  first <- dots[[1L]]

  dataframe_ish <- function(.x) {
    is.data.frame(.x) || (vec_is(.x) && is_named(.x))
  }

  if (is_named(dots) && !all(map_lgl(dots, dataframe_ish))) {
    # This is hit by map_dfr() so we can't easily deprecate
    return(as_tibble(dots))
  }

  for (i in seq_along(dots)) {
    .x <- dots[[i]]
    if (!dataframe_ish(.x)) {
      abort(
        glue("Argument {i} must be a data frame or a named atomic vector."),
        call = .error_call
      )
    }

    if (obj_is_list(.x)) {
      dots[[i]] <- vctrs::data_frame(!!!.x, .name_repair = "minimal")
    }
  }

  if (!is_null(.id)) {
    check_string(.id, call = .error_call)

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

  out <- vec_rbind(!!!dots, .names_to = .id, .error_call = .error_call)

  # Override vctrs coercion rules and instead derive class from first input
  if (is.data.frame(first)) {
    out <- dplyr_reconstruct(out, first)
  } else {
    out <- as_tibble(out)
  }

  out
}

# TODO: Export this
bind_rows_dots <- function(...) {
  dots <- list2(...)

  # Discard any part of `...` that is named where the name starts with `.`,
  # we deem this to be an argument rather than a named data frame to bind
  names <- names2(dots)
  dot_prefixed <- startsWith(names, ".")

  if (any(dot_prefixed)) {
    dots <- dots[!dot_prefixed]

    # If no other arguments are named after removing the `.` arguments,
    # then also clear the `""` names because they would not have been there
    # to begin with
    if (all(names[!dot_prefixed] == "")) {
      names(dots) <- NULL
    }
  }

  # Legacy behavior allows you to supply a list of data frames directly,
  # which we flatten by unwrapping. We now prefer you `!!!` them in.
  if (length(dots) == 1 && is_bare_list(dots[[1]])) {
    dots <- dots[[1]]
  }

  # Second level of legacy flattening behavior, which is very odd, but it
  # technically allows this to work:
  # `bind_rows(list(data.frame(x = 1)), list(data.frame(x = 1)))`
  is_flattenable <- function(x) !is_named(x)
  dots <- list_flatten(dots, fn = is_flattenable)

  # Now that we are done with flattening, discard all `NULL`s
  dots <- discard(dots, is.null)

  dots
}
