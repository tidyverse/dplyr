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
#' `bind_rows()` is a special S3 generic due to the fact that its first
#' argument is `...`. S3 methods for `bind_rows()` are written as:
#'
#' ```r
#' #' @importFrom dplyr bind_rows
#' #' @export
#' bind_rows.my_subclass <- function(..., .id = NULL) {
#' }
#' ```
#'
#' Dispatch is performed on the _first_ argument, so order matters when data
#' frames of different types are being combined.
#'
#' For S3 methods authors:
#'
#' - `...` will always contain at least 1 input. The first input will have the
#'   S3 class that dispatch was performed on, but no other inputs are validated.
#'
#' - `...` must be captured by [rlang::list2()].
#'
#' - `bind_rows()`'s S3 generic will preprocess `...` by applying all legacy
#'   flattening behavior and removing any `NULL` values. Dispatch occurs on the
#'   first argument remaining after this preprocessing, and S3 methods should
#'   expect that `...` contains a flat list of objects to bind.
#'
#' - `bind_rows()` S3 method signatures must exactly match the signature of the
#'   `bind_rows()` generic. Due to how this S3 generic works, additional method
#'   specific arguments are not allowed.
#'
#' - If you call `rlang::abort()` from your S3 method, you will likely need to
#'   provide `abort(call = call("bind_rows"))`, otherwise an intermediate
#'   function may be reported as the error call site.
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
bind_rows <- function(..., .id = NULL) {
  # `bind_rows_dots()` will massage the `...` into the right form. Any
  # `bind_rows()` method will receive the massaged form of `...`, so it should
  # not have to rework them.
  dots <- bind_rows_dots(...)
  return(bind_rows_dispatch(!!!dots, .id = .id))

  # Having `UseMethod("bind_rows")` here has no runtime effect, but tells
  # roxygen2 that `bind_rows()` is an S3 generic, so it can recognize that
  # `bind_rows.default()` is an S3 method (and S3 methods that other packages
  # will write).
  UseMethod("bind_rows")
}

bind_rows_dots <- function(...) {
  dots <- list2(...)

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

bind_rows_dispatch <- function(..., .id = NULL) {
  dots <- list2(...)

  if (length(dots) == 0L) {
    # If we have nothing to dispatch on, we call our data frame method.
    # This returns an empty tibble for lack of anything better to return.
    # If `.id` is specified, it should add an empty character `.id` column,
    # but currently doesn't due to https://github.com/r-lib/vctrs/issues/1627.
    return(bind_rows.data.frame(..., .id = .id))
  }

  # Otherwise, actually dispatch on the first object. This means that all
  # `bind_rows()` methods can expect at least 1 input, even though their
  # signature takes `...`. A `bind_rows()` method must capture dots with
  # `rlang::list2()` because we splice them into `bind_rows_dispatch()`,
  # which is seen as the generic.
  UseMethod("bind_rows", dots[[1L]])
}

#' @export
bind_rows.data.frame <- function(..., .id = NULL) {
  # Otherwise `bind_rows_dispatch()` is reported by `abort()`
  call <- call("bind_rows")

  dots <- list2(...)

  # Used to restore type
  if (length(dots) == 0L) {
    first <- NULL
  } else {
    first <- dots[[1L]]
  }

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
        call = call
      )
    }

    if (obj_is_list(.x)) {
      dots[[i]] <- vctrs::data_frame(!!!.x, .name_repair = "minimal")
    }
  }

  if (!is_null(.id)) {
    check_string(.id, call = call)

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

  out <- vec_rbind(!!!dots, .names_to = .id, .error_call = call)

  # Override vctrs coercion rules and instead derive class from first input
  if (is.data.frame(first)) {
    out <- dplyr_reconstruct(out, first)
  } else {
    out <- as_tibble(out)
  }

  out
}

# We register `bind_rows.data.frame()` like we do for all other dplyr verbs,
# like `mutate.data.frame()`. We also register `bind_rows.default()` because
# legacy behavior of `bind_rows()` allows it to work directly on atomic vectors,
# so we need a default fallthrough. We could just register
# `bind_rows.default()` and not register `bind_rows.data.frame()`, but we felt
# it was correct to "own" the data.frame method as well.
#' @export
bind_rows.default <- function(..., .id = NULL) {
  bind_rows.data.frame(..., .id = .id)
}
