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
  is_flattenable <- function(x) vec_is_list(x) && !is_named(x)
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
      msg <- glue("Argument {i} must be a data frame or a named atomic vector.")
      abort(msg)
    }

    if (is.null(names(.x))) {
      msg <- glue("Argument {i} must have names.")
      abort(msg)
    }
  }

  if (!is_null(.id)) {
    check_string(.id)

    if (!is_named(dots)) {
      names(dots) <- seq_along(dots)
    }
  }

  if (!length(dots)) {
    return(tibble())
  }

  first <- dots[[1L]]
  dots <- map(dots, function(.x) {
    if (vec_is_list(.x)) {
      .x <- vctrs::data_frame(!!!.x, .name_repair = "minimal")
    }
    .x
  })

  if (is.null(.id)) {
    names(dots) <- NULL
  }
  out <- vec_rbind(!!!dots, .names_to = .id, .call = current_env())
  if (length(dots)) {
    if (is.data.frame(first)) {
      out <- dplyr_reconstruct(out, first)
    } else {
      out <- as_tibble(out)
    }
  }
  out
}

#' Bind multiple data frames by column
#'
#' @description
#' Bind any number of data frames by column, making a wider result.
#' This is similar to `do.call(cbind, dfs)`.
#'
#' Where possible prefer using a [join][left_join] to combine multiple
#' data frames. `bind_cols()` binds the rows in order in which they appear
#' so it is easy to create meaningless results without realising it.
#'
#' @param ... Data frames to combine. Each argument can either be a data frame,
#'   a list that could be a data frame, or a list of data frames.
#'   Inputs are [recycled][vctrs::vector_recycling_rules] to the same length,
#'   then matched by position.
#' @param .name_repair One of `"unique"`, `"universal"`, or
#'   `"check_unique"`. See [vctrs::vec_as_names()] for the meaning of these
#'   options.
#' @returns A data frame the same type as the first element of `...`.
#' @export
#' @examples
#' df1 <- tibble(x = 1:3)
#' df2 <- tibble(y = 3:1)
#' bind_cols(df1, df2)
#'
#' # Row sizes must be compatible when column-binding
#' try(bind_cols(tibble(x = 1:3), tibble(y = 1:2)))
bind_cols <- function(..., .name_repair = c("unique", "universal", "check_unique", "minimal")) {
  dots <- list2(...)

  dots <- squash_if(dots, vec_is_list)
  dots <- discard(dots, is.null)

  # Strip names off of data frame components so that vec_cbind() unpacks them
  names2(dots)[map_lgl(dots, is.data.frame)] <- ""

  out <- vec_cbind(!!!dots, .name_repair = .name_repair, .call = current_env())
  if (!any(map_lgl(dots, is.data.frame))) {
    out <- as_tibble(out)
  }
  if (length(dots) && is.data.frame(first <- dots[[1L]])) {
    out <- dplyr_reconstruct(out, first)
  }
  out
}

# helpers -----------------------------------------------------------------

dataframe_ish <- function(.x) {
  is.data.frame(.x) || (vec_is(.x) && is_named(.x))
}
