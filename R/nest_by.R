#' Nest by one or more variables
#'
#' @description
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#' `nest_by()` is closely related to [group_by()]. However, instead of storing
#' the group structure in the metadata, it makes it explicit in the data,
#' giving each group key a single row along with a list-column of data frames
#' that contain all the other data.
#'
#' It returns a [rowwise] data frame, which makes operations on the grouped
#' data particularly elegant. See `vignette("rowwise")` for more details.
#'
#' @details
#' Note that is `df %>% nest_by(x, y)` is roughly equivalent to
#'
#' ```
#' df %>%
#'   group_by(x, y) %>%
#'   summarise(data = list(cur_data())) %>%
#'   rowwise()
#' ```
#'
#' If you want to unnest a nested data frame, you can either use
#' `tidy::unnest()` or take advantage of `summarise()`s mutli-row behaviour:
#'
#' ```
#' nested %>%
#'   summarise(data)
#' ```
#'
#' @return
#' A [rowwise] data frame. The output has the following properties:
#'
#' * The rows come from the underlying [group_keys()].
#' * The columns are the grouping keys plus one list-column of data frames.
#' * Data frame attributes are **not** preserved, because `nest_by()`
#'   fundamentally creates a new data frame.
#' @section Methods:
#' This function is a **generic**, which means that packages can provide
#' implementations (methods) for other classes. See the documentation of
#' individual methods for extra arguments and differences in behaviour.
#'
#' The following methods are currently available in loaded packages:
#' \Sexpr[stage=render,results=rd]{dplyr:::methods_rd("nest_by")}.
#'
#' @inheritParams group_by
#' @param .key Name of the list column
#' @param .keep Should the grouping columns be kept in the list column.
#' @return A tbl with one row per unique combination of the grouping variables.
#' The first columns are the grouping variables, followed by a list column of tibbles
#' with matching rows of the remaining columns.
#' @keywords internal
#' @export
#' @examples
#' # After nesting, you get one row per group
#' iris %>% nest_by(Species)
#' starwars %>% nest_by(species)
#'
#' # The output is grouped by row, which makes modelling particularly easy
#' models <- mtcars %>%
#'   nest_by(cyl) %>%
#'   mutate(model = list(lm(mpg ~ wt, data = data)))
#' models
#'
#' models %>% summarise(rsq = summary(model)$r.squared)
#' # This particularly elegant with the broom functions
#' if (requireNamespace("broom", quietly = TRUE)) {
#'   models %>% summarise(broom::glance(model))
#'   models %>% summarise(broom::tidy(model))
#' }
#'
#' # Note that you can also summarise to unnest the data
#' models %>% summarise(data)
nest_by <- function(.data, ..., .key = "data", .keep = FALSE) {
  UseMethod("nest_by")
}

#' @export
nest_by.data.frame <- function(.data, ..., .key = "data", .keep = FALSE) {
  .data <- group_by(.data, ...)
  nest_by.grouped_df(.data, .key = .key, .keep = .keep)
}

#' @export
nest_by.grouped_df <- function(.data, ..., .key = "data", .keep = FALSE) {
  if (!missing(...)) {
    abort(c(
      "Can't re-group while nesting",
      i = "Either `ungroup()` first or don't supply arguments to `nest_by()"
    ))
  }

  vars <- group_vars(.data)
  keys <- group_keys(.data)
  keys <- mutate(keys, !!.key := group_split(.env$.data, keep = .keep))
  rowwise(keys, tidyselect::all_of(vars))
}
