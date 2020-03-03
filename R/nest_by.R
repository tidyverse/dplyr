#' Nest by one or more variables
#'
#' @description
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#' `nest_by()` is closely related to [group_by()]. However, instead of storing
#' the group structure in the metadata, it makes it explicit in the data,
#' giving each group key a single row with a list-column data frames that
#' contains all the other data.
#'
#' @return
#' A [rowwise()] data frame. The output has the following properties:
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
nest_by <- function(.data, ..., .key = "data", .keep = FALSE, .add = FALSE) {
  UseMethod("nest_by")
}

#' @export
nest_by.data.frame <- function(.data, ..., .key = "data", .keep = FALSE, .add = FALSE) {
  data <- group_by(.data, ..., .add = .add)

  keys <- group_keys(data)
  keys <- mutate(keys, !!.key := group_split(data, keep = .keep))
  rowwise(keys, tidyselect::all_of(group_vars(data)))
}
