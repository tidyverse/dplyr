#' Flexible equality comparison for data frames
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `all_equal()` allows you to compare data frames, optionally ignoring
#' row and column names. It is deprecated as of dplyr 1.1.0, because it
#' makes it too easy to ignore important differences.
#'
#' @param target,current Two data frames to compare.
#' @param ignore_col_order Should order of columns be ignored?
#' @param ignore_row_order Should order of rows be ignored?
#' @param convert Should similar classes be converted? Currently this will
#'   convert factor to character and integer to double.
#' @param ... Ignored. Needed for compatibility with `all.equal()`.
#' @return `TRUE` if equal, otherwise a character vector describing
#'   the reasons why they're not equal. Use [isTRUE()] if using the
#'   result in an `if` expression.
#' @export
#' @keywords internal
#' @examples
#' scramble <- function(x) x[sample(nrow(x)), sample(ncol(x))]
#'
#' # `all_equal()` ignored row and column ordering by default,
#' # but we now feel that that makes it too easy to make mistakes
#' mtcars2 <- scramble(mtcars)
#' all_equal(mtcars, mtcars2)
#'
#' # Instead, be explicit about the row and column ordering
#' all.equal(
#'   mtcars,
#'   mtcars2[rownames(mtcars), names(mtcars)]
#' )
all_equal <- function(target, current, ignore_col_order = TRUE,
                      ignore_row_order = TRUE, convert = FALSE, ...) {

  lifecycle::deprecate_warn("1.1.0",
    "all_equal()",
    "all.equal()",
    details = "And manually order the rows/cols as needed"
  )

  equal_data_frame(target, current,
    ignore_col_order = ignore_col_order,
    ignore_row_order = ignore_row_order,
    convert = convert
  )
}

equal_data_frame <- function(x, y, ignore_col_order = TRUE, ignore_row_order = TRUE, convert = FALSE) {
  compat <- is_compatible(x, y, ignore_col_order = ignore_col_order, convert = convert)
  if (!isTRUE(compat)) {
    # revert the bulleting from is_compatible()
    return(glue_collapse(compat, sep = "\n"))
  }

  nrows_x <- nrow(x)
  nrows_y <- nrow(y)
  if (nrows_x != nrows_y) {
    return("Different number of rows.")
  }

  if (ncol(x) == 0L) {
    return(TRUE)
  }

  # suppressMessages({
    x <- as_tibble(x, .name_repair = "universal")
    y <- as_tibble(y, .name_repair = "universal")
  # })

  x_split <- dplyr_locate_sorted_groups(x)
  y_split <- dplyr_locate_sorted_groups(y[, names(x), drop = FALSE])

  # keys must be identical
  msg <- ""
  if (any(wrong <- !vec_in(x_split$key, y_split$key))) {
    rows <- sort(map_int(x_split$loc[which(wrong)], function(.x) .x[1L]))
    msg <- paste0(msg, "- Rows in x but not in y: ", glue_collapse(rows, sep = ", "), "\n")
  }

  if (any(wrong <- !vec_in(y_split$key, x_split$key))) {
    rows <- sort(map_int(y_split$loc[which(wrong)], function(.x) .x[1L]))
    msg <- paste0(msg, "- Rows in y but not in x: ", glue_collapse(rows, sep = ", "), "\n")
  }
  if (msg != "") {
    return(msg)
  }

  # keys are identical, check that rows occur the same number of times
  if (any(wrong <- lengths(x_split$loc) != lengths(y_split$loc))) {
    rows <- sort(map_int(x_split$loc[which(wrong)], function(.x) .x[1L]))
    return(paste0("- Rows with difference occurrences in x and y: ",
      glue_collapse(rows, sep = ", "),
      "\n"
    ))
  }

  # then if we care about row order, the id need to be identical
  if (!ignore_row_order && !all(vec_equal(x_split$loc, y_split$loc))) {
    return("Same row values, but different order")
  }

  TRUE
}
