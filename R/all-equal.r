#' Flexible equality comparison for data frames
#'
#' @description
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("questioning")}
#' `all_equal()` allows you to compare data frames, optionally ignoring
#' row and column names. It is questioning as of dplyr 1.0.0, because it
#' seems to solve a problem that no longer seems that important.
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
#' # By default, ordering of rows and columns ignored
#' all_equal(mtcars, scramble(mtcars))
#'
#' # But those can be overriden if desired
#' all_equal(mtcars, scramble(mtcars), ignore_col_order = FALSE)
#' all_equal(mtcars, scramble(mtcars), ignore_row_order = FALSE)
#'
#' # By default all_equal is sensitive to variable differences
#' df1 <- data.frame(x = "a", stringsAsFactors = FALSE)
#' df2 <- data.frame(x = factor("a"))
#' all_equal(df1, df2)
#' # But you can request dplyr convert similar types
#' all_equal(df1, df2, convert = TRUE)
all_equal <- function(target, current, ignore_col_order = TRUE,
                      ignore_row_order = TRUE, convert = FALSE, ...) {

  equal_data_frame(target, current,
    ignore_col_order = ignore_col_order,
    ignore_row_order = ignore_row_order,
    convert = convert
  )
}

equal_data_frame <- function(x, y, ignore_col_order = TRUE, ignore_row_order = TRUE, convert = FALSE) {
  compat <- is_compatible_data_frame(x, y, ignore_col_order = ignore_col_order, convert = convert)
  if (!isTRUE(compat)) {
    return(compat)
  }

  nrows_x <- nrow(x)
  nrows_y <- nrow(y)
  if (nrows_x != nrows_y) {
    return("Different number of rows")
  }

  if (ncol(x) == 0L) {
    return(TRUE)
  }

  # suppressMessages({
    x <- as_tibble(x, .name_repair = "universal")
    y <- as_tibble(y, .name_repair = "universal")
  # })

  x_split <- vec_split_id_order(x)
  y_split <- vec_split_id_order(y[, names(x), drop = FALSE])

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
    return(paste0("- Rows with difference occurences in x and y: ",
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
