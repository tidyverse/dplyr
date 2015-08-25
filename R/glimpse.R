#' Get a glimpse of your data.
#'
#' This is like a transposed version of print: columns run down the page,
#' and data runs across. This makes it possible to see every column in
#' a data frame. It's a little like \code{\link{str}} applied to a data frame
#' but it tries to show you as much data as possible. (And it always shows
#' the underlying data, even when applied to a remote data source.)
#'
#' @param tbl A data table
#' @param width Width of output: defaults to the width of the console.
#' @export
#' @examples
#' glimpse(mtcars)
#'
#' if (require("RSQLite") && has_lahman("sqlite")) {
#'   batting <- tbl(lahman_sqlite(), "Batting")
#'   glimpse(batting)
#' }
glimpse <- function(tbl, width = getOption("width")) {
  cat("Observations: ", big_mark(nrow(tbl)), "\n", sep = "")
  if (ncol(tbl) == 0) return(invisible())

  cat("Variables: ", big_mark(ncol(tbl)), "\n", sep = "")

  # this is an overestimate, but shouldn't be too expensive.
  # every type needs at least three characters: "x, "
  rows <- as.integer(width / 3)
  df <- as.data.frame(head(tbl, rows))

  var_types <- vapply(df, type_sum, character(1))
  var_names <- paste0("$ ", format(names(df)), " (", var_types, ") ")

  data_width <- width - nchar(var_names) - 2
  length_est <- pmin(ceiling(max(data_width) / 3) + 1, nrow(tbl))

  formatted <- vapply(df, function(x) paste0(format_v(x), collapse = ", "),
    character(1), USE.NAMES = FALSE)
  truncated <- str_trunc(formatted, data_width)

  cat(paste0(var_names, truncated, collapse = "\n"), "\n", sep = "")
}

str_trunc <- function(x, max_width) {
  width <- nchar(x)

  for(i in seq_along(x)) {
    if (width[i] <= max_width[i]) next

    x[i] <- paste0(substr(x[i], 1, max_width[i] - 3), "...")
  }

  x
}

format_v <- function(x) UseMethod("format_v")
#' @export
format_v.default <- function(x) format(x, trim = TRUE, justify = "none")
#' @export
format_v.character <- function(x) encodeString(x, quote = '"')
