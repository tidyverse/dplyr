#' Get a glimpse of your data.
#'
#' This is like a transposed version of print: columns run down the page,
#' and data runs across. This makes it possible to see every column in
#' a data frame. It's a little like \code{\link{str}} applied to a data frame
#' but it tries to show you as much data as possible. (And it always shows
#' the underlying data, even when applied to a remote data source.)
#'
#' @section S3 methods:
#' \code{glimpse} is an S3 generic with a customised method for \code{tbl}s and
#' \code{data.frames}, and a default method that calls \code{\link{str}}.
#'
#' @param x An object to glimpse at.
#' @param width Width of output: defaults to the width of the console.
#' @param ... Other arguments passed onto individual methods.
#' @return x original x is (invisibly) returned, allowing \code{glimpse} to be
#' used within a data pipe line.
#' @export
#' @examples
#' glimpse(mtcars)
#'
#' \donttest{
#' if (require("RSQLite") && has_lahman("sqlite")) {
#'   batting <- tbl(lahman_sqlite(), "Batting")
#'   glimpse(batting)
#' }
#' }
glimpse <- function(x, width = getOption("width"), ...) {
  UseMethod("glimpse")
}

#' @export
glimpse.tbl <- function(x, width = getOption("width"), ...) {
  cat("Observations: ", big_mark(nrow(x)), "\n", sep = "")
  if (ncol(x) == 0) return(invisible())

  cat("Variables: ", big_mark(ncol(x)), "\n", sep = "")

  if (is.grouped_df(x) || is.grouped_dt(x)) {
    cat("Groups: ", commas(deparse_all(groups(x))), sep = "")
    if (is.grouped_df(x)) {
       grps <- if (is.null(attr(x, "indices"))) "?" else length(attr(x, "indices"))     
       cat(" [", big_mark(grps), "]", sep = "")
    }
    cat("\n")
  }
  
  # this is an overestimate, but shouldn't be too expensive.
  # every type needs at least three characters: "x, "
  rows <- as.integer(width / 3)
  df <- as.data.frame(head(x, rows))

  var_types <- vapply(df, type_sum, character(1))
  var_names <- paste0("$ ", format(names(df)), " (", var_types, ") ")

  data_width <- width - nchar(var_names) - 2

  formatted <- vapply(df, function(x) paste0(format_v(x), collapse = ", "),
    character(1), USE.NAMES = FALSE)
  truncated <- str_trunc(formatted, data_width)

  cat(paste0(var_names, truncated, collapse = "\n"), "\n", sep = "")
  invisible(x)
}

#' @export
glimpse.data.frame <- glimpse.tbl

#' @export
glimpse.default <- function(x, width = getOption("width"), max.level = 3, ...) {
  str(x, width = width, max.level = max.level, ...)
  invisible(x)
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
