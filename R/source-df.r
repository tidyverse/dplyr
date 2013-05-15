#' Create a data frame source.
#'
#' A data frame source wraps a local data frame. The main advantage to using
#' a \code{source_df} over a regular data frame is the printing:
#' source objects only print a few rows and all the columns that fit on one
#' screen, providing describing the rest of it as text.
#'
#' @export
#' @param data a data frame
#' @param name the name of the data frame: used to help you remember where it
#'   came from. If not supplied, taken from the deparsed expression passed
#'   to the \code{data} argument.
#' @examples
#' ds <- source_df(mtcars)
#' ds
#' as.data.frame(ds)
#' as.source(mtcars)
source_df <- function(data, name = NULL) {
  name <- name %||% deparse(substitute(data))
  assert_that(is.data.frame(data), is.string(name))

  structure(list(obj = data, name = name),
    class = c("source_df", "source"))
}

#' @S3method as.source data.frame
as.source.data.frame <- function(x, name = NULL, ...) {
  name <- name %||% deparse(substitute(x))
  source_df(x, name = name)
}

#' @S3method source_vars source_df
source_vars.source_df <- function(x) names(x$obj)

#' @S3method source_vars data.frame
source_vars.data.frame <- function(x) names(x)

# Standard data frame methods --------------------------------------------------

#' @S3method as.data.frame source_df
as.data.frame.source_df <- function(x, row.names = NULL,
                                            optional = NULL, ...) {
  if (!is.null(row.names)) warning("row.names argument ignored", call. = FALSE)
  if (!is.null(optional)) warning("optional argument ignored", call. = FALSE)

  x$obj
}

#' @S3method print source_df
print.source_df <- function(x, ...) {
  cat("Source:     local object\n", sep = "")
  cat("Data frame: ", dQuote(x$name), dim_desc(x), "\n", sep = "")
  cat("\n")
  trunc_mat(x)
}

#' @S3method dimnames source_df
dimnames.source_df <- function(x) dimnames(x$obj)

#' @S3method dim source_df
dim.source_df <- function(x) dim(x$obj)

#' @S3method head source_df
head.source_df <- function(x, ...) head(x$obj, ...)

#' @S3method tail source_df
tail.source_df <- function(x, ...) tail(x$obj, ...)
