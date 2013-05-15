#' Create a data frame source.
#'
#' A data frame source wraps a local data frame. The main advantage to using
#' a \code{data_frame_source} over a regular data frame is the printing:
#' source objects only print a few rows and all the columns that fit on one
#' screen, providing describing the rest of it as text.
#'
#' @export
#' @param data a data frame
#' @param name the name of the data frame: used to help you remember where it
#'   came from. If not supplied, taken from the deparsed expression passed
#'   to the \code{data} argument.
#' @examples
#' ds <- data_frame_source(mtcars)
#' ds
#' as.data.frame(ds)
#' as.source(mtcars)
data_frame_source <- function(data, name = NULL) {
  name <- name %||% deparse(substitute(data))
  assert_that(is.data.frame(data), is.string(name))

  structure(list(obj = data, name = name),
    class = c("source_data_frame", "source"))
}

#' @S3method as.source data.frame
as.source.data.frame <- function(x, name = NULL, ...) {
  name <- name %||% deparse(substitute(x))
  data_frame_source(x, name = name)
}

#' @S3method print source_data_frame
print.source_data_frame <- function(x, ...) {
  cat("Source:     local object\n", sep = "")
  cat("Data frame: ", dQuote(x$name), dim_desc(x), "\n", sep = "")
  cat("\n")
  trunc_mat(x)
}

# Methods forwarded on to underlying data frame --------------------------------

#' @S3method head source_data_frame
head.source_data_frame <- function(x, ...) head(x$obj, ...)

#' @S3method tail source_data_frame
tail.source_data_frame <- function(x, ...) tail(x$obj, ...)

#' @S3method as.data.frame source_data_frame
as.data.frame.source_data_frame <- function(x, ...) x$obj

#' @S3method source_vars source_data_frame
source_vars.source_data_frame <- function(x) names(x$obj)

#' @S3method source_name source_data_frame
source_name.source_data_frame <- function(x) x$name

#' @S3method dim source_data_frame
dim.source_data_frame <- function(x) dim(x$obj)

#' @S3method dimnames source_data_frame
dimnames.source_data_frame <- function(x) dimnames(x$obj)
