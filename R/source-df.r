#' Create a data frame source.
#'
#' A data frame source wraps a local data frame. The main advantage to using
#' a \code{source_df} over a regular data frame is the printing:
#' source objects only print a few rows and all the columns that fit on one
#' screen, providing describing the rest of it as text.
#'
#' @export
#' @param data a data frame
#' @examples
#' ds <- source_df(mtcars)
#' ds
#' as.data.frame(ds)
#' as.source(mtcars)
source_df <- function(data) {
  assert_that(is.data.frame(data))

  structure(list(obj = data),
    class = c("source_df", "source"))
}

#' @S3method as.source data.frame
as.source.data.frame <- function(x, ...) {
  source_df(x)
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
  cat("Source: local data frame ", dim_desc(x), "\n", sep = "")
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
