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

  class(data) <- c("source_df", "source", class(data))
  data
}

#' @S3method as.source data.frame
as.source.data.frame <- function(x, ...) {
  source_df(x)
}

#' @S3method source_vars data.frame
source_vars.data.frame <- function(x) names(x)

# Standard data frame methods --------------------------------------------------

#' @S3method as.data.frame source_df
as.data.frame.source_df <- function(x, row.names = NULL,
                                            optional = FALSE, ...) {
  if (!is.null(row.names)) warning("row.names argument ignored", call. = FALSE)
  if (!identical(optional, FALSE)) warning("optional argument ignored", call. = FALSE)

  class(x) <- setdiff(class(x), c("source_df", "source"))
  x
}

#' @S3method print source_df
print.source_df <- function(x, ...) {
  cat("Source: local data frame ", dim_desc(x), "\n", sep = "")
  cat("\n")
  trunc_mat(x)
}
