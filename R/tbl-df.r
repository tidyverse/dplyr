#' Create a data frame tble.
#'
#' A data frame tbl wraps a local data frame. The main advantage to using
#' a \code{tbl_df} over a regular data frame is the printing:
#' tbl objects only print a few rows and all the columns that fit on one
#' screen, providing describing the rest of it as text.
#'
#' @export
#' @param data a data frame
#' @examples
#' ds <- tbl_df(mtcars)
#' ds
#' as.data.frame(ds)
#' as.tbl(mtcars)
tbl_df <- function(data) {
  assert_that(is.data.frame(data))
  if (is.grouped_df(data)) return(ungroup(data))
  
  class(data) <- unique(c("tbl_df", "tbl", class(data)))
  data
}

#' @S3method as.tbl data.frame
as.tbl.data.frame <- function(x, ...) {
  tbl_df(x)
}

#' @S3method tbl_vars data.frame
tbl_vars.data.frame <- function(x) names(x)


#' @S3method same_src data.frame
same_src.data.frame <- function(x, y) {
  is.data.frame(y)
}

# Standard data frame methods --------------------------------------------------

#' @S3method as.data.frame tbl_df
as.data.frame.tbl_df <- function(x, row.names = NULL,
                                            optional = FALSE, ...) {
#   if (!is.null(row.names)) warning("row.names argument ignored", call. = FALSE)
#   if (!identical(optional, FALSE)) warning("optional argument ignored", call. = FALSE)

  class(x) <- setdiff(class(x), c("tbl_df", "tbl"))
  x
}

#' @S3method print tbl_df
print.tbl_df <- function(x, ...) {
  cat("Source: local data frame ", dim_desc(x), "\n", sep = "")
  cat("\n")
  trunc_mat(x)
}
