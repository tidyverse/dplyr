#' Create a data table source.
#'
#' A data table source wraps a local data table.
#'
#' @export
#' @param data a data table
#' @aliases .datatable.aware
#' @examples
#' if (require("data.table")) {
#' ds <- source_dt(mtcars)
#' ds
#' as.data.table(ds)
#' as.source(mtcars)
#' }
source_dt <- function(data) {
  if (!require("data.table")) {
    stop("data.table package required to use data tables", call. = FALSE)
  }

  data <- as.data.table(data)
  structure(data, class = c("source_dt", "source", class(data)))
}

#' @S3method as.source data.table
as.source.data.table <- function(x, ...) {
  source_dt(x)
}

#' @S3method source_vars source_dt
source_vars.source_dt <- function(x) copy(names(x))

# Standard data frame methods --------------------------------------------------

#' Coerce data table to source.
#'
#' @export
#' @keywords internal
as.data.table.source_dt <- function(x, keep.rownames = NULL) {
#   if (!is.null(keep.rownames)) {
#     warning("keep.rownames argument ignored", call. = FALSE)
#   }

  NextMethod()
}

#' @S3method as.data.frame source_dt
as.data.frame.source_dt <- function(x, row.names = NULL, optional = FALSE, ...) {
#   if (!is.null(row.names)) warning("row.names argument ignored", call. = FALSE)
#   if (!identical(optional, FALSE)) warning("optional argument ignored", call. = FALSE)
  NextMethod()
}

#' @S3method print source_dt
print.source_dt <- function(x, ...) {
  cat("Source:     local data table ", dim_desc(x), "\n", sep = "")
  cat("\n")
  trunc_mat(x)
}

#' @S3method dimnames source_dt
dimnames.source_dt <- function(x) copy(NextMethod())

#' @S3method head source_dt
head.source_dt <- function(x, ...) as.data.frame(NextMethod())

#' @S3method tail source_dt
tail.source_dt <- function(x, ...) source_df(as.data.frame(NextMethod()))

#' @export
.datatable.aware <- TRUE
