#' Create a data table tbl.
#'
#' A data table tbl wraps a local data table.
#'
#' @export
#' @param data a data table
#' @aliases .datatable.aware
#' @examples
#' if (require("data.table")) {
#' ds <- tbl_dt(mtcars)
#' ds
#' as.data.table(ds)
#' as.tbl(mtcars)
#' }
tbl_dt <- function(data) {
  if (!require("data.table")) {
    stop("data.table package required to use data tables", call. = FALSE)
  }

  data <- as.data.table(data)
  structure(data, class = c("tbl_dt", "tbl", class(data)))
}

#' @S3method as.tbl data.table
as.tbl.data.table <- function(x, ...) {
  tbl_dt(x)
}

#' @S3method tbl_vars tbl_dt
tbl_vars.tbl_dt <- function(x) copy(names(x))

#' @S3method groups grouped_dt
groups.tbl_dt <- function(x) {
  NULL
}

# Standard data frame methods --------------------------------------------------

#' Coerce data table to tbl.
#'
#' @export
#' @keywords internal
as.data.table.tbl_dt <- function(x, keep.rownames = NULL) {
#   if (!is.null(keep.rownames)) {
#     warning("keep.rownames argument ignored", call. = FALSE)
#   }

  NextMethod()
}

#' @S3method as.data.frame tbl_dt
as.data.frame.tbl_dt <- function(x, row.names = NULL, optional = FALSE, ...) {
#   if (!is.null(row.names)) warning("row.names argument ignored", call. = FALSE)
#   if (!identical(optional, FALSE)) warning("optional argument ignored", call. = FALSE)
  NextMethod()
}

#' @S3method print tbl_dt
print.tbl_dt <- function(x, ...) {
  cat("Source:     local data table ", dim_desc(x), "\n", sep = "")
  cat("\n")
  trunc_mat(x)
}

#' @S3method dimnames tbl_dt
dimnames.tbl_dt <- function(x) copy(NextMethod())

#' @S3method head tbl_dt
head.tbl_dt <- function(x, ...) as.data.frame(NextMethod())

#' @S3method tail tbl_dt
tail.tbl_dt <- function(x, ...) tbl_df(as.data.frame(NextMethod()))

#' @export
.datatable.aware <- TRUE
