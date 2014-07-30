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
  if (is.grouped_dt(data)) return(ungroup(data))

  if (is.data.table(data)) data <- copy(data) else data <- as.data.table(data)
  setattr(data, "class", c("tbl_dt", "tbl", "data.table", "data.frame"))
  data
}

#' @export
as.tbl.data.table <- function(x, ...) {
  tbl_dt(x)
}

#' @export
tbl_vars.tbl_dt <- function(x) copy(names(x))

#' @export
groups.tbl_dt <- function(x) {
  NULL
}

#' @export
ungroup.tbl_dt <- function(x) x

#' @export
ungroup.data.table <- function(x) x

#' @export
same_src.tbl_dt <- function(x, y) {
  data.table::is.data.table(y)
}


# Standard data frame methods --------------------------------------------------

#' @export
as.data.frame.tbl_dt <- function(x, row.names = NULL, optional = FALSE, ...) {
#   if (!is.null(row.names)) warning("row.names argument ignored", call. = FALSE)
#   if (!identical(optional, FALSE)) warning("optional argument ignored", call. = FALSE)
  NextMethod()
}

#' @export
#' @rdname dplyr-formatting
print.tbl_dt <- function(x, ..., n = NULL) {
  cat("Source: local data table ", dim_desc(x), "\n", sep = "")
  cat("\n")
  trunc_mat(x, n = n)
}

#' @export
dimnames.tbl_dt <- function(x) copy(NextMethod())

#' @export
head.tbl_dt <- function(x, ...) as.data.frame(NextMethod())

#' @export
tail.tbl_dt <- function(x, ...) tbl_df(as.data.frame(NextMethod()))

#' @export
.datatable.aware <- TRUE
