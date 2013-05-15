#' Create a data table source.
#'
#' A data table source wraps a local data table.
#'
#' @export
#' @param data a data table
#' @param name the name of the data frame: used to help you remember where it
#'   came from. If not supplied, taken from the deparsed expression passed
#'   to the \code{data} argument.
#' @examples
#' if (require("data.table")) {
#' ds <- source_dt(mtcars)
#' ds
#' as.data.table(ds)
#' as.source(mtcars)
#' }
source_dt <- function(data, name = NULL) {
  if (!require("data.table")) {
    stop("data.table package required to use data tables", call. = FALSE)
  }

  name <- name %||% deparse(substitute(data))
  assert_that(is.string(name))

  data <- as.data.table(data)
  structure(list(obj = data, name = name),
    class = c("source_dt", "source"))
}

#' @S3method as.source data.table
as.source.data.table <- function(x, name = NULL, ...) {
  name <- name %||% deparse(substitute(x))
  source_dt(x, name = name)
}

#' @S3method source_name source_dt
source_name.source_dt <- function(x) x$name

#' @S3method source_vars source_dt
source_vars.source_dt <- function(x) copy(names(x$obj))

# Standard data frame methods --------------------------------------------------

#' Coerce data table to source.
#'
#' @export
#' @keywords internal
as.data.table.source_dt <- function(x, keep.rownames = NULL) {
  if (!is.null(row.names)) warning("row.names argument ignored", call. = FALSE)

  x$obj
}

#' @export
#' @rdname as.data.table.source_dt
as.data.table.source_dt <- function(x, row.names = NULL,
                                            optional = NULL, ...) {
  if (!is.null(row.names)) warning("row.names argument ignored", call. = FALSE)
  if (!is.null(optional)) warning("optional argument ignored", call. = FALSE)

  as.data.table(x$obj)
}

#' @S3method print source_dt
print.source_dt <- function(x, ...) {
  cat("Source:     local object\n", sep = "")
  cat("Data table: ", dQuote(x$name), dim_desc(x), "\n", sep = "")
  cat("\n")
  trunc_mat(x)
}

#' @S3method dimnames source_dt
dimnames.source_dt <- function(x) copy(dimnames(x$obj))

#' @S3method dim source_dt
dim.source_dt <- function(x) dim(x$obj)

#' @S3method head source_dt
head.source_dt <- function(x, ...) as.data.frame(head(x$obj, ...))

#' @S3method tail source_dt
tail.source_dt <- function(x, ...) as.data.frame(tail(x$obj, ...))
