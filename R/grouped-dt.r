#' A grouped data table.
#'
#' The easiest way to create a grouped data table is to call the \code{group_by}
#' method on a data table or data source: this will take care of capturing
#' the unevalated expressions for you.
#'
#' @param data a data source or data frame.
#' @param vars a list of quoted variables.
#' @param name data source name.
grouped_dt <- function(data, vars, name = NULL) {
  stopifnot(is.data.table(data))

  is_name <- vapply(vars, is.name, logical(1))
  if (!all(is_name)) {
    stop("Data tables can only be grouped by variables, not expressions",
      call. = FALSE)
  }
  setkeyv(data, deparse_all(vars))

  if (is.null(name)) {
    if (is.source(data)) {
      name <- data$name
    } else {
      name <- deparse(substitute(data))
    }
  }
  assert_that(is.string(name))

  data <- list(obj = data, name = name, vars = vars)
  structure(data, class = c("grouped_dt", "source_dt", "source"))
}

#' @rdname grouped_dt
#' @export
is.grouped_dt <- function(x) inherits(x, "grouped_dt")

#' @S3method print grouped_dt
print.grouped_dt <- print.grouped_df

#' @method group_by data.frame
#' @export
#' @rdname grouped_dt
group_by.data.table <- function(x, ..., name = NULL) {
  name <- name %||% substitute(x)
  vars <- dots(...)

  grouped_dt(x, vars, name = name)
}

#' @method group_by source_dt
#' @export
#' @rdname grouped_dt
group_by.source_dt <- function(x, ...) {
  vars <- dots(...)
  grouped_dt(x$obj, vars, name = obj$name)
}

#' @method ungroup grouped_dt
ungroup.grouped_dt <- function(x) {
  source_dt(x$obj, x$name)
}

