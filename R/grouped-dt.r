#' A grouped data table.
#'
#' The easiest way to create a grouped data table is to call the \code{group_by}
#' method on a data table or tbl: this will take care of capturing
#' the unevalated expressions for you.
#'
#' @param data a tbl or data frame.
#' @param vars a list of quoted variables.
#' @param copy If \code{TRUE}, will make copy of input.
#' @export
#' @examples
#' if (require("data.table") && require("nycflights13")) {
#' flights_dt <- tbl_dt(flights)
#' group_size(group_by(flights_dt, year, month, day))
#' group_size(group_by(flights_dt, dest))
#'
#' monthly <- group_by(flights_dt, month)
#' summarise(monthly, n = n(), delay = mean(arr_delay))
#' }
grouped_dt <- function(data, vars, copy = TRUE) {
  stopifnot(is.data.table(data))
  if (length(vars) == 0) return(tbl_dt(data))

  is_name <- vapply(vars, is.name, logical(1))
  if (!all(is_name)) {
    stop("Data tables can only be grouped by variables, not expressions",
      call. = FALSE)
  }

  if (copy) {
    data <- copy(data)
  }
  setkeyv(data, deparse_all(vars))
  setattr(data, "vars", vars)
  setattr(data, "class", c("grouped_dt", "tbl_dt", "tbl", class(data)))
  data
}

#' @export
groups.grouped_dt <- function(x) {
  attr(x, "vars")
}

#' @rdname grouped_dt
#' @param x an object to check
#' @export
is.grouped_dt <- function(x) inherits(x, "grouped_dt")

#' @export
print.grouped_dt <- function(x, ..., n = NULL, width = NULL) {
  cat("Source: local data table ", dim_desc(x), "\n", sep = "")
  cat("Groups: ", commas(deparse_all(groups(x))), "\n", sep = "")
  cat("\n")
  trunc_mat(x, n = n, width = width)
}

#' @export
group_size.grouped_dt <- function(x) {
  summarise(x, n = n())$n
}

#' @export
n_groups.grouped_dt <- function(x) {
  env <- dt_env(x, parent.frame())
  nrow(eval(quote(`_dt`[, list(1), by = `_vars`]), env))
}

#' @export
group_by_.data.table <- function(.data, ..., .dots, add = FALSE) {
  groups <- group_by_prepare(.data, ..., .dots = .dots, add = add)
  grouped_dt(groups$data, groups$groups)
}

#' @export
ungroup.grouped_dt <- function(x) {
  setattr(x, "vars", NULL)
  setattr(x, "class", setdiff(class(x), "grouped_dt"))
  x
}
