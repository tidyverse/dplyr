#' Create an sql translator
#'
#' When creating a package that maps to a new SQL based src, you'll often
#' want to provide some additional mappings from common R commands to the
#' commands that your tbl provides. These three functions make that
#' easy.
#'
#' @section Helper functions:
#'
#' \code{sql_infix} and \code{sql_prefix} create default SQL infix and prefix
#' functions given the name of the SQL function. They don't perform any input
#' checking, but do correctly escape their input, and are useful for
#' quickly providing default wrappers for a new SQL variant.
#'
#' @keywords internal
#' @param ...,.funs named functions, used to add custom converters from standard
#'  R functions to sql functions. Specify individually in \code{...}, or 
#'  provide a list of \code{.funs}
#' @param .parent the sql variant that this variant should inherit from.
#'   Defaults to \code{base_sql} which provides a standard set of
#'   mappings for the most common operators and functions.
#' @param f the name of the sql function as a string
#' @seealso \code{\link{sql}} for an example of a more customised sql
#'   conversion function.
#' @export
#' @examples
#' # An example of adding some mappings for the statistical functions that
#' # postgresql provides:
#' # \url{http://www.postgresql.org/docs/9.2/static/functions-aggregate.html#FUNCTIONS-AGGREGATE-STATISTICS-TABLE}
#'
#' postgres_stat <- sql_translator(.parent = base_agg,
#'   cor = sql_prefix("corr"),
#'   cov = sql_prefix("covar_samp"),
#'   sd =  sql_prefix("stddev_samp"),
#'   var = sql_prefix("var_samp")
#' )
#'
#' translate_sql(cor(x, y), variant = postgres_stat)
#' translate_sql(sd(income / years), variant = postgres_stat)
#'
#' # Any functions not explicitly listed in the converter will be translated
#' # to sql as is, so you don't need to convert all functions.
#' translate_sql(regr_intercept(y, x), variant = postgres_stat)
sql_variant <- function(scalar = sql_translator(), 
                        aggregate = sql_translator(), 
                        window = sql_translator()) {
  stopifnot(is.environment(scalar))
  stopifnot(is.environment(aggregate))
  stopifnot(is.environment(window))
  
  structure(list(scalar = scalar, aggregate = aggregate, window = window),
    class = "sql_variant")
}

is.sql_variant <- function(x) inherits(x, "sql_variant")

#' @export
print.sql_variant <- function(x, ...) {
  wrap_ls <- function(x, ...) {
    vars <- sort(ls(envir = x))
    wrapped <- strwrap(paste0(vars, collapse = ", "), ...)
    if (identical(wrapped, "")) return()
    paste0(wrapped, "\n", collapse = "")
  }
  
  cat("<sql_variant>\n")
  cat(wrap_ls(x$scalar,    prefix = "scalar:    "))
  cat(wrap_ls(x$aggregate, prefix = "aggregate: "))
  cat(wrap_ls(x$window,    prefix = "window:    "))
}

#' @export
names.sql_variant <- function(x) {
  c(ls(envir = x$scalar), ls(envir = x$aggregate), ls(envir = x$window))
}

#' @export
#' @rdname sql_variant
sql_translator <- function(..., .funs = list(), 
                        .parent = new.env(parent = emptyenv())) {
  funs <- c(list(...), .funs)
  if (length(funs) == 0) return(.parent)
  
  list2env(funs, copy_env(.parent))
}

copy_env <- function(from, to = NULL, parent = parent.env(from)) {
  list2env(as.list(from), envir = to, parent = parent)
}

#' @rdname sql_variant
#' @export
sql_infix <- function(f) {
  assert_that(is.string(f))

  f <- toupper(f)
  function(x, y) {
    build_sql(x, " ", sql(f), " ", y)
  }
}

#' @rdname sql_variant
#' @export
sql_prefix <- function(f) {
  assert_that(is.string(f))

  f <- toupper(f)
  function(...) {
    build_sql(sql(f), list(...))
  }
}

#' @rdname sql_variant
#' @export
sql_not_supported <- function(f) {
  assert_that(is.string(f))

  f <- toupper(f)
  function(...) {
    stop(f, " is not available in this SQL variant", call. = FALSE)
  }
}

win_rank <- function(f) {
  force(f)
  function(order = NULL) {
    over(build_sql(sql(f), list()), partition_group(), order %||% partition_order())
  }
}
win_recycled <- function(f) {
  force(f)
  function(x) {
    over(build_sql(sql(f), list(x)), partition_group(), NULL, frame = c(-Inf, Inf))
  }
}
win_cumulative <- function(f) {
  force(f)
  function(x) {
    over(build_sql(sql(f), list(x)), partition_group(), partition_order(), frame = c(-Inf, 0))
  }
}


# Use a global variable to communicate state of partitioning between 
# tbl and sql translator. This isn't the most amazing design, but it keeps
# things loosely coupled and is straightforward to understand.
partition <- new.env(parent = emptyenv())

set_partition <- function(group_by, order_by) {
  old <- list(partition$group_by, partition$order_by)
  if (is.list(group_by)) {
    order_by <- group_by[[2]]
    group_by <- group_by[[1]]
  }
  
  partition$group_by <- group_by
  partition$order_by <- order_by  
  
  invisible(old)
}

partition_group <- function() partition$group_by
partition_order <- function() partition$order_by