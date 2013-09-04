#' @include translate-sql.r
NULL

#' Create an sql variant.
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
#' @param ... named functions, used to add custom converters from standard
#'  R functions to sql functions.
#' @param .parent the sql variant that this variant should inherit from.
#'   Defaults to \code{\link{base_sql}} which provides a standard set of
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
#' postgres_stat <- sql_variant(
#'   cor = sql_prefix("corr"),
#'   cov = sql_prefix("covar_samp"),
#'   sd =  sql_prefix("stddev_samp"),
#'   var = sql_prefix("var_samp")
#' )
#'
#' to_sql(cor(x, y), postgres_stat)
#' to_sql(sd(income / years), postgres_stat)
#'
#' # Any functions not explicitly listed in the converter will be translated
#' # to sql as is, so you don't need to convert all functions.
#' to_sql(regr_intercept(y, x))
sql_variant <- function(..., .parent = base_sql) {
  list2env(list(...), copy_env(.parent))
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

