#' Translate an expression to sql.
#'
#' This is a helper function for convenient exploration. Otherwise conversion
#' normally happens in two distinct phases: first \code{\link{partial_eval}}
#' is called during the building phase, and then \code{\link{to_sql}} is called
#' during the rendering phase.
#'
#' @param expr unevaluated expression to translate
#' @param source data source
#' @export
#' @examples
#' data(baseball, package = "plyr")
#' translate_sql(year < 1890, baseball)
#' translate_sql(year %in% c(1890, 1891), baseball)
#' translate_sql(year %in% 1890:1900, baseball)
#' translate_sql(year >= 1890 & year <= 1900, baseball)
#'
#' x <- 1890
#' translate_sql(year == x, baseball)
#'
#' # By default all computation will happen in sql
#' translate_sql(year < 1889 + 1, baseball)
#' # Use local to force local evaluation
#' translate_sql(year < local(1889 + 1), baseball)
#'
#' # This is also needed if you call a local function:
#' inc <- function(x) x + 1
#' \dontrun{translate_sql(year == inc(x), baseball)}
#' translate_sql(year == local(inc(x)), baseball)
translate_sql <- function(expr, source) {
  expr <- partial_eval(source, substitute(expr))
  to_sql(expr)
}

to_sql <- function(x) {
  if (is.atomic(x)) {
    sql_atomic(x)
  } else if (is.call(x)) {
    # Always evaluate with emptyenv as any non-local references will have
    # been dealt with by partial_eval
    eval(x, sqlite, emptyenv())
  } else if (is.list(x)) {
    lapply(x, to_sql)
  } else {
    stop("Unknown input:", typeof(x), call. = FALSE)
  }
}

# Helper functionals used in multiple sql expressions
sql_infix <- function(op) {
  op <- toupper(op)
  function(x, y) {
    xsql <- to_sql(substitute(x))
    ysql <- to_sql(substitute(y))
    sprintf("%s %s %s", xsql, op, ysql)
  }
}
sql_unary <- function(f) {
  f <- toupper(f)
  function(x) {
    xsql <- to_sql(substitute(x))
    sprintf("%s(%s)", f, xsql)
  }
}
sql_atomic <- function(x) {
  if (is.character(x)) x <- paste0('"', x, '"')
  if (length(x) == 1) return(x)

  paste0("(", paste0(x, collapse = ", "), ")")
}

# Conversion between R and sql function
sqlite <- list(
  remote_var = escape_sql,

  # Operators
  "&" = sql_infix("AND"),
  "|" = sql_infix("OR"),
  "!" =  sql_unary("NOT"),
  xor =  function(x, y) {
    xsql <- to_sql(substitute(x))
    ysql <- to_sql(substitute(y))
    sprintf("%s OR %s AND NOT (%s AND %s)", xsql, ysql, xsql, ysql)
  },

  # Arithmetic
  "+" =  sql_infix("+"),
  "-" =  sql_infix("-"),
  "*" =  sql_infix("*"),
  "/" =  sql_infix("/"),
  "%%" = sql_infix("%"),

  # Math
  "abs" = sql_unary("abs"),
  "round" = function(x, y = 0L) {
    stopifnot(is.numeric(y), length(x) == 1)
    sprintf("ROUND(%s, %d)", to_sql(substitute(x)), y)
  },

  # String
  "tolower" = sql_unary("lower"),
  "toupper" = sql_unary("upper"),
  "nchar" =   sql_unary("length"),

  # Date
  "strftime" = function(x, y) {
    stopifnot(is.character(y), length(y) == 1)
    sprintf("STRFTIME(%s, %s)", to_sql(substitute(x)), y)
  },

  # Logical comparison
  "<" =  sql_infix("<"),
  "<=" = sql_infix("<="),
  ">" =  sql_infix(">"),
  ">=" = sql_infix(">="),
  "!=" = sql_infix("!="),
  "==" = sql_infix("=="),

  # Aggregate functions
  "mean" =  sql_unary("AVG"),
  "count" = function() "COUNT(*)",
  "min" =   sql_unary("MIN"),
  "max" =   sql_unary("MAX"),
  "sum" =   sql_unary("TOTAL"),
  "paste" = function(x, collapse = ",") {
    stopifnot(is.character(sep), length(sep) == 1)
    sprintf("GROUP_CONCAT(%s, %s)", to_sql(substitute(x)), sep)
  },

  # Other special functions
  "(" = function(x) sprintf("(%s)", to_sql(substitute(x))),

  # Special sql functions
  "%in%" =   sql_infix("IN"),
  "%like%" = sql_infix("LIKE"),

  # Functions that get passed through to base R
  "c" = function(...) sql_atomic(c(...)),
  ":" = function(from, to) sql_atomic(from:to)
)
source_translator.source_sql <- function(x) sql
