#' @include translate-sql-helpers.r

#' @export
#' @rdname to_sql
base_sql <- new.env(parent = emptyenv())
base_sql$`==`    <- sql_infix("=")
base_sql$`!`     <- sql_prefix("not")
base_sql$`&`     <- sql_infix("and")
base_sql$`&&`    <- sql_infix("and")
base_sql$`|`     <- sql_infix("or")
base_sql$`||`    <- sql_infix("or")
base_sql$`^`     <- sql_prefix("power")
base_sql$`%%`    <- sql_infix("%")
base_sql$ceiling <- sql_prefix("ceil")
base_sql$mean    <- sql_prefix("avg")
base_sql$var     <- sql_prefix("variance")
base_sql$tolower <- sql_prefix("lower")
base_sql$toupper <- sql_prefix("upper")
base_sql$nchar   <- sql_prefix("length")
base_sql$sql     <- function(...) sql(...)
base_sql$`(` <- function(x) {
  sql("(", escape(x), ")")
}
base_sql$`{` <- function(x) {
  sql("(", escape(x), ")")
}
base_sql$desc <- function(x) {
  sql(escape(x), " DESC")
}
base_sql$xor <- function(x, y) {
  sprintf("%1$s OR %2$s AND NOT (%1$s AND %2$s)", escape(x), escape(y))
}

base_sql$c     <- function(...) escape(c(...))
base_sql$`:`   <- function(from, to) escape(from:to)

senv <- new.env(parent = emptyenv())
senv$pi <- structure("PI()", class = "sql")
