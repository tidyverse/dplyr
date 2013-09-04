#' @include translate-sql-base.r

#' @export
#' @rdname to_sql
sqlite_sql <- sql_variant(
  sd = sql_prefix("stdev")
)

trans_sqlite <- function(x, data, env = NULL) {
  if (is.null(x)) return(NULL)
  
  if (!is.null(env)) {
    x <- partial_eval(x, data, env)
  }
  if (is.list(x)) {
    sql(vapply(x, to_sql_q, variant = sqlite_sql, FUN.VALUE = character(1)))
  } else {
    sql(to_sql_q(x, sqlite_sql))
  }
}
