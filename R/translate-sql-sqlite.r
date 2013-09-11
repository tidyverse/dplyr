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

  pieces <- lapply(x, to_sql_q, variant = sqlite_sql)
  sql(unlist(pieces))
}
