# Need process to register UDFs:
#
# f <- function(x) mean(x) + sd(x) ->
# sql_f <- function(x) sql_plus(sql_mean(x), sql_sd(x))
#
# (also need way for user to add their SQL UDFS)

#' @examples
#' baseball_s <- sqlite_source("inst/db/baseball.sqlite3", "baseball")
#' teams <- c("RC1", "CL1")
#' expr <- quote(year < 1890 & g > 20 & team %in% teams)
#' partial_eval(baseball_s, expr)
#' translate(baseball_s, expr)
translate <- function(source, call, env = parent.frame()) {
  translator <- source_translator(source)
  call <- partial_eval(source, call, env)
  eval(call, translator, emptyenv())
}
