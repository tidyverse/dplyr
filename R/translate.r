#' Translate an R expression to the language of another data store
#'
#' @inheritParams partial_eval
#' @export
#' @keywords internal
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

# Need way to register UDFs:
#
# f <- function(x) mean(x) + sd(x) ->
# sql_f <- function(x) sql_plus(sql_mean(x), sql_sd(x))
#
# Probably should just be attribute of function.
#
# (also need way for user to add their SQL UDFS)


translate_all <- function(x, source) {
  if (length(x) == 0) return(NULL)

  translator <- source_translator(source)

  vapply(x, eval, env = translator, enclos = emptyenv(),
    FUN.VALUE = character(1))
}
