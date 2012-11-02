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

partial_eval <- function(source, call, env = parent.frame()) {
  if (is.atomic(call)) return(call)

  if (is.symbol(call)) {
    # Symbols must be resolveable either locally or remotely

    name <- as.character(call)
    if (name %in% source_vars(source)) {
      substitute(remote_var(var), list(var = as.character(call)))
    } else if (exists(name, env)) {
      substitute(local_value(x), list(x = get(name, env)))
    } else {
      stop(name, " not defined locally or in data source")
    }
  } else if (is.call(call)) {
    # Process call arguments recursively

    call[-1] <- lapply(call[-1], partial_eval, source = source, env = env)
    call
  } else {
    stop("Unknown input type: ", class(call))
  }
}
