# Challenge is distinguishing between local and remote vars - maybe best
# solution is to delay translation until we know the data def.  Still probably
# want explicit way of specifying local/remote variables if you want to be
# extra careful though.
#
# Need process to register UDFs:
#
# f <- function(x) mean(x) + sd(x) ->
# sql_f <- function(x) sql_plus(sql_mean(x), sql_sd(x))
#
# (also need way for user to add their SQL UDFS)
#
# Standard set of renaming:
#  * > gt, >= gte, < lt, <= lte, == eq, != neq
#  * ! not, && and, || or
#  * %in% in, %% mod
#  * + plus, - minus, * times, / divide, ...
#  * ( = parens
#
# Otherwise everything gets standard prefix
# Will need to write wrapper functions to check for arity and type

# Recurse through call, replacing function names with their
# equivalents, local variables with their values, and remote vars with
# quoted values
translate_sql <- function(call, source_vars, env = parent.frame()) {
  if (!is.recursive(call)) {
    # Base case

    if (is.atomic(call)) {
      call
    } else if (is.symbol(call)) {
      name <- as.character(call)
      if (name %in% source_vars) {
        substitute(sql_var(var), list(var = as.character(call)))
      } else if (exists(name, env)) {
        get(name, env)
      } else {
        stop(name, " not defined locally or in data source")
      }
    } else {
      message("How did you get here?")
      browser()
    }
  } else {
    # Recursive case
    if (!is.call(call)) {
      message("How did you get here?")
      browser()
    }

    new_name <- trans_name(call[[1]], "sql")
    if (!exists(as.character(new_name))) {
      # Don't know how to turn into sql equivalent - so give up
      stop("Don't know how to translate ", as.character(call[[1]]), " to sql",
        call. = FALSE)
    } else {
      args <- lapply(call[-1], translate_sql,
        source_vars = source_vars, env = env)
      as.call(c(list(new_name), args))
    }

  }

}

