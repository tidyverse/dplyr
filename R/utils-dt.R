# j <- lazyeval::interp(~ .I[rows], rows = args[[1]]$expr)
# dt_subset(dt, , j, env)

dt_subset <- function(dt, i, j, env = parent.frame()) {
  env <- new.env(parent = env, size = 2L)
  env$`_dt` <- dt
  env$`_vars` <- deparse_all(groups(dt))

  args <- list(
    i = if (missing(i)) quote(expr =) else i,
    j = if (missing(j)) quote(expr =) else j
  )
  call <- substitute(`_dt`[i, j, by = `_vars`], args)

  eval(call, env)
}

dt_env <- function(dt, env) {
  env <- new.env(parent = env, size = 2L)
  env$dt <- dt
  env$vars <- deparse_all(groups(dt))

  env
}

dt_col_compute <- function(dt, call, env = parent.frame()) {
  stopifnot(data.table::is.data.table(dt), is.call(call), is.environment(env))
  env <- dt_env(dt, env)

  wrapper <- substitute(dt[, call], list(call = call))
  if (length(env$vars) > 0) {
    wrapper$by <- quote(vars)
  }

  eval(wrapper, env)$V1
}
