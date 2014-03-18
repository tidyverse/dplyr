dt_env <- function(dt, env) {
  env <- new.env(parent = env, size = 2L)
  env$dt <- dt
  env$vars <- deparse_all(groups(dt))

  env
}

dt_col_compute <- function(dt, call, env = parent.frame()) {
  stopifnot(is.data.table(dt), is.call(call), is.environment(env))
  env <- dt_env(dt, env)

  wrapper <- substitute(dt[, call], list(call = call))
  if (length(env$vars) > 0) {
    wrapper$by <- quote(vars)
  }

  eval(wrapper, env)$V1
}
