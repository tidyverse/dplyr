dt_env <- function(dt, env, dt_unique, vars_unique) {
  env <- new.env(parent = env, size = 2L)
  assign(dt_unique,dt,env)
  assign(vars_unique, deparse_all(groups(dt)),env)
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


dt_unique_name <- function(prefix, env){
  i <- 0L
  name <- prefix
  while (exists(name,env)) {
    i <- i + 1L
    name <- paste0(prefix,as.character(i))
  }
  name
}
