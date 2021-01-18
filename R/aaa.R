on_load <- function(expr, env = topenv(parent.frame())) {
  expr <- substitute(expr)
  callback <- function() eval_bare(expr, env)
  env$.__rlang_hook__. <- c(env$.__rlang_hook__., list(callback))
}

run_on_load <- function(env = topenv(caller_env())) {
  hook <- env$.__rlang_hook__.
  env_unbind(env, ".__rlang_hook__.")

  for (callback in hook) {
    callback()
  }

  env$.__rlang_hook__. <- NULL
}
