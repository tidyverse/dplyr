
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
