# j <- lazyeval::interp(~ .I[rows], rows = args[[1]]$expr)
# dt_subset(dt, , j, env)

dt_subset <- function(dt, i, j, env = parent.frame(), sd_cols = NULL) {
  env <- new.env(parent = env, size = 2L)
  env$`_dt` <- dt
  env$`_vars` <- deparse_all(groups(dt))

  args <- list(
    i = if (missing(i)) quote(expr =) else dt_replace(i),
    j = if (missing(j)) quote(expr =) else dt_replace(j)
  )

  if (missing(j)) {
    call <- substitute(`_dt`[i], args)
  } else {
    args$groupings <- lazyeval::make_call(quote(list), env$`_vars`)$expr
    call <- substitute(`_dt`[i, j, by = groupings], args)
    call$.SDcols = sd_cols
  }
  # print(call)

  eval(call, env)
}

dt_replace <- function(x) {
  if (is.atomic(x)) {
    x
  } else if (is.name(x)) {
    if (identical(x, quote(.))) {
      quote(.SD)
    } else {
      x
    }
  } else if (is.call(x)) {
    if (identical(x, quote(n()))) {
      quote(.N)
    } else {
      x[] <- lapply(x, dt_replace)
      x
    }
  } else {
    # User supplied incorrect input
    stop("Don't know how to handle type ", typeof(x),
      call. = FALSE)
  }
}
