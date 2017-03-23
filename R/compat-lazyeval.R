# nocov - compat-lazyeval (rlang 0.0.0.9000)

# This file serves as a reference for compatibility functions for lazyeval.
# Please find the most recent version in rlang's repository.


warn_underscored <- function() {
  return(NULL)
  warn(paste(
    "The underscored versions are deprecated in favour of",
    "tidy evaluation idioms. Please see the documentation",
    "for `quo()` in rlang"
  ))
}
warn_text_se <- function() {
  return(NULL)
  warn("Text parsing is deprecated, please supply an expression or formula")
}

compat_lazy <- function(lazy, env = caller_env(), warn = TRUE) {
  if (warn) warn_underscored()

  coerce_type(lazy, "quosure",
    quosure = lazy,
    symbol = ,
    language = new_quosure(lazy, env),
    string = {
      if (warn) warn_text_se()
      parse_quosure(lazy, env)
    },
    list =
      coerce_class(lazy, "quosure",
        lazy = new_quosure(lazy$expr, lazy$env)
      )
  )
}

compat_lazy_dots <- function(dots, env, ..., .named = FALSE) {
  if (missing(dots)) {
    dots <- list()
  }
  dots <- c(unclass(dots), list(...))
  dots <- as_list(dots)

  warn <- TRUE
  for (i in seq_along(dots)) {
    dots[[i]] <- compat_lazy(dots[[i]], env, warn)
    warn <- FALSE
  }

  named <- have_name(dots)
  if (.named && any(!named)) {
    nms <- map_chr(dots[!named], f_text)
    names(dots)[!named] <- nms
  }

  names(dots) <- names2(dots)
  dots
}

compat_as_lazy <- function(quo) {
  structure(class = "lazy", list(
    expr = f_rhs(quo),
    env = f_env(quo)
  ))
}
compat_as_lazy_dots <- function(...) {
  structure(class = "lazy_dots", map(quos(...), compat_as_lazy))
}


# nocov end
