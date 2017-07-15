# nocov - compat-lazyeval (last updated: rlang 0.0.0.9018)

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

  if (missing(lazy)) {
    return(quo())
  }

  coerce_type(lazy, "a quosure",
    formula = as_quosure(lazy, env),
    symbol = ,
    language = new_quosure(lazy, env),
    string = ,
    character = {
      if (warn) warn_text_se()
      parse_quosure(lazy[[1]], env)
    },
    logical = ,
    integer = ,
    double = {
      if (length(lazy) > 1) {
        warn("Truncating vector to length 1")
        lazy <- lazy[[1]]
      }
      new_quosure(lazy, env)
    },
    list =
      coerce_class(lazy, "a quosure",
        lazy = new_quosure(lazy$expr, lazy$env)
      )
  )
}

compat_lazy_dots <- function(dots, env, ..., .named = FALSE) {
  if (missing(dots)) {
    dots <- list()
  }
  if (inherits(dots, c("lazy", "formula"))) {
    dots <- list(dots)
  } else {
    dots <- unclass(dots)
  }
  dots <- c(dots, list(...))

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
