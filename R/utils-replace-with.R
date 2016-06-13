
replace_with <- function(x, i, val, name) {
  if (is.null(val)) {
    return(x)
  }

  check_length(val, x, name)
  check_type(val, x, name)
  check_class(val, x, name)

  if (length(val) == 1L) {
    x[i] <- val
  } else {
    x[i] <- val[i]
  }

  x
}

check_length <- function(x, template, name = deparse(substitute(x))) {
  n <- length(template)
  if (length(x) == n) {
    return()
  }

  if (length(x) == 1L) {
    return()
  }

  stop(name, " is length ", length(x), " not 1 or ", n, ".", call. = FALSE)
}

check_type <- function(x, template, name = deparse(substitute(x))) {
  if (identical(typeof(x), typeof(template))) {
    return()
  }

  stop(
    name, " has type '", typeof(x), "' not '", typeof(template), "'",
    call. = FALSE
  )
}

check_class <- function(x, template, name = deparse(substitute(x))) {
  if (!is.object(x)) {
    return()
  }

  if (identical(class(x), class(template))) {
    return()
  }

  stop(name, " has class ", paste(class(x), collapse = "/"), " not ",
    paste(class(template), collapse = "/"), call. = FALSE)
}
