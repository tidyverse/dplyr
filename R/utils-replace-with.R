replace_with <- function(x, i, val, name, reason = NULL) {
  if (is.null(val)) {
    return(x)
  }

  check_length(val, x, name, reason)
  check_type(val, x, name)
  check_class(val, x, name)

  if (length(val) == 1L) {
    x[i] <- val
  } else {
    x[i] <- val[i]
  }

  x
}

check_length <- function(x, template, header, reason = NULL) {
  n <- length(template)
  length_x <- length(x)
  if (length_x == n) {
    return()
  }

  if (length_x == 1L) {
    return()
  }

  if (is.null(reason)) reason <- ""
  else reason <- glue(" ({reason})")

  if (n == 1) {
    glubort(header, "must be length one{reason}, not {length_x}")
  } else {
    glubort(header, "must be length {n}{reason} or one, not {length_x}")
  }
}

check_type <- function(x, template, header) {
  if (identical(typeof(x), typeof(template))) {
    return()
  }

  glubort(header, "must be type {type_of(template)}, not {typeof(x)}")
}

check_class <- function(x, template, header) {
  if (!is.object(x)) {
    return()
  }

  if (identical(class(x), class(template))) {
    return()
  }

  glubort(header, "must be {fmt_classes(template)}, not {fmt_classes(x)}")
}
