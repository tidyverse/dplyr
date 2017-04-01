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

check_length <- function(x, template, name, reason = NULL) {
  n <- length(template)
  if (length(x) == n) {
    return()
  }

  if (length(x) == 1L) {
    return()
  }

  if (is.null(reason)) reason <- ""
  else reason <- glue(" ({reason})")

  if (n == 1) {
    glubort("{hdr(name)} should be length one{reason}, got {length(x)}")
  } else {
    glubort("{hdr(name)} should be length {n}{reason} or one, got {length(x)}")
  }
}

check_type <- function(x, template, name) {
  if (identical(typeof(x), typeof(template))) {
    return()
  }

  glubort("{hdr(name)} should be type {type_of(template)}, got {typeof(x)}")
}

check_class <- function(x, template, name) {
  if (!is.object(x)) {
    return()
  }

  if (identical(class(x), class(template))) {
    return()
  }

  glubort("{hdr(name)} should be {fmt_classes(template)}, got {fmt_classes(x)}")
}
