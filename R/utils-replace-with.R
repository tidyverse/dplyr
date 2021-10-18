replace_with <- function(x, i, val, name, reason = NULL, error_call = caller_env()) {
  if (is.null(val)) {
    return(x)
  }

  check_length(val, x, name, reason)
  check_type(val, x, name, error_call = error_call)
  check_class(val, x, name, error_call = error_call)

  i[is.na(i)] <- FALSE

  if (length(val) == 1L) {
    x[i] <- val
  } else {
    x[i] <- val[i]
  }

  x
}

check_length <- function(x, template, header, reason = NULL) {
  check_length_val(length(x), length(template), header, reason)
}

check_length_col <- function(length_x, n, name, reason = NULL, .abort = abort) {
  check_length_val(length_x, n, fmt_cols(name), reason, .abort = .abort)
}

check_length_val <- function(length_x, n, header, reason = NULL, .abort = abort) {
  if (all(length_x %in% c(1L, n))) {
    return()
  }

  if (is.null(reason)) {
    reason <- ""
  } else {
    reason <- glue(" ({reason})")
  }

  if (n == 1) {
    glubort(header, "must be length 1{reason}, not {commas(length_x)}.", .abort = .abort)
  } else {
    glubort(header, "must be length {n}{reason} or one, not {commas(length_x)}.", .abort = .abort)
  }
}

check_type <- function(x, template, header, error_call = caller_env()) {
  if (identical(typeof(x), typeof(template))) {
    return()
  }

  abort(
    glue("{header} must be {friendly_type_of(template)}, not {friendly_type_of(x)}."),
    call = error_call
  )
}

check_class <- function(x, template, header, error_call = caller_env()) {
  if (!is.object(x)) {
    return()
  }

  if (identical(class(x), class(template))) {
    return()
  }

  exp_classes <- fmt_classes(template)
  out_classes <- fmt_classes(x)
  abort(
    glue("{header} must have class `{exp_classes}`, not class `{out_classes}`."),
    call = error_call
  )
}
