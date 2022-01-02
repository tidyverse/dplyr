replace_with <- function(x, i, val, name, reason = NULL, error_call = caller_env()) {
  if (is.null(val)) {
    return(x)
  }

  check_length(val, x, name, reason, error_call = error_call)
  check_type(val, x, name, error_call = error_call)
  check_class(val, x, name, error_call = error_call)

  i[is.na(i)] <- FALSE

  if (vec_size(val) == 1L) {
    vec_assign(x, i, val)
  } else {
    vec_assign(x, i, vec_slice(val, i))
  }
}

fmt_check_length_val <- function(length_x, n, header, reason = NULL) {
  if (all(length_x %in% c(1L, n))) {
    return()
  }

  if (is.null(reason)) {
    reason <- ""
  } else {
    reason <- glue(" ({reason})")
  }

  if (n == 1) {
    glue("{header} must be length 1{reason}, not {commas(length_x)}.")
  } else {
    glue("{header} must be length {n}{reason} or one, not {commas(length_x)}.")
  }
}
check_length_val <- function(length_x, n, header, reason = NULL, error_call = caller_env()) {
  msg <- fmt_check_length_val(length_x, n, header, reason)
  if (length(msg)) {
    abort(msg, call = error_call)
  }
}

check_length <- function(x, template, header, reason = NULL, error_call = caller_env()) {
  check_length_val(vec_size(x), length(template), header, reason, error_call = error_call)
}

check_type <- function(x, template, header, error_call = caller_env()) {
  if (identical(typeof(x), typeof(template))) {
    return()
  }

  msg <- glue("{header} must be {friendly_type_of(template)}, not {friendly_type_of(x)}.")
  abort(msg, call = error_call)
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
  msg <- glue("{header} must have class `{exp_classes}`, not class `{out_classes}`.")
  abort(msg, call = error_call)
}
