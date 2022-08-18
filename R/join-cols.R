join_cols <- function(x_names,
                      y_names,
                      by,
                      ...,
                      suffix = c(".x", ".y"),
                      keep = NULL,
                      error_call = caller_env()) {
  check_dots_empty0(...)

  check_duplicate_vars(x_names, "x", error_call = error_call)
  check_duplicate_vars(y_names, "y", error_call = error_call)

  check_join_vars(by$x, x_names, by$condition, keep, error_call = error_call)
  check_join_vars(by$y, y_names, by$condition, keep, error_call = error_call)

  suffix <- standardise_join_suffix(suffix, error_call = error_call)

  x_by <- set_names(match(by$x, x_names), by$x)
  y_by <- set_names(match(by$y, y_names), by$x)

  x_loc <- seq_along(x_names)
  names(x_loc) <- x_names
  if (is_null(keep)) {
    # In x_out, equi key variables need to keep the same name, and non-equi
    # key variables and aux variables need suffixes for duplicates that appear
    # in y_out. This is equivalent to `keep = TRUE` for the non-equi keys and
    # `keep = FALSE` for the equi keys.
    equi <- by$condition == "=="
    y_aux <- setdiff(y_names, c(by$x[equi], by$y[equi]))
    x_ignore <- by$x[equi]
    x_check <- !x_names %in% x_ignore
    names(x_loc)[x_check] <- add_suffixes(x_names[x_check], c(x_ignore, y_aux), suffix$x)
  } else if (is_false(keep)) {
    # In x_out, key variables need to keep the same name, and aux
    # variables need suffixes for duplicates that appear in y_out
    y_aux <- setdiff(y_names, c(by$x, by$y))
    x_ignore <- by$x
    x_check <- !x_names %in% x_ignore
    names(x_loc)[x_check] <- add_suffixes(x_names[x_check], c(x_ignore, y_aux), suffix$x)
  } else {
    # In x_out, key variables and aux variables need suffixes
    # for duplicates that appear in y_out
    names(x_loc) <- add_suffixes(x_names, y_names, suffix$x)
  }

  y_loc <- seq_along(y_names)
  names(y_loc) <- add_suffixes(y_names, x_names, suffix$y)
  if (is_null(keep)) {
    equi <- by$condition == "=="
    y_ignore <- by$y[equi]
    y_loc <- y_loc[!y_names %in% y_ignore]
  } else if (is_false(keep)) {
    y_ignore <- by$y
    y_loc <- y_loc[!y_names %in% y_ignore]
  }

  # key = named locations to use for matching
  # out = named locations to use in output
  list(
    x = list(key = x_by, out = x_loc),
    y = list(key = y_by, out = y_loc)
  )
}

check_join_vars <- function(vars,
                            names,
                            condition,
                            keep,
                            ...,
                            error_call = caller_env()) {
  check_dots_empty0(...)

  if (!is.character(vars)) {
    abort("Join columns must be character vectors.", call = error_call)
  }

  na <- is.na(vars)
  if (any(na)) {
    bullets <- c(
      "Join columns must be not NA.",
      x = glue("Problem at position {err_vars(na)}.")
    )
    abort(bullets, call = error_call)
  }

  if (!is_false(keep)) {
    # Non-equi columns are allowed to be duplicated when `keep = TRUE/NULL`
    non_equi <- condition != "=="
    vars <- c(vars[!non_equi], unique(vars[non_equi]))
  }

  dup <- duplicated(vars)
  if (any(dup)) {
    vars <- unique(vars[dup])

    bullets <- c(
      "Join columns must be unique.",
      x = glue("Problem with {err_vars(vars)}.")
    )

    abort(bullets, call = error_call)
  }

  missing <- setdiff(vars, names)
  if (length(missing) > 0) {
    bullets <- c(
      "Join columns must be present in data.",
      x = glue("Problem with {err_vars(missing)}.")
    )
    abort(bullets, call = error_call)
  }
}

check_duplicate_vars <- function(vars,
                                 input,
                                 ...,
                                 error_call = caller_env()) {
  check_dots_empty0(...)

  dup <- duplicated(vars)

  if (any(dup)) {
    bullets <- c(
      glue("Input columns in `{input}` must be unique."),
      x = glue("Problem with {err_vars(vars[dup])}.")
    )
    abort(bullets, call = error_call)
  }
}

standardise_join_suffix <- function(x,
                                    ...,
                                    error_call = caller_env()) {
  check_dots_empty0(...)

  if (!is.character(x) || length(x) != 2) {
    bullets <- glue(
      "`suffix` must be a character vector of length 2, not {obj_type_friendly(x)} of length {length(x)}."
    )
    abort(bullets, call = error_call)
  }

  if (any(is.na(x))) {
    msg <- glue("`suffix` can't be NA.")
    abort(msg, call = error_call)
  }

  list(x = x[[1]], y = x[[2]])
}

add_suffixes <- function(x, y, suffix) {
  if (identical(suffix, "")) {
    return(x)
  }

  out <- rep_along(x, na_chr)
  for (i in seq_along(x)) {
    nm <- x[[i]]
    while (nm %in% y || nm %in% out[seq_len(i - 1)]) {
      nm <- paste0(nm, suffix)
    }

    out[[i]] <- nm
  }
  out
}
