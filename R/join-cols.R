join_cols <- function(x_names, y_names, by = NULL, suffix = c(".x", ".y"), keep = FALSE, error_call = caller_env()) {
  check_duplicate_vars(x_names, "x", error_call = error_call)
  check_duplicate_vars(y_names, "y", error_call = error_call)

  by <- standardise_join_by(by, x_names = x_names, y_names = y_names, error_call = error_call)
  suffix <- standardise_join_suffix(suffix, error_call = error_call)

  x_by <- set_names(match(by$x, x_names), by$x)
  y_by <- set_names(match(by$y, y_names), by$x)

  x_loc <- seq_along(x_names)
  names(x_loc) <- x_names
  if (!keep) {
    # in x_out, key variables need to keep the same name, and aux
    # variables need suffixes for duplicates that appear in y_out
    y_aux <- setdiff(y_names, c(by$x, if (!keep) by$y))
    x_is_aux <- !x_names %in% by$x
    names(x_loc)[x_is_aux] <- add_suffixes(x_names[x_is_aux], c(by$x, y_aux), suffix$x)
  } else {
    # in x_out, key variables and aux variables need suffixes
    # for duplicates that appear in y_out
    names(x_loc) <- add_suffixes(x_names, y_names, suffix$x)
  }

  y_loc <- seq_along(y_names)
  names(y_loc) <- add_suffixes(y_names, x_names, suffix$y)
  if (!keep) {
    y_loc <- y_loc[!y_names %in% by$y]
  }

  # key = named location to use for matching
  # out = named locations to use in output
  list(
    x = list(key = x_by, out = x_loc),
    y = list(key = y_by, out = y_loc)
  )
}

standardise_join_by <- function(by, x_names, y_names, error_call = caller_env()) {
  if (is.null(by)) {
    by <- intersect(x_names, y_names)
    if (length(by) == 0) {
      bullets <- c(
        "`by` must be supplied when `x` and `y` have no common variables.",
        i = "use by = character()` to perform a cross-join."
      )
      abort(bullets, call = error_call)
    }
    by_quoted <- encodeString(by, quote = '"')
    if (length(by_quoted) == 1L) {
      by_code <- by_quoted
    } else {
      by_code <- paste0("c(", paste(by_quoted, collapse = ", "), ")")
    }
    inform(paste0("Joining, by = ", by_code))

    by <- list(x = by, y = by)
  } else if (is.character(by)) {
    by_x <- names(by) %||% by
    by_y <- unname(by)

    # If x partially named, assume unnamed are the same in both tables
    by_x[by_x == ""] <- by_y[by_x == ""]

    by <- list(x = by_x, y = by_y)
  } else if (is.list(by)) {
    # TODO: check lengths
    by <- by[c("x", "y")]
  } else {
    msg <- glue("`by` must be a (named) character vector, list, or NULL, not {friendly_type_of(by)}.")
    abort(msg, call = error_call)
  }

  check_join_vars(by$x, x_names, error_call = error_call)
  check_join_vars(by$y, y_names, error_call = error_call)

  by
}

check_join_vars <- function(vars, names, error_call = caller_env()) {
  if (!is.character(vars)) {
    abort("join columns must be character vectors.", call = error_call)
  }

  na <- is.na(vars)
  if (any(na)) {
    bullets <- c(
      "Join columns must be not NA.",
      x = glue("Problem at position {err_vars(na)}.")
    )
    abort(bullets, call = error_call)
  }

  dup <- duplicated(vars)
  if (any(dup)) {
    bullets <- c(
      "Join columns must be unique.",
      x = glue("Problem at position {err_vars(dup)}.")
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

check_duplicate_vars <- function(vars, input, error_call = caller_env()) {
  dup <- duplicated(vars)
  if (any(dup)) {
    bullets <- c(
      glue("Input columns in `{input}` must be unique."),
      x = glue("Problem with {err_vars(vars[dup])}.")
    )
    abort(bullets, call = error_call)
  }
}

standardise_join_suffix <- function(x, error_call = caller_env()) {
  if (!is.character(x) || length(x) != 2) {
    bullets <- c(
      "`suffix` must be a character vector of length 2.",
      i = glue("`suffix` is {friendly_type_of(x)} of length {length(x)}.")
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
