join_vars2 <- function(x_names, y_names, by = NULL, suffix = c(".x", ".y")) {
  by <- standardise_join_by(by, x_names = x_names, y_names = y_names)
  suffix <- standardise_join_suffix(suffix)

  x_by <- setNames(seq_along(x_names), x_names)
  x_by <- x_by[x_names %in% by$x]

  x_loc <- seq_along(x_names)
  y_aux <- setdiff(y_names, c(by$x, by$y))
  names(x_loc) <- add_suffixes(x_names, y_aux, suffix$x)

  y_loc <- seq_along(y_names)
  names(y_loc) <- add_suffixes(y_names, x_names, suffix$y)

  # remove join keys from y
  y_loc <- y_loc[!y_names %in% by$y]

  # key = named location to use for matching
  # out = named locations to use in output
  list(
    x = list(key = x_by, out = x_loc),
    y = list(out = y_loc) # key = y_by,
  )
}

standardise_join_by <- function(by, x_names, y_names) {
  if (is.null(by)) {
    by <- intersect(x_names, y_names)
    if (length(by) == 0) {
      bad_args("by", "required, because the data sources have no common variables")
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
    by <- by[c("x", "y")]
  } else {
    bad_args("by", "must be a (named) character vector, list, or NULL, not {friendly_type_of(by)}")
  }

  check_join_vars(by$x, x_names)
  check_join_vars(by$y, y_names)

  by
}

check_join_vars <- function(vars, names) {
  if (!is.character(vars)) {
    abort("join columns must be character vectors")
  }

  na <- is.na(vars)
  if (any(na)) {
    abort(glue_c(
      "Join columns must be not NA",
      x = "Problem at position {vars(na)}"
    ))
  }

  dup <- duplicated(vars)
  if (any(dup)) {
    abort(glue_c(
      "Join columns must be duplicated",
      x = "Problem at position {vars(dup)}"
    ))
  }

  missing <- setdiff(vars, names)
  if (length(missing) > 0) {
    abort(glue_c(
      "Join columns must be present in data",
      x = "Problem with {vars(missing)}"
    ))
  }


}

standardise_join_suffix <- function(x) {
  if (!is.character(x) || length(x) != 2) {
    abort(glue_c(
      "`suffix` must be a character vector of length 2",
      i = "suffix is {friendly_type_of(x)} of length {length(x)}"
    ))
  }

  if (any(is.na(x))) {
    bad_args("suffix", "can't be NA")
  }

  if (all(x == "")) {
    bad_args("suffix", "can't be the empty string for both suffixes")
  }

  list(x = x[[1]], y = x[[2]])
}
