join_rows <- function(x_key,
                      y_key,
                      type = c("inner", "left", "right", "full", "semi", "anti", "nest"),
                      na_matches = "na",
                      condition = "==",
                      filter = "none",
                      multiple = NULL,
                      unmatched = "drop") {
  type <- arg_match(type)

  missing <- standardise_join_missing(type, na_matches, unmatched)
  no_match <- standardise_join_no_match(type, unmatched)
  remaining <- standardise_join_remaining(type, unmatched)
  multiple <- standardise_multiple(multiple, condition, filter)

  matches <- dplyr_matches(
    needles = x_key,
    haystack = y_key,
    condition = condition,
    filter = filter,
    missing = missing,
    no_match = no_match,
    remaining = remaining,
    multiple = multiple
  )

  list(x = matches$needles, y = matches$haystack)
}

dplyr_matches <- function(needles,
                          haystack,
                          ...,
                          condition = "==",
                          filter = "none",
                          missing = "match",
                          no_match = NA_integer_,
                          remaining = "drop",
                          multiple = "all") {
  withCallingHandlers(
    vctrs:::vec_matches(
      needles = needles,
      haystack = haystack,
      ...,
      condition = condition,
      filter = filter,
      missing = missing,
      no_match = no_match,
      remaining = remaining,
      multiple = multiple,
      nan_distinct = TRUE
    ),
    vctrs_error_incompatible_type = rethrow_error_join_incompatible_type,
    vctrs_error_matches_nothing = rethrow_error_join_matches_nothing,
    vctrs_error_matches_missing = rethrow_error_join_matches_missing,
    vctrs_error_matches_remaining = rethrow_error_join_matches_remaining,
    vctrs_error_matches_multiple = rethrow_error_join_matches_multiple,
    vctrs_warning_matches_multiple = rethrow_warning_join_matches_multiple
  )
}

rethrow_error_join_incompatible_type <- function(cnd) {
  rx <- "^[^$]+[$]"
  x_name <- sub(rx, "", cnd$x_arg)
  y_name <- sub(rx, "", cnd$y_arg)

  stop_join(
    message = c(
      glue("Can't join on `x${x_name}` x `y${y_name}` because of incompatible types."),
      i = glue("`x${x_name}` is of type <{x_type}>>.", x_type = vec_ptype_full(cnd$x)),
      i = glue("`y${y_name}` is of type <{y_type}>>.", y_type = vec_ptype_full(cnd$y))
    ),
    class = "dplyr_error_join_incompatible_type"
  )
}

rethrow_error_join_matches_nothing <- function(cnd) {
  i <- cnd$i

  stop_join(
    message = c(
      "Each row of `x` must have a match in `y`.",
      i = glue("Row {i} of `x` does not have a match.")
    ),
    class = "dplyr_error_join_matches_nothing"
  )
}

rethrow_error_join_matches_missing <- function(cnd) {
  # Only occurs with `na_matches = "never", unmatched = "error"` for
  # right and inner joins, and is a signal that `x` has unmatched missings
  # that would result in dropped rows. So really this is a matched-nothing case.
  rethrow_error_join_matches_nothing(cnd)
}

rethrow_error_join_matches_remaining <- function(cnd) {
  i <- cnd$i

  stop_join(
    message = c(
      "Each row of `y` must be matched by `x`.",
      i = glue("Row {i} of `y` was not matched.")
    ),
    class = "dplyr_error_join_matches_remaining"
  )
}

rethrow_error_join_matches_multiple <- function(cnd) {
  i <- cnd$i

  stop_join(
    message = c(
      glue("Each row in `x` can match at most 1 row in `y`."),
      i = glue("Row {i} of `x` matches multiple rows.")
    ),
    class = "dplyr_error_join_matches_multiple"
  )
}

rethrow_warning_join_matches_multiple <- function(cnd) {
  i <- cnd$i

  warn_join(
    message = c(
      glue("Each row in `x` can match at most 1 row in `y`."),
      i = glue("Row {i} of `x` matches multiple rows.")
    ),
    class = "dplyr_warning_join_matches_multiple"
  )

  # Cancel `cnd`
  maybe_restart("muffleWarning")
}

stop_join <- function(message = NULL, class = NULL, ...) {
  stop_dplyr(message = message, class = c(class, "dplyr_error_join"), ...)
}
warn_join <- function(message = NULL, class = NULL, ...) {
  warn_dplyr(message = message, class = c(class, "dplyr_warning_join"), ...)
}

stop_dplyr <- function(message = NULL, class = NULL, ...) {
  abort(message = message, class = c(class, "dplyr_error"), ...)
}
warn_dplyr <- function(message = NULL, class = NULL, ...) {
  warn(message = message, class = c(class, "dplyr_warning"), ...)
}

standardise_join_missing <- function(type, na_matches, unmatched) {
  if (na_matches == "na") {
    return("match")
  }

  if (unmatched == "error" && (type == "right" || type == "inner")) {
    # `x` has the potential to drop rows when `na_matches = "never"`
    return("error")
  }

  if (type == "inner" || type == "right" || type == "semi") {
    return("drop")
  } else {
    return("propagate")
  }
}

standardise_join_no_match <- function(type, unmatched) {
  if (unmatched == "error" && (type == "right" || type == "inner")) {
    # `x` has the potential to drop rows
    return("error")
  }

  if (type == "inner" || type == "right" || type == "semi") {
    return("drop")
  } else if (type == "nest") {
    return(0L)
  } else {
    return(NA_integer_)
  }
}

standardise_join_remaining <- function(type, unmatched) {
  if (unmatched == "error" && (type == "left" || type == "inner" || type == "nest")) {
    # `y` has the potential to drop rows
    return("error")
  }

  if (type == "right" || type == "full") {
    return(NA_integer_)
  } else {
    return("drop")
  }
}

standardise_multiple <- function(multiple, condition, filter) {
  if (!is_null(multiple)) {
    # User supplied
    return(multiple)
  }

  if (is_null(condition)) {
    # Cross join, never warn
    return("all")
  }

  # "warning" for equi and rolling joins where multiple matches are surprising.
  # "all" for non-equi joins where multiple matches are expected.
  non_equi <- (condition != "==") & (filter == "none")

  if (any(non_equi)) {
    "all"
  } else {
    "warning"
  }
}

