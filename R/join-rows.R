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
    multiple = multiple,
    needles_arg = "x",
    haystack_arg = "y"
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
                          multiple = "all",
                          needles_arg = "",
                          haystack_arg = "") {
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
      needles_arg = needles_arg,
      haystack_arg = haystack_arg,
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
  x_name <- cnd$x_arg
  y_name <- cnd$y_arg

  x_type <- vec_ptype_full(cnd$x)
  y_type <- vec_ptype_full(cnd$y)

  stop_join(
    message = c(
      glue("Can't join `{x_name}` with `{y_name}` because of incompatible types."),
      i = glue("`{x_name}` is of type <{x_type}>."),
      i = glue("`{y_name}` is of type <{y_type}>.")
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
    # Matching missing values overrides the other arguments
    "match"
  } else if (unmatched == "error" && (type == "right" || type == "inner")) {
    # Ensure that `x` can't drop rows when `na_matches = "never"`
    "error"
  } else if (type == "inner" || type == "right" || type == "semi") {
    # With these joins and `na_matches = "never"`, drop missings from `x`
    "drop"
  } else if (type == "nest") {
    # Nest join is special and returns `0` which will be sliced out later
    0L
  } else {
    # Otherwise we are keeping all keys from `x`
    "propagate"
  }
}

standardise_join_no_match <- function(type, unmatched) {
  if (unmatched == "error" && (type == "right" || type == "inner")) {
    # Ensure that `x` can't drop rows
    "error"
  } else if (type == "inner" || type == "right" || type == "semi") {
    # With these joins, unmatched keys in `x` get dropped
    "drop"
  } else if (type == "nest") {
    # Nest join is special and returns `0` which will be sliced out later
    0L
  } else {
    # Otherwise we are keeping all keys from `x`
    NA_integer_
  }
}

standardise_join_remaining <- function(type, unmatched) {
  if (unmatched == "error" && (type == "left" || type == "inner" || type == "nest")) {
    # Ensure that `y` can't drop rows
    "error"
  } else if (type == "right" || type == "full") {
    # With these joins, unmatched keys in `y` are kept
    NA_integer_
  } else {
    # Otherwise we drop unmatched keys in `y`
    "drop"
  }
}

standardise_multiple <- function(multiple, condition, filter) {
  if (!is_null(multiple)) {
    # User supplied value always wins
    return(multiple)
  }

  if (is_null(condition)) {
    # Cross join is special cased to never warn
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

