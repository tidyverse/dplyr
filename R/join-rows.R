join_rows <- function(x_key,
                      y_key,
                      ...,
                      type = c("inner", "left", "right", "full", "semi", "anti", "nest"),
                      na_matches = "na",
                      condition = "==",
                      filter = "none",
                      cross = FALSE,
                      multiple = NULL,
                      unmatched = "drop",
                      error_call = caller_env()) {
  check_dots_empty0(...)

  type <- arg_match(type, error_call = error_call)

  if (cross) {
    # Rather than matching on key values, match on a proxy where every x value
    # matches every y value. This purposefully does not propagate missings, as
    # missing values aren't considered in a cross-join.
    x_key <- vec_rep(1L, times = vec_size(x_key))
    y_key <- vec_rep(1L, times = vec_size(y_key))
    condition <- "=="
    filter <- "none"
  }

  incomplete <- standardise_join_incomplete(type, na_matches, unmatched)
  no_match <- standardise_join_no_match(type, unmatched)
  remaining <- standardise_join_remaining(type, unmatched)
  multiple <- standardise_multiple(multiple, condition, filter, cross)

  matches <- dplyr_locate_matches(
    needles = x_key,
    haystack = y_key,
    condition = condition,
    filter = filter,
    incomplete = incomplete,
    no_match = no_match,
    remaining = remaining,
    multiple = multiple,
    needles_arg = "x",
    haystack_arg = "y",
    error_call = error_call
  )

  list(x = matches$needles, y = matches$haystack)
}

dplyr_locate_matches <- function(needles,
                                 haystack,
                                 ...,
                                 condition = "==",
                                 filter = "none",
                                 incomplete = "compare",
                                 no_match = NA_integer_,
                                 remaining = "drop",
                                 multiple = "all",
                                 needles_arg = "",
                                 haystack_arg = "",
                                 error_call = caller_env()) {
  check_dots_empty0(...)

  withCallingHandlers(
    vctrs::vec_locate_matches(
      needles = needles,
      haystack = haystack,
      condition = condition,
      filter = filter,
      incomplete = incomplete,
      no_match = no_match,
      remaining = remaining,
      multiple = multiple,
      needles_arg = needles_arg,
      haystack_arg = haystack_arg,
      nan_distinct = TRUE
    ),
    vctrs_error_incompatible_type = function(cnd) {
      rethrow_error_join_incompatible_type(cnd, error_call)
    },
    vctrs_error_matches_nothing = function(cnd) {
      rethrow_error_join_matches_nothing(cnd, error_call)
    },
    vctrs_error_matches_incomplete = function(cnd) {
      rethrow_error_join_matches_incomplete(cnd, error_call)
    },
    vctrs_error_matches_remaining = function(cnd) {
      rethrow_error_join_matches_remaining(cnd, error_call)
    },
    vctrs_error_matches_multiple = function(cnd) {
      rethrow_error_join_matches_multiple(cnd, error_call)
    },
    vctrs_warning_matches_multiple = function(cnd) {
      rethrow_warning_join_matches_multiple(cnd)
    }
  )
}

rethrow_error_join_incompatible_type <- function(cnd, call) {
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
    class = "dplyr_error_join_incompatible_type",
    call = call
  )
}

rethrow_error_join_matches_nothing <- function(cnd, call) {
  i <- cnd$i

  stop_join(
    message = c(
      "Each row of `x` must have a match in `y`.",
      i = glue("Row {i} of `x` does not have a match.")
    ),
    class = "dplyr_error_join_matches_nothing",
    call = call
  )
}

rethrow_error_join_matches_incomplete <- function(cnd, call) {
  # Only occurs with `na_matches = "never", unmatched = "error"` for
  # right and inner joins, and is a signal that `x` has unmatched incompletes
  # that would result in dropped rows. So really this is a matched-nothing case.
  rethrow_error_join_matches_nothing(cnd, call)
}

rethrow_error_join_matches_remaining <- function(cnd, call) {
  i <- cnd$i

  stop_join(
    message = c(
      "Each row of `y` must be matched by `x`.",
      i = glue("Row {i} of `y` was not matched.")
    ),
    class = "dplyr_error_join_matches_remaining",
    call = call
  )
}

rethrow_error_join_matches_multiple <- function(cnd, call) {
  i <- cnd$i

  stop_join(
    message = c(
      glue("Each row in `x` can match at most 1 row in `y`."),
      i = glue("Row {i} of `x` matches multiple rows.")
    ),
    class = "dplyr_error_join_matches_multiple",
    call = call
  )
}

rethrow_warning_join_matches_multiple <- function(cnd) {
  i <- cnd$i

  warn_join(
    message = c(
      glue("Each row in `x` should match at most 1 row in `y`."),
      i = glue("Row {i} of `x` matches multiple rows."),
      i = paste0(
        "If multiple matches are expected, specify `multiple = \"all\"` in ",
        "the join call to silence this warning."
      )
    ),
    class = "dplyr_warning_join_matches_multiple"
  )

  # Cancel `cnd`
  maybe_restart("muffleWarning")
}

stop_join <- function(message = NULL, class = NULL, ..., call = caller_env()) {
  stop_dplyr(message = message, class = c(class, "dplyr_error_join"), ..., call = call)
}
warn_join <- function(message = NULL, class = NULL, ...) {
  warn_dplyr(message = message, class = c(class, "dplyr_warning_join"), ...)
}

stop_dplyr <- function(message = NULL, class = NULL, ..., call = caller_env()) {
  abort(message = message, class = c(class, "dplyr_error"), ..., call = call)
}
warn_dplyr <- function(message = NULL, class = NULL, ...) {
  warn(message = message, class = c(class, "dplyr_warning"), ...)
}

standardise_join_incomplete <- function(type, na_matches, unmatched) {
  if (na_matches == "na") {
    # Comparing missings in incomplete observations overrides the other arguments
    "compare"
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
    NA_integer_
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

standardise_multiple <- function(multiple, condition, filter, cross) {
  if (!is_null(multiple)) {
    # User supplied value always wins
    return(multiple)
  }

  if (cross) {
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

