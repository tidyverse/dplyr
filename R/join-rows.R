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
                      error_call = caller_env(),
                      user_env = caller_env()) {
  check_dots_empty0(...)

  type <- arg_match(type, error_call = error_call)

  unmatched <- check_unmatched(unmatched, type, error_call = error_call)
  x_unmatched <- unmatched$x
  y_unmatched <- unmatched$y

  if (cross) {
    # TODO: Remove this section when `by = character()` is defunct

    # Rather than matching on key values, match on a proxy where every x value
    # matches every y value. This purposefully does not propagate missings, as
    # missing values aren't considered in a cross-join.
    x_key <- vec_rep(1L, times = vec_size(x_key))
    y_key <- vec_rep(1L, times = vec_size(y_key))

    condition <- "=="
    filter <- "none"

    if (is_null(multiple)) {
      # If user supplied `multiple`, that wins. Otherwise expect multiple.
      multiple <- "all"
    }
  }

  incomplete <- standardise_join_incomplete(type, na_matches, x_unmatched)
  no_match <- standardise_join_no_match(type, x_unmatched)
  remaining <- standardise_join_remaining(type, y_unmatched)
  multiple <- standardise_multiple(multiple, condition, filter, user_env)

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
      abort("`join_cast_common()` should have handled this.", .internal = TRUE)
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
      rethrow_warning_join_matches_multiple(cnd, error_call)
    }
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
      glue("Each row in `x` must match at most 1 row in `y`."),
      i = glue("Row {i} of `x` matches multiple rows.")
    ),
    class = "dplyr_error_join_matches_multiple",
    call = call
  )
}

rethrow_warning_join_matches_multiple <- function(cnd, call) {
  i <- cnd$i

  warn_join(
    message = c(
      glue("Each row in `x` is expected to match at most 1 row in `y`."),
      i = glue("Row {i} of `x` matches multiple rows."),
      i = paste0(
        "If multiple matches are expected, set `multiple = \"all\"` ",
        "to silence this warning."
      )
    ),
    class = "dplyr_warning_join_matches_multiple",
    call = call
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

check_unmatched <- function(unmatched, type, error_call = caller_env()) {
  # Inner joins check both `x` and `y` for unmatched keys, so `unmatched` is
  # allowed to be a character vector of size 2 in that case to check `x` and `y`
  # independently
  inner <- type == "inner"
  n_unmatched <- length(unmatched)

  if (n_unmatched == 1L || (n_unmatched == 2L && inner)) {
    arg_match(
      arg = unmatched,
      values = c("drop", "error"),
      multiple = TRUE,
      error_arg = "unmatched",
      error_call = error_call
    )
  } else if (inner) {
    cli::cli_abort(
      "{.arg unmatched} must be length 1 or 2, not {n_unmatched}.",
      call = error_call
    )
  } else {
    cli::cli_abort(
      "{.arg unmatched} must be length 1, not {n_unmatched}.",
      call = error_call
    )
  }

  if (n_unmatched == 1L) {
    list(x = unmatched, y = unmatched)
  } else {
    list(x = unmatched[[1L]], y = unmatched[[2L]])
  }
}

standardise_join_incomplete <- function(type, na_matches, x_unmatched) {
  if (na_matches == "na") {
    # Comparing missings in incomplete observations overrides the other arguments
    "compare"
  } else if (x_unmatched == "error" && (type == "right" || type == "inner")) {
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

standardise_join_no_match <- function(type, x_unmatched) {
  if (x_unmatched == "error" && (type == "right" || type == "inner")) {
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

standardise_join_remaining <- function(type, y_unmatched) {
  if (y_unmatched == "error" && (type == "left" || type == "inner" || type == "nest")) {
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

standardise_multiple <- function(multiple, condition, filter, user_env) {
  if (!is_null(multiple)) {
    # User supplied value always wins
    return(multiple)
  }

  # "warning" for equi and rolling joins where multiple matches are surprising.
  # "all" for non-equi joins where multiple matches are expected.
  non_equi <- (condition != "==") & (filter == "none")
  if (any(non_equi)) {
    return("all")
  }

  if (is_direct(user_env)) {
    # Direct calls result in a warning when there are multiple matches,
    # because they are likely surprising and the caller will be able to set
    # the `multiple` argument. Indirect calls don't warn, because the caller
    # is unlikely to have access to `multiple` to silence it.
    "warning"
  } else {
    "all"
  }
}

# TODO: Use upstream function when exported from rlang
# `lifecycle:::is_direct()`
is_direct <- function(env) {
  env_inherits_global(env) || from_testthat(env)
}
env_inherits_global <- function(env) {
  # `topenv(emptyenv())` returns the global env. Return `FALSE` in
  # that case to allow passing the empty env when the
  # soft-deprecation should not be promoted to deprecation based on
  # the caller environment.
  if (is_reference(env, empty_env())) {
    return(FALSE)
  }

  is_reference(topenv(env), global_env())
}
# TRUE if we are in unit tests and the package being tested is the
# same as the package that called
from_testthat <- function(env) {
  tested_package <- Sys.getenv("TESTTHAT_PKG")
  if (!nzchar(tested_package)) {
    return(FALSE)
  }

  top <- topenv(env)
  if (!is_namespace(top)) {
    return(FALSE)
  }

  # Test for environment names rather than reference/contents because
  # testthat clones the namespace
  identical(ns_env_name(top), tested_package)
}
