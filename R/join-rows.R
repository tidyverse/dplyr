join_rows <- function(x_key,
                      y_key,
                      ...,
                      type = c("inner", "left", "right", "full", "semi", "anti", "nest"),
                      na_matches = "na",
                      condition = "==",
                      filter = "none",
                      cross = FALSE,
                      multiple = "all",
                      unmatched = "drop",
                      relationship = NULL,
                      error_call = caller_env(),
                      user_env = caller_env()) {
  check_dots_empty0(...)

  type <- arg_match0(
    arg = type,
    values = c("inner", "left", "right", "full", "semi", "anti", "nest"),
    error_call = error_call
  )

  unmatched <- check_unmatched(unmatched, type, error_call = error_call)
  x_unmatched <- unmatched$x
  y_unmatched <- unmatched$y

  # TODO: Remove this when `multiple = NULL / "error" / "warning"` is defunct
  if (is_null(multiple)) {
    warn_join_multiple_null(user_env = user_env)
    multiple <- "all"
  } else if (is_string(multiple, "error")) {
    warn_join_multiple("error", user_env = user_env)
  } else if (is_string(multiple, "warning")) {
    warn_join_multiple("warning", user_env = user_env)
  }

  if (cross) {
    # TODO: Remove this section when `by = character()` is defunct

    # Rather than matching on key values, match on a proxy where every x value
    # matches every y value. This purposefully does not propagate missings, as
    # missing values aren't considered in a cross-join.
    x_key <- vec_rep(1L, times = vec_size(x_key))
    y_key <- vec_rep(1L, times = vec_size(y_key))

    condition <- "=="
    filter <- "none"
  }

  if (is_null(relationship)) {
    relationship <- compute_join_relationship(type, condition, cross, user_env = user_env)
  } else {
    relationship <- check_join_relationship(relationship, error_call = error_call)
  }

  incomplete <- standardise_join_incomplete(type, na_matches, x_unmatched)
  no_match <- standardise_join_no_match(type, x_unmatched)
  remaining <- standardise_join_remaining(type, y_unmatched)

  matches <- dplyr_locate_matches(
    needles = x_key,
    haystack = y_key,
    condition = condition,
    filter = filter,
    incomplete = incomplete,
    no_match = no_match,
    remaining = remaining,
    multiple = multiple,
    relationship = relationship,
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
                                 relationship = "none",
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
      relationship = relationship,
      needles_arg = needles_arg,
      haystack_arg = haystack_arg,
      nan_distinct = TRUE
    ),
    vctrs_error_incompatible_type = function(cnd) {
      abort("`join_cast_common()` should have handled this.", .internal = TRUE)
    },
    vctrs_error_matches_overflow = function(cnd) {
      rethrow_error_join_matches_overflow(cnd, error_call)
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
    vctrs_error_matches_relationship_one_to_one = function(cnd) {
      rethrow_error_join_relationship_one_to_one(cnd, error_call)
    },
    vctrs_error_matches_relationship_one_to_many = function(cnd) {
      rethrow_error_join_relationship_one_to_many(cnd, error_call)
    },
    vctrs_error_matches_relationship_many_to_one = function(cnd) {
      rethrow_error_join_relationship_many_to_one(cnd, error_call)
    },
    vctrs_warning_matches_relationship_many_to_many = function(cnd) {
      rethrow_warning_join_relationship_many_to_many(cnd, error_call)
    },
    vctrs_error_matches_multiple = function(cnd) {
      rethrow_error_join_matches_multiple(cnd, error_call)
    },
    vctrs_warning_matches_multiple = function(cnd) {
      rethrow_warning_join_matches_multiple(cnd, error_call)
    }
  )
}

rethrow_error_join_matches_overflow <- function(cnd, call) {
  size <- cnd$size

  stop_join(
    message = c(
      "This join would result in more rows than dplyr can handle.",
      i = glue(
        "{size} rows would be returned. ",
        "2147483647 rows is the maximum number allowed."
      ),
      i = paste0(
        "Double check your join keys. This error commonly occurs due to a ",
        "missing join key, or an improperly specified join condition."
      )
    ),
    class = "dplyr_error_join_matches_overflow",
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

rethrow_error_join_relationship_one_to_one <- function(cnd, call) {
  i <- cnd$i
  which <- cnd$which

  if (which == "needles") {
    x_name <- "x"
    y_name <- "y"
  } else {
    x_name <- "y"
    y_name <- "x"
  }

  stop_join_matches_multiple(
    i = i,
    x_name = x_name,
    y_name = y_name,
    class = "dplyr_error_join_relationship_one_to_one",
    call = call
  )
}

rethrow_error_join_relationship_one_to_many <- function(cnd, call) {
  stop_join_matches_multiple(
    i = cnd$i,
    x_name = "y",
    y_name = "x",
    class = "dplyr_error_join_relationship_one_to_many",
    call = call
  )
}

rethrow_error_join_relationship_many_to_one <- function(cnd, call) {
  stop_join_matches_multiple(
    i = cnd$i,
    x_name = "x",
    y_name = "y",
    class = "dplyr_error_join_relationship_many_to_one",
    call = call
  )
}

rethrow_warning_join_relationship_many_to_many <- function(cnd, call) {
  i <- cnd$i
  j <- cnd$j

  warn_join(
    message = c(
      "Detected an unexpected many-to-many relationship between `x` and `y`.",
      i = glue("Row {i} of `x` matches multiple rows in `y`."),
      i = glue("Row {j} of `y` matches multiple rows in `x`."),
      i = paste0(
        "If a many-to-many relationship is expected, ",
        "set `relationship = \"many-to-many\"` to silence this warning."
      )
    ),
    class = "dplyr_warning_join_relationship_many_to_many",
    call = call
  )

  # Cancel `cnd`
  maybe_restart("muffleWarning")
}

rethrow_error_join_matches_multiple <- function(cnd, call) {
  stop_join_matches_multiple(
    i = cnd$i,
    x_name = "x",
    y_name = "y",
    class = "dplyr_error_join_matches_multiple",
    call = call
  )
}

rethrow_warning_join_matches_multiple <- function(cnd, call) {
  i <- cnd$i

  warn_join(
    message = c(
      glue("Each row in `x` is expected to match at most 1 row in `y`."),
      i = glue("Row {i} of `x` matches multiple rows.")
    ),
    class = "dplyr_warning_join_matches_multiple",
    call = call
  )

  # Cancel `cnd`
  maybe_restart("muffleWarning")
}

stop_join_matches_multiple <- function(i, x_name, y_name, class, call) {
  stop_join(
    message = c(
      glue("Each row in `{x_name}` must match at most 1 row in `{y_name}`."),
      i = glue("Row {i} of `{x_name}` matches multiple rows in `{y_name}`.")
    ),
    class = class,
    call = call
  )
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

compute_join_relationship <- function(type, condition, cross, user_env = caller_env(2)) {
  if (type == "nest") {
    # Not unreasonable to see a many-to-many relationship here, but it can't
    # result in a Cartesian explosion in the result so we don't check for it
    return("none")
  }

  if (type %in% c("semi", "anti")) {
    # Impossible to generate a many-to-many relationship here because we set
    # `multiple = "any"`
    return("none")
  }

  if (cross) {
    # TODO: Remove when `by = character()` is defunct
    # Cross-joins always result in many-to-many relationships
    return("none")
  }

  any_inequality <- any(condition != "==")

  if (any_inequality) {
    # We only check for a many-to-many relationship when doing an equality join,
    # because that is where it is typically unexpected.
    # - Inequality and overlap joins often generate many-to-many relationships
    #   by nature
    # - Rolling joins are a little trickier, but we've decided that not warning
    #   is probably easier to explain. `relationship = "many-to-one"` can always
    #   be used explicitly as needed.
    return("none")
  }

  if (!is_direct(user_env)) {
    # Indirect calls don't warn, because the caller is unlikely to have access
    # to `relationship` to silence it
    return("none")
  }

  "warn-many-to-many"
}

check_join_relationship <- function(relationship, error_call = caller_env()) {
  arg_match0(
    arg = relationship,
    values = c("one-to-one", "one-to-many", "many-to-one", "many-to-many"),
    error_call = error_call
  )
}

# ------------------------------------------------------------------------------

warn_join_multiple <- function(what, user_env = caller_env(2)) {
  what <- glue::glue('Specifying `multiple = "{what}"`')

  lifecycle::deprecate_warn(
    when = "1.1.1",
    what = I(what),
    with = I('`relationship = "many-to-one"`'),
    user_env = user_env,
    always = TRUE
  )
}

warn_join_multiple_null <- function(user_env = caller_env(2)) {
  # Only really needed in case people wrapped `left_join()` and friends and
  # passed the old default of `NULL` through
  lifecycle::deprecate_warn(
    when = "1.1.1",
    what = I("Specifying `multiple = NULL`"),
    with = I('`multiple = "all"`'),
    user_env = user_env,
    always = TRUE
  )
}

# ------------------------------------------------------------------------------

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
