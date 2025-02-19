join_cols <- function(
  x_names,
  y_names,
  by,
  ...,
  suffix = c(".x", ".y"),
  keep = NULL,
  error_call = caller_env()
) {
  check_dots_empty0(...)

  if (is_false(keep) && any(by$condition != "==")) {
    abort(
      "Can't set `keep = FALSE` when using an inequality, rolling, or overlap join.",
      call = error_call
    )
  }

  check_duplicate_vars(x_names, "x", error_call = error_call)
  check_duplicate_vars(y_names, "y", error_call = error_call)

  check_join_vars(by$x, x_names, by$condition, "x", error_call = error_call)
  check_join_vars(by$y, y_names, by$condition, "y", error_call = error_call)

  suffix <- standardise_join_suffix(suffix, error_call = error_call)

  x_by <- set_names(match(by$x, x_names), by$x)
  y_by <- set_names(match(by$y, y_names), by$y)

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
    names(x_loc)[x_check] <- add_suffixes(
      x_names[x_check],
      c(x_ignore, y_aux),
      suffix$x
    )
  } else if (is_false(keep)) {
    # In x_out, key variables need to keep the same name, and aux
    # variables need suffixes for duplicates that appear in y_out
    y_aux <- setdiff(y_names, c(by$x, by$y))
    x_ignore <- by$x
    x_check <- !x_names %in% x_ignore
    names(x_loc)[x_check] <- add_suffixes(
      x_names[x_check],
      c(x_ignore, y_aux),
      suffix$x
    )
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

check_join_vars <- function(
  vars,
  names,
  condition,
  input,
  ...,
  error_call = caller_env()
) {
  check_dots_empty0(...)

  if (!is.character(vars)) {
    message <- glue("Join columns in `{input}` must be character vectors.")
    abort(message, call = error_call)
  }

  na <- is.na(vars)
  if (any(na)) {
    bullets <- c(
      glue("Join columns in `{input}` can't be `NA`."),
      x = glue("Problem at position {err_vars(na)}.")
    )
    abort(bullets, call = error_call)
  }

  # Columns are allowed to appear in more than one non-equi condition
  # (but not in a mix of non-equi and equi conditions).
  # When non-equi conditions are present, `keep` can't be `FALSE` so we don't
  # have to worry about merging into the same key column multiple times (#6499).
  non_equi <- condition != "=="
  vars <- c(vars[!non_equi], unique(vars[non_equi]))

  dup <- duplicated(vars)
  if (any(dup)) {
    vars <- unique(vars[dup])

    bullets <- c(
      glue("Join columns in `{input}` must be unique."),
      x = glue("Problem with {err_vars(vars)}.")
    )

    abort(bullets, call = error_call)
  }

  missing <- setdiff(vars, names)
  if (length(missing) > 0) {
    bullets <- c(
      glue("Join columns in `{input}` must be present in the data."),
      x = glue("Problem with {err_vars(missing)}.")
    )
    abort(bullets, call = error_call)
  }
}

check_duplicate_vars <- function(vars, input, ..., error_call = caller_env()) {
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

standardise_join_suffix <- function(x, ..., error_call = caller_env()) {
  check_dots_empty0(...)

  if (!is.character(x) || length(x) != 2) {
    bullets <- glue(
      "`suffix` must be a character vector of length 2, not {obj_type_friendly(x)} of length {length(x)}."
    )
    abort(bullets, call = error_call)
  }

  if (any(is.na(x))) {
    msg <- glue("`suffix` can't be `NA`.")
    abort(msg, call = error_call)
  }

  list(x = x[[1]], y = x[[2]])
}

# `join_cols()` checks that `x` and `y` are individually unique,
# which plays into assumptions made here
add_suffixes <- function(x, y, suffix) {
  if (identical(suffix, "")) {
    return(x)
  }

  x <- c(y, x)

  # Never marks the "first" duplicate (i.e. never anything in `y`)
  dup <- duplicated(x)

  while (any(dup)) {
    x[dup] <- paste0(x[dup], suffix)
    dup <- duplicated(x)
  }

  loc <- seq2(length(y) + 1L, length(x))
  x <- x[loc]

  x
}

join_cast_common <- function(x, y, vars, error_call = caller_env()) {
  ptype <- join_ptype_common(x, y, vars, error_call = error_call)
  vec_cast_common(x = x, y = y, .to = ptype, .call = error_call)
}

join_ptype_common <- function(x, y, vars, error_call = caller_env()) {
  # Explicit `x/y_arg = ""` to avoid auto naming in `cnd$x_arg`
  ptype <- try_fetch(
    vec_ptype2(x, y, x_arg = "", y_arg = "", call = error_call),
    vctrs_error_incompatible_type = function(cnd) {
      rethrow_error_join_incompatible_type(cnd, vars, error_call)
    }
  )

  # Finalize unspecified columns (#6804)
  ptype <- vec_ptype_finalise(ptype)

  ptype
}

rethrow_error_join_incompatible_type <- function(cnd, vars, call) {
  x_name <- cnd$x_arg
  y_name <- cnd$y_arg

  # Remap `y_name` to actual name from `y`. Useful for `join_by(a == b)`
  # where the name from `x` is used when determining the common type and will
  # be in the error `cnd`, but we need to tell the user about the name in `y`.
  loc <- match(y_name, names(vars$x$key))
  y_name <- names(vars$y$key)[[loc]]

  x_name <- paste0("x$", x_name)
  y_name <- paste0("y$", y_name)

  x_type <- vec_ptype_full(cnd$x)
  y_type <- vec_ptype_full(cnd$y)

  stop_join(
    message = c(
      glue("Can't join `{x_name}` with `{y_name}` due to incompatible types."),
      i = glue("`{x_name}` is a <{x_type}>."),
      i = glue("`{y_name}` is a <{y_type}>.")
    ),
    class = "dplyr_error_join_incompatible_type",
    call = call
  )
}
