vec_case_when <- function(...,
                          .default = NULL,
                          .default_arg = ".default",
                          .ptype = NULL,
                          .size = NULL,
                          .call = caller_env()) {
  args <- list2(...)
  args <- name_unnamed_args(args)

  n_args <- length(args)

  if (n_args == 0L) {
    abort("`...` can't be empty.", call = .call)
  }
  if ((n_args %% 2) != 0L) {
    message <- c(
      "`...` must contain an even number of inputs.",
      i = glue("{n_args} inputs were provided.")
    )
    abort(message, call = .call)
  }

  if (!is_string(.default_arg)) {
    abort("`.default_arg` must be a string.", call = .call)
  }

  n_wheres <- n_args / 2L
  loc_wheres <- seq.int(1L, n_args - 1L, by = 2)
  wheres <- args[loc_wheres]
  where_args <- names2(wheres)

  for (i in seq_len(n_wheres)) {
    where <- wheres[[i]]
    where_arg <- where_args[[i]]

    vec_assert(
      x = where,
      ptype = logical(),
      arg = where_arg,
      call = .call
    )
  }

  .size <- vec_size_common(
    !!!wheres,
    .size = .size,
    .call = .call
  )

  n_values <- n_wheres
  loc_values <- loc_wheres + 1L
  values <- args[loc_values]
  value_args <- names2(values)

  # Allow `.default` to participate in common type determination.
  # In terms of size/ptype behavior it is exactly like any other `values` element.
  .ptype <- vec_ptype_common(
    !!!values,
    "{.default_arg}" := .default,
    .ptype = .ptype,
    .call = .call
  )

  # Cast early to generate correct error message indices
  values <- vec_cast_common(
    !!!values,
    .to = .ptype,
    .call = .call
  )

  if (is.null(.default)) {
    .default <- vec_init(.ptype)
  } else {
    .default <- vec_cast(
      x = .default,
      to = .ptype,
      x_arg = .default_arg,
      call = .call
    )
  }

  # Check for correct sizes
  for (i in seq_len(n_wheres)) {
    where <- wheres[[i]]
    where_arg <- where_args[[i]]

    vec_assert(where, size = .size, arg = where_arg, call = .call)
  }

  value_sizes <- list_sizes(values)

  for (i in seq_len(n_values)) {
    value_size <- value_sizes[[i]]

    if (value_size != 1L) {
      value <- values[[i]]
      value_arg <- value_args[[i]]

      vec_assert(value, size = .size, arg = value_arg, call = .call)
    }
  }

  default_size <- vec_size(.default)

  if (default_size != 1L) {
    vec_assert(.default, size = .size, arg = .default_arg, call = .call)
  }

  n_used <- 0L
  locs <- vector("list", n_values)

  # Starts as unused. Any `TRUE` value in `where` flips it to used.
  unused <- vec_rep(TRUE, times = .size)

  # Track unhandled missings using boolean operations.
  # If `FALSE`, any `NA` in `where` flips it to `NA`.
  # Any `TRUE` in `where` overrides both `NA` and `FALSE` to `TRUE`.
  missing <- vec_rep(FALSE, times = .size)

  for (i in seq_len(n_wheres)) {
    if (!any(unused)) {
      break
    }

    where <- wheres[[i]]

    loc <- unused & where
    missing <- missing | where

    loc <- which(loc)
    locs[[i]] <- loc

    unused[loc] <- FALSE
    n_used <- n_used + 1L
  }

  if (n_used == n_wheres) {
    # If all of the `where` conditions are used,
    # then we check if we need `missing` or `.default`

    missing <- vec_equal_na(missing)

    if (any(missing)) {
      missing <- which(missing)

      n_used <- n_used + 1L
      n_values <- n_values + 1L
      locs[[n_values]] <- missing
      values[[n_values]] <- vec_init(.ptype)
      value_sizes[[n_values]] <- 1L

      # Missing locations don't count as unused
      unused[missing] <- FALSE
    }

    if (any(unused)) {
      unused <- which(unused)

      n_used <- n_used + 1L
      n_values <- n_values + 1L
      locs[[n_values]] <- unused
      values[[n_values]] <- .default
      value_sizes[[n_values]] <- default_size
    }
  }

  for (i in seq_len(n_used)) {
    loc <- locs[[i]]
    value <- values[[i]]
    value_size <- value_sizes[[i]]

    if (value_size == 1L) {
      # Recycle "up"
      value <- vec_recycle(value, size = vec_size(loc))
    } else {
      # Slice "down"
      value <- vec_slice(value, loc)
    }

    values[[i]] <- value
  }

  # Remove names used for error messages. We don't want them in the result.
  values <- unname(values)

  if (n_used != n_values) {
    # Trim to only what will be used to fill the result
    seq_used <- seq_len(n_used)
    values <- values[seq_used]
    locs <- locs[seq_used]
  }

  vec_unchop(
    x = values,
    indices = locs,
    ptype = .ptype
  )
}

name_unnamed_args <- function(args) {
  names <- names2(args)
  names <- name_unnamed(names)
  names(args) <- names
  args
}

name_unnamed <- function(names) {
  if (is.null(names)) {
    return(names)
  }

  unnamed <- names == ""

  if (any(unnamed)) {
    unnamed <- which(unnamed)
    names[unnamed] <- paste0("..", unnamed)
  }

  names
}
