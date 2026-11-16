#' Subset rows using their positions
#'
#' @description
#' `slice()` lets you index rows by their (integer) locations. It allows you
#' to select, remove, and duplicate rows. It is accompanied by a number of
#' helpers for common use cases:
#'
#' * `slice_head()` and `slice_tail()` select the first or last rows.
#' * `slice_sample()` randomly selects rows.
#' * `slice_min()` and `slice_max()` select rows with the smallest or largest
#'   values of a variable.
#'
#' If `.data` is a [grouped_df], the operation will be performed on each group,
#' so that (e.g.) `slice_head(df, n = 5)` will select the first five rows in
#' each group.
#'
#' @details
#' Slice does not work with relational databases because they have no
#' intrinsic notion of row order. If you want to perform the equivalent
#' operation, use [filter()] and [row_number()].
#'
#' For `slice_sample()`, note that the weights provided in `weight_by` are
#' passed through to the `prob` argument of [base::sample.int()]. This means
#' they cannot be used to reconstruct summary statistics from the underlying
#' population. See [this discussion](https://stats.stackexchange.com/q/639211/)
#' for more details.
#'
#' @family single table verbs
#' @inheritParams args_by
#' @inheritParams arrange
#' @inheritParams filter
#' @param ... For `slice()`: <[`data-masking`][rlang::args_data_masking]>
#'   Integer row values.
#'
#'   Provide either positive values to keep, or negative values to drop.
#'   The values provided must be either all positive or all negative.
#'   Indices beyond the number of rows in the input are silently ignored.
#'
#'   For `slice_*()`, these arguments are passed on to methods.
#'
#' @param n,prop Provide either `n`, the number of rows, or `prop`, the
#'   proportion of rows to select. If neither are supplied, `n = 1` will be
#'   used. If `n` is greater than the number of rows in the group
#'   (or `prop > 1`), the result will be silently truncated to the group size.
#'   `prop` will be rounded towards zero to generate an integer number of
#'   rows.
#'
#'   A negative value of `n` or `prop` will be subtracted from the group
#'   size. For example, `n = -2` with a group of 5 rows will select 5 - 2 = 3
#'   rows; `prop = -0.25` with 8 rows will select 8 * (1 - 0.25) = 6 rows.
#' @return
#' An object of the same type as `.data`. The output has the following
#' properties:
#'
#' * Each row may appear 0, 1, or many times in the output.
#' * Columns are not modified.
#' * Groups are not modified.
#' * Data frame attributes are preserved.
#' @section Methods:
#' These function are **generic**s, which means that packages can provide
#' implementations (methods) for other classes. See the documentation of
#' individual methods for extra arguments and differences in behaviour.
#'
#' Methods available in currently loaded packages:
#'
#' * `slice()`: \Sexpr[stage=render,results=rd]{dplyr:::methods_rd("slice")}.
#' * `slice_head()`: \Sexpr[stage=render,results=rd]{dplyr:::methods_rd("slice_head")}.
#' * `slice_tail()`: \Sexpr[stage=render,results=rd]{dplyr:::methods_rd("slice_tail")}.
#' * `slice_min()`: \Sexpr[stage=render,results=rd]{dplyr:::methods_rd("slice_min")}.
#' * `slice_max()`: \Sexpr[stage=render,results=rd]{dplyr:::methods_rd("slice_max")}.
#' * `slice_sample()`: \Sexpr[stage=render,results=rd]{dplyr:::methods_rd("slice_sample")}.
#' @export
#' @examples
#' # Similar to head(mtcars, 1):
#' mtcars |> slice(1L)
#' # Similar to tail(mtcars, 1):
#' mtcars |> slice(n())
#' mtcars |> slice(5:n())
#' # Rows can be dropped with negative indices:
#' slice(mtcars, -(1:4))
#'
#' # First and last rows based on existing order
#' mtcars |> slice_head(n = 5)
#' mtcars |> slice_tail(n = 5)
#'
#' # Rows with minimum and maximum values of a variable
#' mtcars |> slice_min(mpg, n = 5)
#' mtcars |> slice_max(mpg, n = 5)
#'
#' # slice_min() and slice_max() may return more rows than requested
#' # in the presence of ties.
#' mtcars |> slice_min(cyl, n = 1)
#' # Use with_ties = FALSE to return exactly n matches
#' mtcars |> slice_min(cyl, n = 1, with_ties = FALSE)
#' # Or use additional variables to break the tie:
#' mtcars |> slice_min(tibble(cyl, mpg), n = 1)
#'
#' # slice_sample() allows you to random select with or without replacement
#' mtcars |> slice_sample(n = 5)
#' mtcars |> slice_sample(n = 5, replace = TRUE)
#'
#' # slice_sample() can be used to shuffle rows with `prop = 1`
#' mtcars |> slice_sample(prop = 1)
#'
#' # You can optionally weight by a variable - this code weights by the
#' # physical weight of the cars, so heavy cars are more likely to get
#' # selected.
#' mtcars |> slice_sample(weight_by = wt, n = 5)
#'
#' # Group wise operation ----------------------------------------
#' df <- tibble(
#'   group = rep(c("a", "b", "c"), c(1, 2, 4)),
#'   x = runif(7)
#' )
#'
#' # All slice helpers operate per group, silently truncating to the group
#' # size, so the following code works without error
#' df |> group_by(group) |> slice_head(n = 2)
#'
#' # When specifying the proportion of rows to include non-integer sizes
#' # are rounded down, so group a gets 0 rows
#' df |> group_by(group) |> slice_head(prop = 0.5)
#'
#' # Filter equivalents --------------------------------------------
#' # slice() expressions can often be written to use `filter()` and
#' # `row_number()`, which can also be translated to SQL. For many databases,
#' # you'll need to supply an explicit variable to use to compute the row number.
#' filter(mtcars, row_number() == 1L)
#' filter(mtcars, row_number() == n())
#' filter(mtcars, between(row_number(), 5, n()))
slice <- function(.data, ..., .by = NULL, .preserve = FALSE) {
  check_by_typo(...)

  by <- enquo(.by)

  if (!quo_is_null(by) && !is_false(.preserve)) {
    abort("Can't supply both `.by` and `.preserve`.")
  }

  UseMethod("slice")
}

#' @export
slice.data.frame <- function(.data, ..., .by = NULL, .preserve = FALSE) {
  check_dots_unnamed()

  dots <- enquos(...)

  by <- compute_by(
    by = {{ .by }},
    data = .data,
    by_arg = the$slice_by_arg,
    data_arg = ".data"
  )

  loc <- slice_rows(.data, dots, by)
  dplyr_row_slice(.data, loc, preserve = .preserve)
}

#' @export
#' @rdname slice
slice_head <- function(.data, ..., n, prop, by = NULL) {
  check_dot_by_typo(...)
  check_slice_unnamed_n_prop(..., n = n, prop = prop)

  UseMethod("slice_head")
}

#' @export
slice_head.data.frame <- function(.data, ..., n, prop, by = NULL) {
  check_dots_empty0(...)

  size <- get_slice_size(n = n, prop = prop)
  idx <- function(n) {
    seq2(1, size(n))
  }

  dplyr_local_error_call()
  dplyr_local_slice_by_arg("by")

  dot <- new_slice_helper_quo(quo(idx(dplyr::n())), type = "head", size = size)
  slice(.data, !!dot, .by = {{ by }})
}

#' @export
#' @rdname slice
slice_tail <- function(.data, ..., n, prop, by = NULL) {
  check_dot_by_typo(...)
  check_slice_unnamed_n_prop(..., n = n, prop = prop)

  UseMethod("slice_tail")
}

#' @export
slice_tail.data.frame <- function(.data, ..., n, prop, by = NULL) {
  check_dots_empty0(...)

  size <- get_slice_size(n = n, prop = prop)
  idx <- function(n) {
    seq2(n - size(n) + 1, n)
  }

  dplyr_local_error_call()
  dplyr_local_slice_by_arg("by")

  dot <- new_slice_helper_quo(quo(idx(dplyr::n())), type = "tail", size = size)
  slice(.data, !!dot, .by = {{ by }})
}

#' @export
#' @rdname slice
#' @param order_by <[`data-masking`][rlang::args_data_masking]> Variable or
#'   function of variables to order by. To order by multiple variables, wrap
#'   them in a data frame or tibble.
#' @param with_ties Should ties be kept together? The default, `TRUE`,
#'   may return more rows than you request. Use `FALSE` to ignore ties,
#'   and return the first `n` rows.
#' @param na_rm Should missing values in `order_by` be removed from the result?
#'   If `FALSE`, `NA` values are sorted to the end (like in [arrange()]), so
#'   they will only be included if there are insufficient non-missing values to
#'   reach `n`/`prop`.
slice_min <- function(
  .data,
  order_by,
  ...,
  n,
  prop,
  by = NULL,
  with_ties = TRUE,
  na_rm = FALSE
) {
  check_required(order_by)
  check_dot_by_typo(...)
  check_slice_unnamed_n_prop(..., n = n, prop = prop)
  check_bool(with_ties)
  check_bool(na_rm)

  UseMethod("slice_min")
}

#' @export
slice_min.data.frame <- function(
  .data,
  order_by,
  ...,
  n,
  prop,
  by = NULL,
  with_ties = TRUE,
  na_rm = FALSE
) {
  check_dots_empty0(...)

  size <- get_slice_size(n = n, prop = prop)

  dplyr_local_error_call()
  dplyr_local_slice_by_arg("by")

  dot <- slice_rank_helper_quo(
    enquo(order_by),
    size = size,
    direction = "asc",
    with_ties = with_ties,
    na_rm = na_rm
  )
  slice(.data, !!dot, .by = {{ by }})
}

#' @export
#' @rdname slice
slice_max <- function(
  .data,
  order_by,
  ...,
  n,
  prop,
  by = NULL,
  with_ties = TRUE,
  na_rm = FALSE
) {
  check_required(order_by)
  check_dot_by_typo(...)
  check_slice_unnamed_n_prop(..., n = n, prop = prop)
  check_bool(with_ties)
  check_bool(na_rm)

  UseMethod("slice_max")
}

#' @export
slice_max.data.frame <- function(
  .data,
  order_by,
  ...,
  n,
  prop,
  by = NULL,
  with_ties = TRUE,
  na_rm = FALSE
) {
  check_dots_empty0(...)

  size <- get_slice_size(n = n, prop = prop)

  dplyr_local_error_call()
  dplyr_local_slice_by_arg("by")

  dot <- slice_rank_helper_quo(
    enquo(order_by),
    size = size,
    direction = "desc",
    with_ties = with_ties,
    na_rm = na_rm
  )
  slice(.data, !!dot, .by = {{ by }})
}

#' @export
#' @rdname slice
#' @param replace Should sampling be performed with (`TRUE`) or without
#'   (`FALSE`, the default) replacement.
#' @param weight_by <[`data-masking`][rlang::args_data_masking]> Sampling
#'   weights. This must evaluate to a vector of non-negative numbers the same
#'   length as the input. Weights are automatically standardised to sum to 1.
#'   See the `Details` section for more technical details regarding these
#'   weights.
slice_sample <- function(
  .data,
  ...,
  n,
  prop,
  by = NULL,
  weight_by = NULL,
  replace = FALSE
) {
  check_dot_by_typo(...)
  check_slice_unnamed_n_prop(..., n = n, prop = prop)
  check_bool(replace)

  UseMethod("slice_sample")
}

#' @export
slice_sample.data.frame <- function(
  .data,
  ...,
  n,
  prop,
  by = NULL,
  weight_by = NULL,
  replace = FALSE
) {
  check_dots_empty0(...)

  size <- get_slice_size(n = n, prop = prop, allow_outsize = replace)

  dplyr_local_error_call()
  dplyr_local_slice_by_arg("by")

  slice(
    .data,
    .by = {{ by }},
    local({
      weight_by <- {{ weight_by }}

      n <- dplyr::n()
      if (!is.null(weight_by)) {
        vec_check_size(weight_by, size = n)
      }
      sample_int(n, size(n), replace = !!replace, wt = weight_by)
    })
  )
}

# helpers -----------------------------------------------------------------

slice_rows <- function(
  data,
  dots,
  by,
  error_call = caller_env(),
  user_env = caller_env(2)
) {
  error_call <- dplyr_error_call(error_call)

  mask <- DataMask$new(data, by, "slice", error_call = error_call)
  on.exit(mask$forget(), add = TRUE)

  helper <- slice_helper_spec(dots)
  if (!is.null(helper)) {
    return(slice_helper_rows(helper, mask, data, error_call = error_call))
  }

  chunks <- slice_eval(mask, dots, error_call = error_call, user_env = user_env)
  slice_indices <- slice_combine(
    chunks,
    dots,
    mask = mask,
    error_call = error_call
  )

  vec_c(!!!slice_indices, .ptype = integer())
}

is_slice_call <- function(error_call) {
  is_slice <- TRUE
  if (is_environment(error_call) && !identical(error_call$.Generic, "slice")) {
    is_slice <- FALSE
  }
  is_slice
}

slice_eval <- function(
  mask,
  dots,
  error_call = caller_env(),
  user_env = caller_env(2)
) {
  index <- 0L
  impl <- function(...) {
    n <- ...length()
    out <- vector("list", n)

    for (i in seq_len(n)) {
      index <<- i

      slice_idx <- ...elt(i)

      if (is.matrix(slice_idx) && mat_n_col(slice_idx) == 1) {
        lifecycle::deprecate_warn(
          when = "1.1.0",
          what = I("Slicing with a 1-column matrix"),
          env = error_call,
          user_env = user_env,
          always = TRUE,
          id = "dplyr-slice-one-column-matrix"
        )
        slice_idx <- slice_idx[, 1]
      }

      out[[i]] <- vec_as_subscript(
        slice_idx,
        logical = "error",
        character = "error",
        arg = as_label(dots[[i]]),
        call = NULL # error always chained to slice()
      )
    }

    index <<- 0L
    vec_c(!!!out, .ptype = integer())
  }

  withCallingHandlers(
    mask$eval_all(quo(impl(!!!dots))),
    error = function(cnd) {
      if (inherits(cnd, "vctrs_error_subscript")) {
        action <- "process"
      } else {
        action <- "compute"
      }
      if (index && is_slice_call(error_call)) {
        local_error_context(dots, index, mask = mask)
        header <- cnd_bullet_header(action)
      } else {
        header <- glue("Can't {action} indices.")
      }

      bullets <- c(header, i = cnd_bullet_cur_group_label())
      abort(bullets, call = error_call, parent = cnd)
    }
  )
}

slice_combine <- function(chunks, dots, mask, error_call = caller_env()) {
  rows <- mask$get_rows()
  slice_indices <- new_list(length(rows))

  withCallingHandlers(
    for (group in seq_along(rows)) {
      current_rows <- rows[[group]]

      loc <- num_as_location(
        i = chunks[[group]],
        n = length(current_rows),
        zero = "remove",
        oob = "remove",
        missing = "remove",
        arg = as_label(dots[[group]]),
        call = NULL # error always chained to slice()
      )
      grp_loc <- current_rows[loc]
      grp_loc <- grp_loc[!is.na(grp_loc)]

      slice_indices[[group]] <- grp_loc
    },
    error = function(cnd) {
      mask$set_current_group(group)
      bullets <- c(
        "Can't compute indices.",
        i = cnd_bullet_cur_group_label()
      )
      abort(bullets, call = error_call, parent = cnd)
    }
  )

  slice_indices
}

# The `slice_*()` helpers pass `slice()` a quosure that computes the indices of
# one group, so that `slice()` methods which evaluate their dots themselves keep
# working. They also attach a description of the helper to that quosure, which
# `slice.data.frame()` uses to compute the indices of all groups at once rather
# than evaluating R code in every group.
new_slice_helper_quo <- function(quo, ...) {
  attr(quo, "dplyr:::slice_helper") <- list(...)
  quo
}

slice_rank_helper_quo <- function(order_by, size, direction, with_ties, na_rm) {
  quo <- quo(local({
    n <- dplyr::n()
    order_by <- !!order_by
    vec_check_size(order_by, size = n)

    slice_rank_idx(
      order_by,
      size(n),
      direction = !!direction,
      with_ties = !!with_ties,
      na_rm = !!na_rm
    )
  }))

  new_slice_helper_quo(
    quo,
    type = "rank",
    order_by = order_by,
    size = size,
    direction = direction,
    with_ties = with_ties,
    na_rm = na_rm
  )
}

slice_helper_spec <- function(dots) {
  if (length(dots) != 1L) {
    return(NULL)
  }
  attr(dots[[1L]], "dplyr:::slice_helper", exact = TRUE)
}

slice_helper_rows <- function(helper, mask, data, error_call = caller_env()) {
  rows <- mask$get_rows()
  sizes <- list_sizes(rows)
  sizes_out <- as.integer(helper$size(sizes))

  withCallingHandlers(
    switch(
      helper$type,
      head = slice_ends_rows(rows, sizes, sizes_out, tail = FALSE),
      tail = slice_ends_rows(rows, sizes, sizes_out, tail = TRUE),
      rank = slice_rank_rows(helper, mask, data, rows, sizes, sizes_out),
      abort("Unknown slice helper.", .internal = TRUE)
    ),
    error = function(cnd) {
      bullets <- "Can't compute indices."
      if (has_active_group_context(mask)) {
        bullets <- c(bullets, i = cnd_bullet_cur_group_label())
      }
      abort(bullets, call = error_call, parent = cnd)
    }
  )
}

slice_ends_rows <- function(rows, sizes, sizes_out, tail) {
  starts <- cumsum(sizes) - sizes + 1L
  if (tail) {
    starts <- starts + sizes - sizes_out
  }

  locs <- sequence(sizes_out, from = starts)
  list_unchop(rows, ptype = integer())[locs]
}

slice_rank_rows <- function(helper, mask, data, rows, sizes, sizes_out) {
  all_rows <- list_unchop(rows, ptype = integer())
  n_groups <- length(rows)

  # `values` holds the `order_by` values of all groups, one after the other
  values <- NULL
  if (n_groups > 1L) {
    values <- slice_order_by_column(helper$order_by, data, mask, all_rows)
  }

  if (is.null(values)) {
    chunks <- slice_order_by_chunks(helper$order_by, mask, sizes)
    if (n_groups > 1L && slice_chunks_are_combinable(chunks)) {
      values <- list_unchop(chunks)
    }
  }

  if (!is.null(values)) {
    locs <- slice_rank_locs(
      values,
      sizes = sizes,
      sizes_out = sizes_out,
      direction = helper$direction,
      with_ties = helper$with_ties,
      na_rm = helper$na_rm
    )
    return(all_rows[locs])
  }

  # Otherwise rank group by group
  out <- vector("list", n_groups)
  for (i in seq_len(n_groups)) {
    mask$set_current_group(i)
    loc <- slice_rank_idx(
      chunks[[i]],
      sizes_out[[i]],
      direction = helper$direction,
      with_ties = helper$with_ties,
      na_rm = helper$na_rm
    )
    out[[i]] <- rows[[i]][loc]
  }
  list_unchop(out, ptype = integer())
}

# A bare column doesn't need to be evaluated in every group, so its values can
# be taken directly when they are ranked the same way across groups
slice_order_by_column <- function(order_by, data, mask, rows) {
  if (!quo_is_symbol(order_by)) {
    return(NULL)
  }

  name <- as_string(quo_get_expr(order_by))
  loc <- match(name, mask$current_vars())
  if (is.na(loc) || name %in% c(".data", ".env")) {
    return(NULL)
  }

  column <- .subset2(data, loc)
  if (!is_pointwise_orderable(vec_ptype(column))) {
    return(NULL)
  }

  vec_slice(column, rows)
}

slice_order_by_chunks <- function(order_by, mask, sizes) {
  # Evaluated as a nested quosure, like the per-group helper quosure does, so
  # that errors report the same call. Parentheses wouldn't do, because
  # injection removes them.
  chunks <- mask$eval_all(quo({
    !!order_by
  }))
  check_slice_order_by_sizes(chunks, sizes, mask)

  chunks
}

check_slice_order_by_sizes <- function(chunks, sizes, mask) {
  if (list_all_vectors(chunks) && identical(list_sizes(chunks), sizes)) {
    return(invisible())
  }

  # Find the first problematic group and let `vec_check_size()` describe it
  for (i in seq_along(chunks)) {
    mask$set_current_group(i)
    vec_check_size(
      chunks[[i]],
      size = sizes[[i]],
      arg = "order_by",
      call = NULL
    )
  }

  invisible()
}

# Combining the groups mustn't change how their values are ordered, which it
# could if their types differ (e.g. factors with group specific levels)
slice_chunks_are_combinable <- function(chunks) {
  ptype <- vec_ptype(chunks[[1L]])
  if (!is_pointwise_orderable(ptype)) {
    return(FALSE)
  }

  for (chunk in chunks) {
    if (!identical(vec_ptype(chunk), ptype)) {
      return(FALSE)
    }
  }

  TRUE
}

# Whether `vec_rank()` orders each value independently of the other values, so
# that ranking all groups together gives the same result as ranking each group
# on its own. That isn't the case for lists, which are ordered by first
# appearance, or for classes that may define their own order proxy, including
# subclasses of the classes allowed here.
is_pointwise_orderable <- function(ptype) {
  class <- paste(class(ptype), collapse = "/")

  if (is.data.frame(ptype)) {
    if (!class %in% c("data.frame", "tbl_df/tbl/data.frame")) {
      return(FALSE)
    }
    return(all(map_lgl(ptype, is_pointwise_orderable)))
  }

  if (!is.null(dim(ptype))) {
    return(FALSE)
  }

  if (is.object(ptype)) {
    classes <- c(
      "factor",
      "ordered/factor",
      "Date",
      "POSIXct/POSIXt",
      "difftime"
    )
    return(class %in% classes)
  }

  typeof(ptype) %in% c("logical", "integer", "double", "character")
}

# Computes `slice_rank_idx()` for every group at once. `order_by` holds the
# groups' values one after the other, and the result indexes into it.
slice_rank_locs <- function(
  order_by,
  sizes,
  sizes_out,
  direction,
  with_ties,
  na_rm
) {
  groups <- vec_rep_each(seq_along(sizes), sizes)
  offsets <- cumsum(sizes) - sizes

  # Ranking `order_by` on its own first means that ranking within groups only
  # has to rank pairs of integers. Missing values go at the end, as in
  # `slice_rank_idx()`.
  na_value <- if (direction == "asc") "largest" else "smallest"
  keys <- vec_rank(
    order_by,
    ties = "min",
    direction = direction,
    na_value = na_value
  )

  ties <- if (with_ties) "min" else "sequential"
  ranks <- vec_rank(
    new_data_frame(list(group = groups, key = keys)),
    ties = ties
  )

  # Rows of earlier groups rank below every row of the current group, so
  # removing them gives the rank within the group
  keep <- ranks - offsets[groups] <= sizes_out[groups]
  if (na_rm) {
    keep <- keep & vec_detect_complete(order_by)
  }

  locs <- which(keep)
  locs[order(ranks[locs])]
}

check_constant <- function(x, name, error_call = caller_env()) {
  withCallingHandlers(force(x), error = function(e) {
    bullets <- c(
      glue("`{name}` must be a constant.")
    )
    abort(bullets, parent = e, call = error_call)
  })
}

check_slice_unnamed_n_prop <- function(
  ...,
  n,
  prop,
  error_call = caller_env()
) {
  if (!missing(n) || !missing(prop)) {
    return(invisible())
  }

  # Special case to capture e.g. `slice_head(2)`
  # Capture dots as quosures so that we can label
  dots <- enquos(...)

  if (length(dots) == 1L && names2(dots)[[1L]] == "") {
    slice_call <- frame_call(frame = error_call)[[1]]
    slice_call <- as_label(slice_call)
    bullets <- c(
      "`n` must be explicitly named.",
      i = glue("Did you mean `{slice_call}(n = {as_label(dots[[1]])})`?")
    )
    abort(bullets, call = error_call)
  }

  invisible()
}

check_slice_n_prop <- function(n, prop, error_call = caller_env()) {
  if (missing(n) && missing(prop)) {
    list(type = "n", n = 1L)
  } else if (!missing(n) && missing(prop)) {
    n <- check_constant(n, "n", error_call = error_call)
    if (!is_integerish(n, n = 1) || is.na(n)) {
      abort(
        glue("`n` must be a round number, not {obj_type_friendly(n)}."),
        call = error_call
      )
    }
    list(type = "n", n = n)
  } else if (!missing(prop) && missing(n)) {
    prop <- check_constant(prop, "prop", error_call = error_call)
    if (!is.numeric(prop) || length(prop) != 1 || is.na(prop)) {
      abort(
        glue("`prop` must be a number, not {obj_type_friendly(prop)}."),
        call = error_call
      )
    }
    list(type = "prop", prop = prop)
  } else {
    abort("Must supply `n` or `prop`, but not both.", call = error_call)
  }
}

# Always returns an integer between 0 and the group size
get_slice_size <- function(
  n,
  prop,
  allow_outsize = FALSE,
  error_call = caller_env()
) {
  slice_input <- check_slice_n_prop(n, prop, error_call = error_call)

  if (slice_input$type == "n") {
    if (slice_input$n >= 0) {
      if (allow_outsize) {
        body <- expr(!!floor(slice_input$n))
      } else {
        body <- expr(clamp(0, !!floor(slice_input$n), n))
      }
    } else {
      body <- expr(clamp(0, ceiling(n + !!slice_input$n), n))
    }
  } else if (slice_input$type == "prop") {
    if (slice_input$prop >= 0) {
      if (allow_outsize) {
        body <- expr(floor(!!slice_input$prop * n))
      } else {
        body <- expr(clamp(0, floor(!!slice_input$prop * n), n))
      }
    } else {
      body <- expr(clamp(0, ceiling(n + !!slice_input$prop * n), n))
    }
  }

  new_function(pairlist2(n = ), body)
}

# Vectorised over `x` and `max`, so that `get_slice_size()` can compute the size
# of every group at once. An infinite `prop` gives a `NaN` `x` for empty groups,
# which have no rows to keep.
clamp <- function(min, x, max) {
  # Fast path for a single group, as `slice_sample()` computes sizes group by
  # group
  if (length(x) == 1L && length(max) == 1L) {
    if (is.nan(x) || x < min) {
      return(min)
    } else if (x > max) {
      return(max)
    } else {
      return(x)
    }
  }

  out <- pmin(pmax(x, min), max)
  out[is.nan(out)] <- min
  out
}

sample_int <- function(n, size, replace = FALSE, wt = NULL) {
  if (size == 0L) {
    integer(0)
  } else {
    sample.int(n, size, prob = wt, replace = replace)
  }
}

slice_rank_idx <- function(
  order_by,
  size,
  with_ties = TRUE,
  direction = c("asc", "desc"),
  na_rm = FALSE,
  call = caller_env()
) {
  direction <- arg_match0(
    arg = direction,
    values = c("asc", "desc"),
    error_call = call
  )
  # puts missing values at the end
  na_value <- if (direction == "asc") "largest" else "smallest"
  ties <- if (with_ties) "min" else "sequential"

  ranks <- vec_rank(
    x = order_by,
    ties = ties,
    direction = direction,
    na_value = na_value
  )

  keep <- ranks <= size
  if (na_rm) {
    keep[!vec_detect_complete(order_by)] <- FALSE
  }

  which <- which(keep)
  which[order(ranks[which])]
}

on_load({
  # Default used by `slice()`
  the$slice_by_arg <- ".by"
})
dplyr_local_slice_by_arg <- function(by_arg, frame = caller_env()) {
  local_bindings(slice_by_arg = by_arg, .env = the, .frame = frame)
}
