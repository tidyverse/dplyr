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
#' mtcars %>% slice(1L)
#' # Similar to tail(mtcars, 1):
#' mtcars %>% slice(n())
#' mtcars %>% slice(5:n())
#' # Rows can be dropped with negative indices:
#' slice(mtcars, -(1:4))
#'
#' # First and last rows based on existing order
#' mtcars %>% slice_head(n = 5)
#' mtcars %>% slice_tail(n = 5)
#'
#' # Rows with minimum and maximum values of a variable
#' mtcars %>% slice_min(mpg, n = 5)
#' mtcars %>% slice_max(mpg, n = 5)
#'
#' # slice_min() and slice_max() may return more rows than requested
#' # in the presence of ties.
#' mtcars %>% slice_min(cyl, n = 1)
#' # Use with_ties = FALSE to return exactly n matches
#' mtcars %>% slice_min(cyl, n = 1, with_ties = FALSE)
#' # Or use additional variables to break the tie:
#' mtcars %>% slice_min(tibble(cyl, mpg), n = 1)
#'
#' # slice_sample() allows you to random select with or without replacement
#' mtcars %>% slice_sample(n = 5)
#' mtcars %>% slice_sample(n = 5, replace = TRUE)
#'
#' # you can optionally weight by a variable - this code weights by the
#' # physical weight of the cars, so heavy cars are more likely to get
#' # selected
#' mtcars %>% slice_sample(weight_by = wt, n = 5)
#'
#' # Group wise operation ----------------------------------------
#' df <- tibble(
#'   group = rep(c("a", "b", "c"), c(1, 2, 4)),
#'   x = runif(7)
#' )
#'
#' # All slice helpers operate per group, silently truncating to the group
#' # size, so the following code works without error
#' df %>% group_by(group) %>% slice_head(n = 2)
#'
#' # When specifying the proportion of rows to include non-integer sizes
#' # are rounded down, so group a gets 0 rows
#' df %>% group_by(group) %>% slice_head(prop = 0.5)
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

  slice(.data, idx(dplyr::n()), .by = {{ by }})
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

  slice(.data, idx(dplyr::n()), .by = {{ by }})
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
slice_min <- function(.data, order_by, ..., n, prop, by = NULL, with_ties = TRUE, na_rm = FALSE) {
  check_required(order_by)
  check_dot_by_typo(...)
  check_slice_unnamed_n_prop(..., n = n, prop = prop)
  check_bool(with_ties)
  check_bool(na_rm)

  UseMethod("slice_min")
}

#' @export
slice_min.data.frame <- function(.data, order_by, ..., n, prop, by = NULL, with_ties = TRUE, na_rm = FALSE) {
  check_dots_empty0(...)

  size <- get_slice_size(n = n, prop = prop)

  dplyr_local_error_call()
  dplyr_local_slice_by_arg("by")

  slice(
    .data,
    .by = {{ by }},
    local({
      n <- dplyr::n()
      order_by <- {{ order_by }}
      vec_check_size(order_by, size = n)

      slice_rank_idx(
        order_by,
        size(n),
        direction = "asc",
        with_ties = !!with_ties,
        na_rm = !!na_rm
      )
    })
  )
}

#' @export
#' @rdname slice
slice_max <- function(.data, order_by, ..., n, prop, by = NULL, with_ties = TRUE, na_rm = FALSE) {
  check_required(order_by)
  check_dot_by_typo(...)
  check_slice_unnamed_n_prop(..., n = n, prop = prop)
  check_bool(with_ties)
  check_bool(na_rm)

  UseMethod("slice_max")
}

#' @export
slice_max.data.frame <- function(.data, order_by, ..., n, prop, by = NULL, with_ties = TRUE, na_rm = FALSE) {
  check_dots_empty0(...)

  size <- get_slice_size(n = n, prop = prop)

  dplyr_local_error_call()
  dplyr_local_slice_by_arg("by")

  slice(
    .data,
    .by = {{ by }},
    local({
      n <- dplyr::n()
      order_by <- {{ order_by }}
      vec_check_size(order_by, size = n)

      slice_rank_idx(
        order_by,
        size(n),
        direction = "desc",
        with_ties = !!with_ties,
        na_rm = !!na_rm
      )
    })
  )
}

#' @export
#' @rdname slice
#' @param replace Should sampling be performed with (`TRUE`) or without
#'   (`FALSE`, the default) replacement.
#' @param weight_by <[`data-masking`][rlang::args_data_masking]> Sampling
#'   weights. This must evaluate to a vector of non-negative numbers the same
#'   length as the input. Weights are automatically standardised to sum to 1.
slice_sample <- function(.data, ..., n, prop, by = NULL, weight_by = NULL, replace = FALSE) {
  check_dot_by_typo(...)
  check_slice_unnamed_n_prop(..., n = n, prop = prop)
  check_bool(replace)

  UseMethod("slice_sample")
}

#' @export
slice_sample.data.frame <- function(.data, ..., n, prop, by = NULL, weight_by = NULL, replace = FALSE) {
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

slice_rows <- function(data,
                       dots,
                       by,
                       error_call = caller_env(),
                       user_env = caller_env(2)) {
  error_call <- dplyr_error_call(error_call)

  mask <- DataMask$new(data, by, "slice", error_call = error_call)
  on.exit(mask$forget(), add = TRUE)

  chunks <- slice_eval(mask, dots, error_call = error_call, user_env = user_env)
  slice_indices <- slice_combine(chunks, dots, mask = mask, error_call = error_call)

  vec_c(!!!slice_indices, .ptype = integer())
}


is_slice_call <- function(error_call) {
  is_slice <- TRUE
  if (is_environment(error_call) && !identical(error_call$.Generic, "slice")) {
    is_slice <- FALSE
  }
  is_slice
}

slice_eval <- function(mask,
                       dots,
                       error_call = caller_env(),
                       user_env = caller_env(2)) {
  index <- 0L
  impl <- function(...) {
    n <- ...length2()
    out <- vector("list", n)

    for (i in seq_len(n)) {
      index <<- i

      slice_idx <- ...elt2(i)

      if (is.matrix(slice_idx) && ncol(slice_idx) == 1) {
        lifecycle::deprecate_warn(
          when = "1.1.0",
          what = I("Slicing with a 1-column matrix"),
          env = error_call,
          user_env = user_env
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
    }, error = function(cnd) {
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

check_constant <- function(x, name, error_call = caller_env()) {
  withCallingHandlers(force(x), error = function(e) {
    bullets <- c(
      glue("`{name}` must be a constant.")
    )
    abort(bullets, parent = e, call = error_call)
  })
}

check_slice_unnamed_n_prop <- function(..., n, prop, error_call = caller_env()) {
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
get_slice_size <- function(n, prop, allow_outsize = FALSE, error_call = caller_env()) {
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

clamp <- function(min, x, max) {
  if (x < min) {
    min
  } else if (x > max) {
    max
  } else {
    x
  }
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

# Backports for R 3.5.0 utils
...length2 <- function(frame = caller_env()) {
  dots <- env_get(frame, "...")

  if (is_missing(dots)) {
    0L
  } else {
    length(dots)
  }
}
...elt2 <- function(i, frame = caller_env()) {
  eval_bare(sym(paste0("..", i)), frame)
}
