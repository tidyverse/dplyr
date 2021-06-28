#' Subset rows using their positions
#'
#' @description
#' `slice()` lets you index rows by their (integer) locations. It allows you
#' to select, remove, and duplicate rows. It is accompanied by a number of
#' helpers for common use cases:
#'
#' * `slice_head()` and `slice_tail()` select the first or last rows.
#' * `slice_sample()` randomly selects rows.
#' * `slice_min()` and `slice_max()` select rows with highest or lowest values
#'   of a variable.
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
#' @inheritParams arrange
#' @inheritParams filter
#' @param ... For `slice()`: <[`data-masking`][dplyr_data_masking]> Integer row
#'   values.
#'
#'   Provide either positive values to keep, or negative values to drop.
#'   The values provided must be either all positive or all negative.
#'   Indices beyond the number of rows in the input are silently ignored.
#'
#'   For `slice_helpers()`, these arguments are passed on to methods.
#'
#' @param n,prop Provide either `n`, the number of rows, or `prop`, the
#'   proportion of rows to select. If neither are supplied, `n = 1` will be
#'   used.
#'
#'   If a negative value of `n` or `prop` is provided, the specified number or
#'   proportion of rows will be removed.
#'
#'   If `n` is greater than the number of rows in the group (or `prop > 1`),
#'   the result will be silently truncated to the group size. If the
#'   `prop`ortion of a group size does not yield an integer number of rows, the
#'   absolute value of `prop*nrow(.data)` is rounded down.
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
#' # in the presence of ties. Use with_ties = FALSE to suppress
#' mtcars %>% slice_min(cyl, n = 1)
#' mtcars %>% slice_min(cyl, n = 1, with_ties = FALSE)
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
slice <- function(.data, ..., .preserve = FALSE) {
  UseMethod("slice")
}

#' @export
slice.data.frame <- function(.data, ..., .preserve = FALSE) {
  loc <- slice_rows(.data, ..., caller_env = caller_env())
  dplyr_row_slice(.data, loc, preserve = .preserve)
}

#' @export
#' @rdname slice
slice_head <- function(.data, ..., n, prop) {
  UseMethod("slice_head")
}

#' @export
slice_head.data.frame <- function(.data, ..., n, prop) {
  ellipsis::check_dots_empty()
  size <- get_slice_size(n, prop, "slice_head")
  idx <- function(n) seq2(1, size(n))
  slice(.data, idx(dplyr::n()))
}

#' @export
#' @rdname slice
slice_tail <- function(.data, ..., n, prop) {
  UseMethod("slice_tail")
}

#' @export
slice_tail.data.frame <- function(.data, ..., n, prop) {
  ellipsis::check_dots_empty()
  size <- get_slice_size(n, prop, "slice_tail")
  idx <- function(n) seq2(n - size(n) + 1, n)
  slice(.data, idx(dplyr::n()))
}

check_slice_by <- function(idx, arg, x, n) {
  if (!is.null(x)) {
    vec_assert(x, size = n, arg = arg)
  }
  idx(x, n)
}

#' @export
#' @rdname slice
#' @param order_by Variable or function of variables to order by.
#' @param with_ties Should ties be kept together? The default, `TRUE`,
#'   may return more rows than you request. Use `FALSE` to ignore ties,
#'   and return the first `n` rows.
slice_min <- function(.data, order_by, ..., n, prop, with_ties = TRUE) {
  UseMethod("slice_min")
}

#' @export
slice_min.data.frame <- function(.data, order_by, ..., n, prop, with_ties = TRUE) {
  if (missing(order_by)) {
    abort("argument `order_by` is missing, with no default.")
  }

  ellipsis::check_dots_empty()
  size <- get_slice_size(n, prop, "slice_min")
  if (with_ties) {
    idx <- function(x, n) head(order(x), smaller_ranks(x, size(n)))
  } else {
    idx <- function(x, n) head(order(x), size(n))
  }
  slice(.data, check_slice_by(idx, "slice_min(order_by=)", {{ order_by }}, dplyr::n()))
}

#' @export
#' @rdname slice
slice_max <- function(.data, order_by, ..., n, prop, with_ties = TRUE) {
  UseMethod("slice_max")
}

#' @export
slice_max.data.frame <- function(.data, order_by, ..., n, prop, with_ties = TRUE) {
  if (missing(order_by)) {
    abort("argument `order_by` is missing, with no default.")
  }
  ellipsis::check_dots_empty()
  size <- get_slice_size(n, prop, "slice_max")
  if (with_ties) {
    idx <- function(x, n) head(
        order(x, decreasing = TRUE), smaller_ranks(desc(x), size(n))
    )
  } else {
    idx <- function(x, n) head(order(x, decreasing = TRUE), size(n))
  }

  slice(.data, check_slice_by(idx, "slice_max(order_by=)", {{ order_by }}, dplyr::n()))
}

#' @export
#' @rdname slice
#' @param replace Should sampling be performed with (`TRUE`) or without
#'   (`FALSE`, the default) replacement.
#' @param weight_by Sampling weights. This must evaluate to a vector of
#'   non-negative numbers the same length as the input. Weights are
#'   automatically standardised to sum to 1.
slice_sample <- function(.data, ..., n, prop, weight_by = NULL, replace = FALSE) {
  UseMethod("slice_sample")
}

#' @export
slice_sample.data.frame <- function(.data, ..., n, prop, weight_by = NULL, replace = FALSE) {
  ellipsis::check_dots_empty()

  size <- get_slice_size(n, prop, "slice_sample")
  idx <- function(x, n) sample_int(n, size(n), replace = replace, wt = x)
  slice(.data, idx({{ weight_by }}, dplyr::n()))
}

# helpers -----------------------------------------------------------------


slice_rows <- function(.data, ..., caller_env) {
  dots <- enquos(...)
  if (is_empty(dots)) {
    return(TRUE)
  }

  mask <- DataMask$new(.data, caller_env)
  on.exit(mask$forget("slice"), add = TRUE)

  rows <- mask$get_rows()

  slice_indices <- new_list(length(rows))

  quo <- quo(c(!!!dots))
  withCallingHandlers({
    chunks <- mask$eval_all(quo)

    for (group in seq_along(rows)) {
      current_rows <- rows[[group]]
      res <- chunks[[group]]

      if (is.logical(res) && all(is.na(res))) {
        res <- integer()
      } else if (is.numeric(res)) {
        res <- vec_cast(res, integer())
      } else if (!is.integer(res)) {
        mask$set_current_group(group)
        abort("`slice()` expressions should return indices (positive or negative integers).")
      }

      if (length(res) == 0L) {
        # nothing to do
      } else if (all(res >= 0, na.rm = TRUE)) {
        res <- res[!is.na(res) & res <= length(current_rows) & res > 0]
      } else if (all(res <= 0, na.rm = TRUE)) {
        res <- setdiff(seq_along(current_rows), -res)
      } else {
        mask$set_current_group(group)
        abort("`slice()` expressions should return either all positive or all negative.")
      }

      slice_indices[[group]] <- current_rows[res]
    }

  }, error = function(e) {
    abort(c(
      conditionMessage(e),
      i = cnd_bullet_cur_group_label()
    ), class = "dplyr_error")
  })

  vec_c(!!!slice_indices, .ptype = integer())
}

check_constant <- function(x, name, fn) {
  withCallingHandlers(force(x), error = function(e) {
    abort(c(
      glue("`{name}` must be a constant in `{fn}()`."),
      x = conditionMessage(e)
    ), parent = e)
  })
}

check_slice_size <- function(n, prop, .slice_fn = "check_slice_size") {
  if (missing(n) && missing(prop)) {
    list(type = "n", n = 1L)
  } else if (!missing(n) && missing(prop)) {
    n <- check_constant(n, "n", .slice_fn)
    if (!is.numeric(n) || length(n) != 1 || is.na(n)) {
      abort("`n` must be a single number.")
    }
    list(type = "n", n = n)
  } else if (!missing(prop) && missing(n)) {
    prop <- check_constant(prop, "prop", .slice_fn)
    if (!is.numeric(prop) || length(prop) != 1 || is.na(prop)) {
      abort("`prop` must be a single number.")
    }
    list(type = "prop", prop = prop)
  } else {
    abort("Must supply exactly one of `n` and `prop` arguments.")
  }
}

get_slice_size <- function(n, prop, .slice_fn = "get_slice_size") {
  slice_input <- check_slice_size(n, prop, .slice_fn)

  if (slice_input$type == "n") {
    if (slice_input$n < 0) {
      function(n) max(ceiling(n + slice_input$n), 0)
    } else {
      function(n) min(floor(slice_input$n), n)
    }
  } else if (slice_input$type == "prop") {
    if (slice_input$prop < 0) {
      function(n) max(ceiling(n + slice_input$prop * n), 0)
    } else {
      function(n) min(floor(slice_input$prop * n), n)
    }
  }
}

sample_int <- function(n, size, replace = FALSE, wt = NULL) {
  if (!replace) {
    size <- min(size, n)
  }
  if (size == 0L) {
    integer(0)
  } else {
    sample.int(n, size, prob = wt, replace = replace)
  }
}

smaller_ranks <- function(x, y) {
  sum(min_rank(x) <= y, na.rm = TRUE)
}

