#' Choose rows by position
#'
#' Choose rows by their ordinal position in the tbl.  Grouped tbls use
#' the ordinal position within the group.
#'
#' Slice does not work with relational databases because they have no
#' intrinsic notion of row order. If you want to perform the equivalent
#' operation, use [filter()] and [row_number()].
#'
#' @family single table verbs
#' @param .data A tbl.
#' @param ... <[`tidy-eval`][dplyr_tidy_eval]> Integer row values.
#'   Provide either positive values to keep, or negative values to drop.
#'   The values provided must be either all positive or all negative.
#'   Indices beyond the number of rows in the input are silently ignored.
#' @inheritParams filter
#' @inheritSection filter Tidy data
#' @export
#' @examples
#' slice(mtcars, 1L)
#' # Similar to tail(mtcars, 1):
#' slice(mtcars, n())
#' slice(mtcars, 5:n())
#' # Rows can be dropped with negative indices:
#' slice(mtcars, -5:-n())
#' # In this case, the result will be equivalent to:
#' slice(mtcars, 1:4)
#'
#' by_cyl <- group_by(mtcars, cyl)
#' slice(by_cyl, 1:2)
#'
#' # Equivalent code using filter that will also work with databases,
#' # but won't be as fast for in-memory data. For many databases, you'll
#' # need to supply an explicit variable to use to compute the row number.
#' filter(mtcars, row_number() == 1L)
#' filter(mtcars, row_number() == n())
#' filter(mtcars, between(row_number(), 5, n()))
slice <- function(.data, ..., .preserve = FALSE) {
  UseMethod("slice")
}

#' @export
slice.data.frame <- function(.data, ..., .preserve = FALSE) {
  idx <- slice_rows(.data, ...)
  .data[idx$data, , drop = FALSE]
}

#' @export
slice.grouped_df <- function(.data, ..., .preserve = !group_by_drop_default(.data)) {
  idx <- slice_rows(.data, ...)
  data <- as.data.frame(.data)[idx$data, , drop = FALSE]

  groups <- group_data(.data)
  groups$.rows <- new_list_of(idx$groups, ptype = integer())
  groups <- group_data_trim(groups, .preserve)

  new_grouped_df(data, groups)
}

slice_rows <- function(.data, ...) {
  dots <- enquos(...)
  if (is_empty(dots)) {
    return(.data)
  }

  rows <- group_rows(.data)
  mask <- DataMask$new(.data, caller_env(), rows)

  quo <- quo(c(!!!dots))

  chunks <- mask$eval_all(quo)

  slice_indices <- new_list(length(rows))
  new_rows <- new_list(length(rows))
  k <- 1L

  for (group in seq_along(rows)) {
    current_rows <- rows[[group]]
    res <- chunks[[group]]

    if (is.logical(res) && all(is.na(res))) {
      res <- integer()
    } else if (is.numeric(res)) {
      res <- vec_cast(res, integer())
    } else if (!is.integer(res)) {
      abort(
        "slice() expressions should return indices (positive or negative integers)",
        "dplyr_slice_incompatible"
      )
    }

    if (length(res) == 0L) {
      # nothing to do
    } else if(all(res >= 0, na.rm = TRUE)) {
      res <- res[!is.na(res) & res <= length(current_rows) & res > 0]
    } else if (all(res <= 0, na.rm = TRUE)) {
      res <- setdiff(seq_along(current_rows), -res)
    } else {
      abort(
        "slice() expressions should return either all positive or all negative",
        "dplyr_slice_ambiguous"
      )
    }

    slice_indices[[group]] <- current_rows[res]
    new_k <- k + length(res)
    new_rows[[group]] <- seq2(k, new_k - 1L)
    k <- new_k
  }

  list(
    data = vec_c(!!!slice_indices, .ptype = integer()),
    groups = new_rows
  )
}

# Slice helpers -----------------------------------------------------------

#' Slice helpers
#'
#' @description
#' These functions provide useful tools for selecting rows, using [slice()]:
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
#' @param .data A data frame or extension.
#' @param ... Additional arguments passed on to methods.
#' @param n,prop Provide either `n`, the number of rows, or `prop`, the
#'   proportion of rows to select. If `n` is greater than the number of
#'   rows in the group (or `prop > 1`), it will be silently truncated to the
#'   group size. If the `prop`ortion of a group size is not an integer, it will
#'   be rounded down.
#' @export
#' @examples
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
slice_head <- function(.data, ..., n, prop) {
  UseMethod("slice_head")
}

#' @export
slice_head.data.frame <- function(.data, ..., n, prop) {
  size <- check_slice_size(n, prop)
  idx <- switch(size$type,
    n =    function(n) seq2(1, min(size$n, n)),
    prop = function(n) seq2(1, min(size$prop * n, n))
  )

  slice(.data, idx(dplyr::n()))
}

#' @export
#' @rdname slice_head
slice_tail <- function(.data, ..., n, prop) {
  UseMethod("slice_tail")
}

#' @export
slice_tail.data.frame <- function(.data, ..., n, prop) {
  size <- check_slice_size(n, prop)
  idx <- switch(size$type,
    n =    function(n) seq2(max(n - size$n + 1, 1), n),
    prop = function(n) seq2(max(n - size$prop * n + 1, 1), n)
  )
  slice(.data, idx(dplyr::n()))
}

#' @export
#' @rdname slice_head
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
    abort("argument `order_by` is missing, with no default")
  }

  size <- check_slice_size(n, prop)
  if (with_ties) {
    idx <- switch(size$type,
      n =    function(x, n) head(order(x), sum(min_rank(x) <= size$n)),
      prop = function(x, n) head(order(x), sum(min_rank(x) <= size$prop * n)),
    )
  } else {
    idx <- switch(size$type,
      n =    function(x, n) head(order(x), size$n),
      prop = function(x, n) head(order(x), size$prop * n)
    )
  }
  slice(.data, idx({{ order_by }}, dplyr::n()))
}

#' @export
#' @rdname slice_head
slice_max <- function(.data, order_by, ..., n, prop, with_ties = TRUE) {
  UseMethod("slice_max")
}

#' @export
slice_max.data.frame <- function(.data, order_by, ..., n, prop, with_ties = TRUE) {
  if (missing(order_by)) {
    abort("argument `order_by` is missing, with no default")
  }

  size <- check_slice_size(n, prop)
  if (with_ties) {
    idx <- switch(size$type,
      n =    function(x, n) head(order(x, decreasing = TRUE), sum(min_rank(desc(x)) <= size$n)),
      prop = function(x, n) head(order(x, decreasing = TRUE), sum(min_rank(desc(x)) <= size$prop * n))
    )
  } else {
    idx <- switch(size$type,
      n =    function(x, n) head(order(x, decreasing = TRUE), size$n),
      prop = function(x, n) head(order(x, decreasing = TRUE), size$prop * n)
    )
  }

  slice(.data, idx({{ order_by }}, dplyr::n()))
}

#' @export
#' @rdname slice_head
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
  size <- check_slice_size(n, prop)
  idx <- switch(size$type,
    n =    function(x, n) sample_int(n, size$n, replace = replace, wt = x),
    prop = function(x, n) sample_int(n, size$prop * n, replace = replace, wt = x),
  )

  slice(.data, idx({{ weight_by }}, dplyr::n()))
}

# helpers -----------------------------------------------------------------

check_slice_size <- function(n, prop) {
  if (!missing(n) && missing(prop)) {
    if (!is.numeric(n) || length(n) != 1) {
      abort("`n` must be a single number")
    }
    if (is.na(n) || n < 0) {
      abort("`n` must be a non-missing positive number")
    }

    list(type = "n", n = n)
  } else if (!missing(prop) && missing(n)) {
    if (!is.numeric(prop) || length(prop) != 1) {
      abort("`prop` must be a single number")
    }
    if (is.na(prop) || prop < 0) {
      abort("`prop` must be a non-missing positive number")
    }
    list(type = "prop", prop = prop)
  } else {
    abort("Must supply exactly one of `n` and `prop` arguments")
  }
}

sample_int <- function(n, size, replace = FALSE, wt = NULL) {
  if (replace) {
    sample.int(n, size, prob = wt, replace = TRUE)
  } else {
    sample.int(n, min(size, n), prob = wt)
  }
}
