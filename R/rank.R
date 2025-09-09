#' Integer ranking functions
#'
#' @description
#' Three ranking functions inspired by SQL2003. They differ primarily in how
#' they handle ties:
#'
#' * `row_number()` gives every input a unique rank, so that `c(10, 20, 20, 30)`
#'   would get ranks `c(1, 2, 3, 4)`. It's equivalent to
#'   `rank(ties.method = "first")`.
#'
#' * `min_rank()` gives every tie the same (smallest) value so that
#'   `c(10, 20, 20, 30)` gets ranks `c(1, 2, 2, 4)`. It's the way that ranks
#'   are usually computed in sports and is equivalent to
#'   `rank(ties.method = "min")`.
#'
#' * `dense_rank()` works like `min_rank()`, but doesn't leave any gaps,
#'   so that `c(10, 20, 20, 30)` gets ranks `c(1, 2, 2, 3)`.
#'
#' @param x A vector to rank
#'
#'   By default, the smallest values will get the smallest ranks. Use [desc()]
#'   to reverse the direction so the largest values get the smallest ranks.
#'
#'   Missing values will be given rank `NA`. Use `coalesce(x, Inf)` or
#'   `coalesce(x, -Inf)` if you want to treat them as the largest or smallest
#'   values respectively.
#'
#'   To rank by multiple columns at once, supply a data frame.
#' @return An integer vector.
#' @family ranking functions
#' @examples
#' x <- c(5, 1, 3, 2, 2, NA)
#' row_number(x)
#' min_rank(x)
#' dense_rank(x)
#'
#' # Ranking functions can be used in `filter()` to select top/bottom rows
#' df <- data.frame(
#'   grp = c(1, 1, 1, 2, 2, 2, 3, 3, 3),
#'   x = c(3, 2, 1, 1, 2, 2, 1, 1, 1),
#'   y = c(1, 3, 2, 3, 2, 2, 4, 1, 2),
#'   id = 1:9
#' )
#' # Always gives exactly 1 row per group
#' df |> group_by(grp) |> filter(row_number(x) == 1)
#' # May give more than 1 row if ties
#' df |> group_by(grp) |> filter(min_rank(x) == 1)
#' # Rank by multiple columns (to break ties) by selecting them with `pick()`
#' df |> group_by(grp) |> filter(min_rank(pick(x, y)) == 1)
#' # See slice_min() and slice_max() for another way to tackle the same problem
#'
#' # You can use row_number() without an argument to refer to the "current"
#' # row number.
#' df |> group_by(grp) |> filter(row_number() == 1)
#'
#' # It's easiest to see what this does with mutate():
#' df |> group_by(grp) |> mutate(grp_id = row_number())
#' @export
row_number <- function(x) {
  if (missing(x)) {
    seq_len(n())
  } else {
    vec_rank(x, ties = "sequential", incomplete = "na")
  }
}

#' @export
#' @rdname row_number
min_rank <- function(x) {
  vec_rank(x, ties = "min", incomplete = "na")
}

#' @export
#' @rdname row_number
dense_rank <- function(x) {
  vec_rank(x, ties = "dense", incomplete = "na")
}

#' Bucket a numeric vector into `n` groups
#'
#' @description
#' `ntile()` is a sort of very rough rank, which breaks the input vector into
#' `n` buckets. If `length(x)` is not an integer multiple of `n`, the size of
#' the buckets will differ by up to one, with larger buckets coming first.
#'
#' Unlike other ranking functions, `ntile()` ignores ties: it will create
#' evenly sized buckets even if the same value of `x` ends up in different
#' buckets.
#'
#' @inheritParams row_number
#' @param n Number of groups to bucket into
#' @export
#' @family ranking functions
#' @examples
#' x <- c(5, 1, 3, 2, 2, NA)
#' ntile(x, 2)
#' ntile(x, 4)
#'
#' # If the bucket sizes are uneven, the larger buckets come first
#' ntile(1:8, 3)
#'
#' # Ties are ignored
#' ntile(rep(1, 8), 3)
ntile <- function(x = row_number(), n) {
  # Avoid recomputation in default case:
  # row_number(row_number(x)) == row_number(x)
  if (!missing(x)) {
    x <- row_number(x)
  }
  len <- vec_size(x) - sum(vec_detect_missing(x))

  check_number_whole(n)
  n <- vec_cast(n, integer())
  if (n <= 0L) {
    abort("`n` must be positive.")
  }

  # Definition from
  # https://techcommunity.microsoft.com/t5/sql-server/ranking-functions-rank-dense-rank-and-ntile/ba-p/383384
  if (len == 0L) {
    rep(NA_integer_, vec_size(x))
  } else {
    n_larger <- as.integer(len %% n)
    n_smaller <- as.integer(n - n_larger)
    size <- len / n
    larger_size <- as.integer(ceiling(size))
    smaller_size <- as.integer(floor(size))

    larger_threshold <- larger_size * n_larger
    bins <- if_else(
      x <= larger_threshold,
      (x + (larger_size - 1L)) / larger_size,
      (x + (-larger_threshold + smaller_size - 1L)) / smaller_size + n_larger
    )

    as.integer(floor(bins))
  }
}

#' Proportional ranking functions
#'
#' @description
#' These two ranking functions implement two slightly different ways to
#' compute a percentile. For each `x_i` in `x`:
#'
#' * `cume_dist(x)` counts the total number of values less than
#'   or equal to `x_i`, and divides it by the number of observations.
#'
#' * `percent_rank(x)` counts the total number of values less than
#'   `x_i`, and divides it by the number of observations minus 1.
#'
#' In both cases, missing values are ignored when counting the number
#' of observations.
#'
#' @inheritParams row_number
#' @returns A numeric vector containing a proportion.
#' @family ranking functions
#' @export
#' @examples
#' x <- c(5, 1, 3, 2, 2)
#'
#' cume_dist(x)
#' percent_rank(x)
#'
#' # You can understand what's going on by computing it by hand
#' sapply(x, function(xi) sum(x <= xi) / length(x))
#' sapply(x, function(xi) sum(x < xi)  / (length(x) - 1))
#' # The real computations are a little more complex in order to
#' # correctly deal with missing values
percent_rank <- function(x) {
  (min_rank(x) - 1) / (sum(vec_detect_complete(x)) - 1)
}

#' @export
#' @rdname percent_rank
cume_dist <- function(x) {
  vec_rank(x, ties = "max", incomplete = "na") / sum(vec_detect_complete(x))
}
