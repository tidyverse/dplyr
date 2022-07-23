#' Windowed rank functions.
#'
#' Six variations on ranking functions, mimicking the ranking functions
#' described in SQL2003. They are currently implemented using the built in
#' `rank` function, and are provided mainly as a convenience when
#' converting between R and SQL. All ranking functions map smallest inputs
#' to smallest outputs. Use [desc()] to reverse the direction.
#'
#' * `row_number()`: equivalent to `rank(ties.method = "first")`
#'
#' * `min_rank()`: equivalent to `rank(ties.method = "min")`
#'
#' * `dense_rank()`: like `min_rank()`, but with no gaps between
#'   ranks
#'
#' * `percent_rank()`: a number between 0 and 1 computed by
#'   rescaling `min_rank` to `[0, 1]`
#'
#' * `cume_dist()`: a cumulative distribution function. Proportion
#'   of all values less than or equal to the current rank.
#'
#' * `ntile()`: a rough rank, which breaks the input vector into
#'   `n` buckets. The size of the buckets may differ by up to one,
#'   larger buckets have lower rank.
#'
#' @name ranking
#' @param x a vector of values to rank. Missing values are left as is.
#'   If you want to treat them as the smallest or largest values, replace
#'   with Inf or -Inf before ranking.
#' @examples
#' x <- c(5, 1, 3, 2, 2, NA)
#' row_number(x)
#' min_rank(x)
#' dense_rank(x)
#' percent_rank(x)
#' cume_dist(x)
#'
#' ntile(x, 2)
#' ntile(1:8, 3)
#'
#' # row_number can be used with single table verbs without specifying x
#' # (for data frames and databases that support windowing)
#' mutate(mtcars, row_number() == 1L)
#' mtcars %>% filter(between(row_number(), 1, 10))
NULL

#' @export
#' @rdname ranking
row_number <- function(x) {
  if (missing(x)){
    seq_len(n())
  } else {
    rank(x, ties.method = "first", na.last = "keep")
  }
}

# Definition from
# https://techcommunity.microsoft.com/t5/sql-server/ranking-functions-rank-dense-rank-and-ntile/ba-p/383384
#' @param n number of groups to split up into.
#' @export
#' @rdname ranking
ntile <- function(x = row_number(), n) {
  # Avoid recomputation in default case:
  # row_number(row_number(x)) == row_number(x)
  if (!missing(x)) {
    x <- row_number(x)
  }
  len <- length(x) - sum(is.na(x))

  n <- as.integer(floor(n))

  if (len == 0L) {
    rep(NA_integer_, length(x))
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
      (x + (- larger_threshold + smaller_size - 1L)) / smaller_size + n_larger
    )

    as.integer(floor(bins))
  }
}

#' @export
#' @rdname ranking
min_rank <- function(x) rank(x, ties.method = "min", na.last = "keep")

#' @export
#' @rdname ranking
dense_rank <- function(x) {
  match(x, sort(unique(x)))
}

#' @export
#' @rdname ranking
percent_rank <- function(x) {
  (min_rank(x) - 1) / (sum(!is.na(x)) - 1)
}

#' @export
#' @rdname ranking
cume_dist <- function(x) {
  rank(x, ties.method = "max", na.last = "keep") / sum(!is.na(x))
}

#' @export
#' @rdname ranking
#' @examples
#' percentile_rank(0:9)
#' x <- c(1L, 2L, 1L, 7L, 5L, NA_integer_, 7L, 10L)
#' percentile_rank(x)
#'
#' # with weights (example from Wikipedia)
#' percentile_rank(7:1, c(1L, 0L, 2L, 2L, 3L, 1L, 1L))
#' @param weights Optional weights for each value.  When used, can guarantee
#'   that the full set of `x` is included, e.g., a percentile rank will be
#'   derived for instances in which a value if `x` is not present.
percentile_rank <- function(x, weights = NULL) {
  if (!rlang::is_null(weights)) {
    return(do_percentile_rank(x, weights))
  }

  tab <- vctrs::vec_count(x, "location")
  res <- with(tab, do_percentile_rank(key, count))
  res[vctrs::vec_match(x, tab$key)]
}

do_percentile_rank <- function(u, w) {
  if (vctrs::vec_duplicate_any(u)) {
    # TODO improve error message
    rlang::abort("Duplicates found in x")
  }

  if (!rlang::is_integer(w)) {
    # TODO improve error message
    rlang::abort("Weights must be interger-like")
  }

  # vec_recycle_common() not needed because length(w) <= length(u), never
  # length(w) > length(u)
  if (length(w) == 1L) {
    if (is.na(w)) {
      # If weight is NA return NA?  Maybe through an warning?
      return(rep.int(NA_real_, length(n)))
    }

    # no ordering necessary
    ok <- !is.na(u)
    n <- sum(ok)
    p <- vctrs::vec_repeat(1L, n)
    res <- (cumsum(p) - 0.5) / n
  } else {
    if (length(w) != length(u)) {
      # TODO improve error message
      abort("length(w) must be 1L or equal to length(x)")
    }

    ok <- is.na(u) & !is.na
    o <- vctrs::vec_order(u[ok])
    p <- w[ok][o]
    res <- (cumsum(p) - p * 0.5)[vctrs::vec_match(u[ok][o], u[ok])] / sum(w[ok])
  }

  out <- vctrs::vec_init(NA_real_, length(ok))
  out[ok] <- res
  out
}
