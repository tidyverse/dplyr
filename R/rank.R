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
#'   `n` buckets.
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
#' ntile(runif(100), 10)
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
# http://blogs.msdn.com/b/craigfr/archive/2008/03/31/ranking-functions-rank-dense-rank-and-ntile.aspx
#' @param n number of groups to split up into.
#' @export
#' @rdname ranking
ntile <- function(x = row_number(), n) {
  len <- sum(!is.na(x))

  if (len == 0L) {
    rep(NA_integer_, length(x))
  } else {
    as.integer(floor(n * (row_number(x) - 1) / len + 1))
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
