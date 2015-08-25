#' Windowed rank functions.
#'
#' Six variations on ranking functions, mimicing the ranking functions
#' described in SQL2003. They are currently implemented using the built in
#' \code{rank} function, and are provided mainly as a convenience when
#' converting between R and SQL. All ranking functions map smallest inputs
#' to smallest outputs. Use \code{\link{desc}} to reverse the direction..
#'
#' \itemize{
#' \item \code{row_number}: equivalent to \code{rank(ties.method = "first")}
#'
#' \item \code{min_rank}: equivalent to \code{rank(ties.method = "min")}
#'
#' \item \code{dense_rank}: like \code{min_rank}, but with no gaps between
#'   ranks
#'
#' \item \code{percent_rank}: a number between 0 and 1 computed by
#'   rescaling \code{min_rank} to [0, 1]
#'
#' \item \code{cume_dist}: a cumulative distribution function. Proportion
#'   of all values less than or equal to the current rank.
#'
#' \item \code{ntile}: a rough rank, which breaks the input vector into
#'   \code{n} buckets.
#'
#' }
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
NULL

#' @export
#' @rdname ranking
row_number <- function(x) rank(x, ties.method = "first", na.last = "keep")

# Definition from
# http://blogs.msdn.com/b/craigfr/archive/2008/03/31/ranking-functions-rank-dense-rank-and-ntile.aspx
#' @param n number of groups to split up into.
#' @export
#' @rdname ranking
ntile <- function(x, n) {
  floor((n * (row_number(x) - 1) / length(x)) + 1)
}

#' @export
#' @rdname ranking
min_rank <- function(x) rank(x, ties.method = "min", na.last = "keep")

#' @export
#' @rdname ranking
dense_rank <- function(x) {
  r <- rank(x, na.last = "keep")
  match(r, sort(unique(r)))
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

