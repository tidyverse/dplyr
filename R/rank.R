#' Windowed rank functions
#' 
#' @name ranking
#' @param x a vector of values to rank
#' @examples
#' x <- c(5, 1, 3, 2, 2)
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
row_number <- function(x) order(x)

# Definition from 
# http://blogs.msdn.com/b/craigfr/archive/2008/03/31/ranking-functions-rank-dense-rank-and-ntile.aspx
#' @param n number of groups to split up into.
#' @export
#' @rdname ranking
ntile <- function(x, n) {
  floor((n * (rank(x, ties.method = "first") - 1) / length(x)) + 1)
}

#' @export
#' @rdname ranking
min_rank <- function(x) base::rank(x, ties.method = "min")

#' @export
#' @rdname ranking
dense_rank <- function(x) {
  r <- rank(x)
  match(r, sort(unique(r)))
}

#' @export
#' @rdname ranking
percent_rank <- function(x) {
  (min_rank(x) - 1) / (length(x) - 1)
}

#' @export
#' @rdname ranking
cume_dist <- function(x) {
  rank(x, ties.method = "max") / length(x)
}

