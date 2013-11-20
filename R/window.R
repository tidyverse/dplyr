#' Lead and lag.
#' 
#' Lead and lag are useful for comparing values offset by a constant (e.g. the
#' previous or next value)
#' 
#' @param x a vector of values
#' @param n a postive integer of length 1, giving the number of positions to
#'   lead or lag by
#' @param default value used for non-existant rows. Defaults to \code{NA}.
#' @examples
#' lead(1:10, 1)
#' lead(1:10, 2)
#' 
#' lag(1:10, 1)
#' lead(1:10, 1)
#' 
#' x <- runif(5)
#' cbind(ahead = lead(x), x, behind = lag(x))
#' @name lead-lag
NULL

#' @export
#' @rdname lead-lag
lead <- function(x, n = 1L, default = NA) {
  if (n == 0) return(x)
  if (n < 0 || length(n) > 1) stop("n must be a single positive integer")
  
  xlen <- length(x)
  n <- pmin(n, xlen)
  
  c(x[-seq_len(n)], rep(default, n)) 
}

#' @export
#' @rdname lead-lag
lag <- function(x, n = 1L, default = NA) {
  if (n == 0) return(x)
  if (n < 0 || length(n) > 1) stop("n must be a single positive integer")
  
  xlen <- length(x)
  n <- pmin(n, xlen)
  
  c(rep(default, n), x[seq_len(xlen - n)])
}
