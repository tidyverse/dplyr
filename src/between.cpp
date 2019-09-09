#include <Rcpp.h>

//' Do values in a numeric vector fall in specified range?
//'
//' This is a shortcut for `x >= left & x <= right`, implemented
//' efficiently in C++ for local values, and translated to the
//' appropriate SQL for remote tables.
//'
//' @param x A numeric vector of values
//' @param left,right Boundary values
//' @export
//' @examples
//' between(1:12, 7, 9)
//'
//' x <- rnorm(1e2)
//' x[between(x, -1, 1)]
// [[Rcpp::export(rng = false)]]
Rcpp::LogicalVector between(Rcpp::NumericVector x, double left, double right) {
  int n = x.size();
  Rcpp::LogicalVector out(Rcpp::no_init(n));

  // Assume users know what they're doing with date/times. In the future
  // should ensure that left and right are the correct class too.
  if (!Rf_isNull(Rf_getAttrib(x, R_ClassSymbol)) && !Rf_inherits(x, "Date") && !Rf_inherits(x, "POSIXct")) {
    Rcpp::warningcall(R_NilValue, "between() called on numeric vector with S3 class");
  }

  if (Rcpp::NumericVector::is_na(left) || Rcpp::NumericVector::is_na(right)) {
    for (int i = 0; i < n; ++i)
      out[i] = NA_LOGICAL;
    return out;
  }

  for (int i = 0; i < n; ++i) {
    if (Rcpp::NumericVector::is_na(x[i])) {
      out[i] = NA_LOGICAL;
    } else if ((x[i] >= left) && (x[i] <= right)) {
      out[i] = true;
    } else {
      out[i] = false;
    }
  }

  return out;
}

/*** R

library(microbenchmark)

betweenr <- function(x, left, right){
 x >= left & x <= right
}

x <- c(NA, runif(1e4), NA)
stopifnot(all.equal(between(x, 0.1, 0.9), betweenr(x, 0.1, 0.9)))

microbenchmark(
  between(x, 0.1, 0.9),
  betweenr(x, 0.1, 0.9)
)

*/
