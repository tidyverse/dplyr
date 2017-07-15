#include "pch.h"
#include <Rcpp.h>
using namespace Rcpp;

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
//' x <- rnorm(1e2)
//' x[between(x, -1, 1)]
// [[Rcpp::export]]
LogicalVector between(NumericVector x, double left, double right) {
  int n = x.size();
  LogicalVector out = no_init(n);

  // Assume users know what they're doing with date/times. In the future
  // should ensure that left and right are the correct class too.
  if (x.attr("class") != R_NilValue && !Rf_inherits(x, "Date") && !Rf_inherits(x, "POSIXct")) {
    warningcall(R_NilValue, "between() called on numeric vector with S3 class");
  }

  if (NumericVector::is_na(left) || NumericVector::is_na(right)) {
    for (int i = 0; i < n; ++i)
      out[i] = NA_LOGICAL;
    return out;
  }

  for (int i = 0; i < n; ++i) {
    if (NumericVector::is_na(x[i])) {
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
