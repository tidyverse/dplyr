#include "pch.h"
#include <dplyr/main.h>

//' Cumulativate versions of any, all, and mean
//'
//' dplyr adds `cumall()`, `cumany()`, and `cummean()` to complete
//' R's set of cumulate functions to match the aggregation functions available
//' in most databases
//'
//' @param x For `cumall()` and `cumany()`, a logical vector; for
//'   `cummean()` an integer or numeric vector
//' @export
//' @examples
//' x <- c(1, 3, 5, 2, 2)
//'
//' # `cummean()` returns a numeric/integer vector of the same length
//' # as the input vector.
//' cummean(x)
//'
//' # `cumall()` and `cumany()` return logicals, not numerics/integers.
//' cumall(x)
//' cumany(x)
//'
//'
//' # An NA only affects all elements after it for `cumall()` and `cummean()`,
//' # but NOT for `cumany()`
//' x <- c(1, 3, 5, NA, 2, 2)
//'
//' # NAs in every position after the corresponding NA position in the vector.
//' cumall(x)
//' cummean(x)
//'
//' # No NAs at all!
//' cumany(x)
//'
//' # An NA at the beginning of the vector causes all elements after to be NA.
//' x <- c(NA, 1, 3, 5, 2, 2)
//'
//' cummean(x)
//' cumall(x)
//' cumany(x)
//'
//' # NULLs are thrown out.
//' x <- c(1, 3, 5, NULL, 2, 2)
//'
//' cummean(x)
//' cumall(x)
//' cumany(x)
// [[Rcpp::export]]
LogicalVector cumall(LogicalVector x) {
  int n = x.length();
  LogicalVector out(n, FALSE);
  int current  = TRUE;
  int previous = TRUE;

  for (int i = 0; i < n; i++) {
    current = x[i];
    if (current == FALSE) {
      break;
    } else if (current == NA_LOGICAL) {
      out[i] = NA_LOGICAL;
      previous = NA_LOGICAL;
    } else {
      out[i] = previous;
    }
  }

  return out;
}

//' @export
//' @rdname cumall
// [[Rcpp::export]]
LogicalVector cumany(LogicalVector x) {
  int n = x.length();
  LogicalVector out(n, TRUE);
  int current  = FALSE;
  int previous = FALSE;

  for (int i = 0; i < n; i++) {
    current = x[i];
    if (current == FALSE) {
      out[i] = previous;
    } else if (current == NA_LOGICAL) {
      out[i]   = NA_LOGICAL;
      previous = NA_LOGICAL;
    } else {
      break;
    }
  }

  return out;
}

//' @export
//' @rdname cumall
// [[Rcpp::export]]
NumericVector cummean(NumericVector x) {
  int n = x.length();
  NumericVector out(no_init(n));

  double sum = out[0] = x[0];
  for (int i = 1; i < n; i++) {
    sum += x[i];
    out[i] = sum / (i + 1.0);
  }

  return out;
}
