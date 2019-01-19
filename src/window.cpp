#include "pch.h"
#include <dplyr/main.h>

//' Cumulativate versions of any, all, and mean
//'
//' dplyr adds `cumall()`, `cumany()`, and `cummean()` to complete
//' R's set of cumulate functions to match the aggregation functions available
//' in most databases
//'
//' * `cumean()`: cumulative mean
//'
//' * `cumall()`: useful for finding all records after where a condition
//' is `FALSE` for the first time
//'
//' * `cumany()`: useful for finding all records up to where a condition
//' is `TRUE` for the first time
//'
//' @param x For `cumall()` and `cumany()`, a logical vector; for
//'   `cummean()` an integer or numeric vector
//' @export
//' @examples
//'
//' # `cummean()` returns a numeric/integer vector of the same length
//' # as the input vector.
//' # `cumall()` and `cumany()` return logicals, not numerics/integers.
//' x <- c(1, 3, 5, 2, 2)
//' cummean(x)
//' cumall(x)
//'
//' # An `NA only affects all elements after it for `cumall()` and `cummean()`,
//' # but NOT for `cumany()`. This behavior is consistent with that of
//' # `NA || TRUE` compared to `NA && TRUE`.
//' x <- c(1, 3, 5, NA, 2, 2)
//' cummean(x)
//' cumall(x)
//'
//' # An `NA` at the beginning of the vector causes all elements after to be `NA`.
//' x <- c(NA, 1, 3, 5, 2, 2)
//' cummean(x)
//' cumall(x)
//'
//' # `cumall()` vs. `cumany()`
//' # `cumall()` will return all rows until the first `FALSE`.
//' # `cumany()` will return all rows after the first `TRUE`.
//' dplyr::filter(storms, cumall(wind > 26))
//' dplyr::filter(storms, cumany(wind > 26))
//'
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
