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
