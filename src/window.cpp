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
  LogicalVector out(n, NA_LOGICAL);

  int current = out[0] = x[0];
  if (current == NA_LOGICAL) return out;
  if (current == FALSE) {
    std::fill(out.begin(), out.end(), FALSE);
    return out;
  }
  for (int i = 1; i < n; i++) {
    current = x[i];
    if (current == NA_LOGICAL) break;
    if (current == FALSE) {
      std::fill(out.begin() + i, out.end(), FALSE);
      break;
    }
    out[i] = current && out[i - 1];
  }
  return out;
}

//' @export
//' @rdname cumall
// [[Rcpp::export]]
LogicalVector cumany(LogicalVector x) {
  int n = x.length();
  LogicalVector out(n, NA_LOGICAL);

  int current = out[0] = x[0];
  if (current == NA_LOGICAL) return out;
  if (current == TRUE) {
    std::fill(out.begin(), out.end(), TRUE);
    return out;
  }
  for (int i = 1; i < n; i++) {
    current = x[i];
    if (current == NA_LOGICAL) break;
    if (current == TRUE) {
      std::fill(out.begin() + i, out.end(), TRUE);
      break;
    }
    out[i] = current || out[i - 1];
  }

  return out;
}

//' @export
//' @rdname cumall
// [[Rcpp::export]]
NumericVector cummean(NumericVector x) {
  int n = x.length();
  NumericVector out = no_init(n);

  double sum = out[0] = x[0];
  for (int i = 1; i < n; i++) {
    sum += x[i];
    out[i] = sum / (i + 1.0);
  }

  return out;
}
