#include "pch.h"
#include <dplyr/main.h>

//' Cumulativate versions of any, all, and mean
//'
//' dplyr provides `cumall()`, `cumany()`, and `cummean()` to complete R's set
//' of cumulativee functions.
//'
//' @section Cumulative logical functions:
//'
//' These are particularly useful in conjunction with `filter()`:
//'
//' * `cumall(x)`: all cases until the first `FALSE`.
//' * `cumall(!x)`: all cases until the first `TRUE`.
//' * `cumany(x)`: all cases after the first `TRUE`.
//' * `cumany(!x)`: all cases after the first `FALSE`.
//'
//' @param x For `cumall()` and `cumany()`, a logical vector; for
//'   `cummean()` an integer or numeric vector.
//' @return A vector the same length as `x`.
//' @export
//' @examples
//' # `cummean()` returns a numeric/integer vector of the same length
//' # as the input vector.
//' x <- c(1, 3, 5, 2, 2)
//' cummean(x)
//' cumsum(x) / seq_along(x)
//'
//' # `cumall()` and `cumany()` return logicals
//' cumall(x < 5)
//' cumany(x == 3)
//'
//' # `cumall()` vs. `cumany()`
//' df <- data.frame(
//'   date = as.Date("2020-01-01") + 0:6,
//'   balance = c(100, 50, 25, -25, -50, 30, 120)
//' )
//' # all rows after first overdraft
//' df %>% filter(cumany(balance < 0))
//' # all rows until first overdraft
//' df %>% filter(cumall(!(balance < 0)))
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
