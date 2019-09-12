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

//' Cumulativate versions of any, all, and mean
//'
//' dplyr provides `cumall()`, `cumany()`, and `cummean()` to complete R's set
//' of cumulative functions.
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
// [[Rcpp::export(rng = false)]]
Rcpp::LogicalVector cumall(Rcpp::LogicalVector x) {
  int n = x.length();
  Rcpp::LogicalVector out(n, TRUE);
  int* p_x = x.begin();
  int* p_out = out.begin();

  // nothing to do as long as x[i] is TRUE
  int i = 0 ;
  for (; i < n; i++, ++p_x, ++p_out) {
    if (*p_x != TRUE) {
      break;
    }
  }
  if (i == n) {
    return out;
  }

  // set to NA as long as x[i] is NA or TRUE
  for (; i < n; i++, ++p_x, ++p_out) {
    if (*p_x == FALSE) {
      break;
    }
    *p_out = NA_LOGICAL;
  }

  if (i == n) {
    return out;
  }

  // then if we are here, the rest is FALSE
  for (; i < n; i++, ++p_out) {
    *p_out = FALSE;
  }
  return out;
}

//' @export
//' @rdname cumall
// [[Rcpp::export(rng = false)]]
Rcpp::LogicalVector cumany(Rcpp::LogicalVector x) {
  int n = x.length();
  Rcpp::LogicalVector out(n, FALSE);
  int* p_x = x.begin();
  int* p_out = out.begin();

  // nothing to do as long as x[i] is FALSE
  int i = 0 ;
  for (; i < n; i++, ++p_x, ++p_out) {
    if (*p_x != FALSE) {
      break;
    }
  }
  if (i == n) {
    return out;
  }

  // set to NA as long as x[i] is NA or FALSE
  for (; i < n; i++, ++p_x, ++p_out) {
    if (*p_x == TRUE) {
      break;
    }
    *p_out = NA_LOGICAL;
  }

  if (i == n) {
    return out;
  }

  // then if we are here, the rest is TRUE
  for (; i < n; i++, ++p_out) {
    *p_out = TRUE;
  }
  return out;

}

//' @export
//' @rdname cumall
// [[Rcpp::export(rng = false)]]
Rcpp::NumericVector cummean(Rcpp::NumericVector x) {
  int n = x.length();
  Rcpp::NumericVector out(Rcpp::no_init(n));

  double sum = out[0] = x[0];
  for (int i = 1; i < n; i++) {
    sum += x[i];
    out[i] = sum / (i + 1.0);
  }

  return out;
}
