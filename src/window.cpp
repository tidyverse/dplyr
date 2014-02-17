#include <Rcpp.h>
using namespace Rcpp;

//' Cumulativate versions of any, all, and mean
//'
//' dplyr adds \code{cumall}, \code{cumany}, and \code{cummean} to complete
//' R's set of cumulate functions to match the aggregation functions available
//' in most databases
//'
//' @param x For \code{cumall} & \code{cumany}, a logical vector; for
//'   \code{cummean} an integer or numeric vector
//' @export
// [[Rcpp::export]]
LogicalVector cumall(LogicalVector x) {
  int n = x.length();
  LogicalVector out(n);

  out[0] = x[0];
  for (int i = 1; i < n; i++) {
    out[i] = x[i] && out[i - 1];
  }

  return out;
}

//' @export
//' @rdname cumall
// [[Rcpp::export]]
LogicalVector cumany(LogicalVector x) {
  int n = x.length();
  LogicalVector out(n);

  out[0] = x[0];
  for (int i = 1; i < n; i++) {
    out[i] = x[i] || out[i - 1];
  }

  return out;
}

//' @export
//' @rdname cumall
// [[Rcpp::export]]
NumericVector cummean(NumericVector x) {
  int n = x.length();
  NumericVector out(n);

  out[0] = x[0];
  for (int i = 1; i < n; i++) {
    out[i] = out[i - 1] * ((i * 1.0) / (i + 1)) + x[i] / (i + 1);
  }

  return out;
}
