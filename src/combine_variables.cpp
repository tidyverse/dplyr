#include "pch.h"
#include <Rcpp.h>
using namespace Rcpp;

int vector_sign(IntegerVector x) {
  bool pos = false, neg = false;

  int n = x.size();
  for (int i = 0; i < n; ++i) {
    if (x[i] < 0) neg = true;
    if (x[i] > 0) pos = true;

    if (neg && pos) break;
  }

  if (neg == pos) {
    // Either mixed, or all zeros
    return 0;
  } else if (neg) {
    return -1;
  } else {
    return 1;
  }
}
