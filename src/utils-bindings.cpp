#include "pch.h"
#include <dplyr/main.h>

#include <dplyr/data/DataMask.h>

using namespace Rcpp;

// [[Rcpp::export]]
SEXP materialize_binding(int idx, XPtr<DataMaskBase> subsets) {
  return subsets->materialize(idx);
}
