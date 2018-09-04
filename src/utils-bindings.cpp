#include "pch.h"
#include <dplyr/main.h>

#include <dplyr/data/LazySplitSubsets.h>

using namespace Rcpp;

// [[Rcpp::export]]
SEXP materialize_binding(int idx, XPtr<LazySplitSubsetsBase> subsets) {
  return subsets->materialize(idx);
}
