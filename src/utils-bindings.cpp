#include "pch.h"
#include <dplyr/main.h>

#include <dplyr/data/DataMask.h>

using namespace Rcpp;

// [[Rcpp::export]]
SEXP materialize_binding(int idx, XPtr<DataMaskWeakProxyBase> mask_proxy_xp) {
  LOG_VERBOSE << idx;

  return mask_proxy_xp->materialize(idx);
}
