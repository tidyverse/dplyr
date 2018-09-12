#include "pch.h"
#include <dplyr/main.h>

#include <dplyr/data/DataMask.h>

using namespace Rcpp;

// [[Rcpp::export]]
SEXP materialize_binding(int idx, XPtr<DataMaskWeakProxyBase> mask_proxy_xp) {
  return mask_proxy_xp->materialize(idx);
}
