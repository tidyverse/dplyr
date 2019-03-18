#include "pch.h"
#include <dplyr/main.h>

#include <dplyr/data/DataMask.h>

// [[Rcpp::export(rng = false)]]
SEXP materialize_binding(int idx, Rcpp::XPtr<dplyr::DataMaskWeakProxyBase> mask_proxy_xp) {
  LOG_VERBOSE << idx;

  return mask_proxy_xp->materialize(idx);
}
