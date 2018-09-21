#include "pch.h"
#include <dplyr/main.h>

#include <dplyr/data/DataMask.h>

using namespace Rcpp;

// [[Rcpp::export]]
SEXP materialize_binding(int idx, List mask_proxy_xp_wrapped) {
  // This is called from a closure created by .make_active_binding_fun().
  // Naked XPtrs have unusual copy semantics, this is why the closure
  // stores a list need to wrap in a list.
  //
  // From R-exts: They are unusual in their copying semantics in that
  // when an R object is copied, the external pointer object is not duplicated.
  // (For this reason external pointers should only be used as part of an object
  // with normal semantics, for example an attribute or an element of a list.)
  XPtr<DataMaskWeakProxyBase> mask_proxy_xp = mask_proxy_xp_wrapped[0];
  return mask_proxy_xp->materialize(idx);
}
