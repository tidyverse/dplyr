#include "pch.h"
#include <dplyr/main.h>

#include <dplyr/HybridHandlerMap.h>

#include <dplyr/Result/ILazySubsets.h>

#include <dplyr/Result/In.h>

using namespace Rcpp;
using namespace dplyr;

Result* in_prototype(SEXP call, const ILazySubsets& subsets, int) {
  SEXP lhs = maybe_rhs(CADR(call));
  SEXP rhs = maybe_rhs(CADDR(call));

  // if lhs is not a symbol, let R handle it
  if (TYPEOF(lhs) != SYMSXP) return 0;

  SymbolString name = SymbolString(Symbol(lhs));

  // if the lhs is not in the data, let R handle it
  if (!subsets.has_variable(name)) return 0;

  SEXP v = subsets.get_variable(name);

  // if the type of the data is not the same as the type of rhs,
  // including if it needs evaluation, let R handle it
  if (TYPEOF(v) != TYPEOF(rhs)) return 0;

  // otherwise use hybrid version
  switch (TYPEOF(v)) {
  case LGLSXP:
    return new In<LGLSXP>(v, rhs);
  case INTSXP:
    return new In<INTSXP>(v, rhs);
  case REALSXP:
    return new In<REALSXP>(v, rhs);
  case STRSXP:
    return new In<STRSXP>(v, rhs);
  default:
    break;
  }

  // type not handled
  return 0;

}

void install_in_handlers(HybridHandlerMap& handlers) {
  handlers[ Rf_install("%in%") ] = in_prototype;
}
