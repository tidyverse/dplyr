#include <dplyr/main.h>

#include <dplyr/HybridHandlerMap.h>

#include <dplyr/Result/ILazySubsets.h>

#include <dplyr/Result/In.h>

#include <tools/constfold.h>

using namespace Rcpp;
using namespace dplyr;

Result* in_prototype(SEXP call, const ILazySubsets& subsets, int nargs) {
  SEXP lhs = CADR(call);
  SEXP rhse = CADDR(call);

  // if lhs is not a symbol, let R handle it
  if (TYPEOF(lhs) != SYMSXP) return 0;

  // if the lhs is not in the data, let R handle it
  if (!subsets.count(lhs)) return 0;

  SEXP v = subsets.get_variable(lhs);

  SEXP rhs = r_constfold(rhse);

  // if the type of the data is not the same as the type of rhs (after constant folding),
  // including if it still needs evaluation, let R handle it
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
