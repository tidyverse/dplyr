#include <dplyr/main.h>

#include <dplyr/HybridHandlerMap.h>

#include <dplyr/Result/ILazySubsets.h>

#include <dplyr/Result/Min.h>
#include <dplyr/Result/Max.h>

#include <tools/constfold.h>
#include <tools/match.h>

using namespace Rcpp;
using namespace dplyr;

template< template <int, bool> class Tmpl, bool narm>
Result* minmax_prototype_impl(SEXP arg, bool is_summary) {
  if (!hybridable(arg)) return 0;

  switch (TYPEOF(arg)) {
  case INTSXP:
    return new Tmpl<INTSXP,narm>(arg, is_summary);
  case REALSXP:
    return new Tmpl<REALSXP,narm>(arg, is_summary);
  default:
    break;
  }
  return 0;
}

SEXP get_min() {
  // min() is a primitive, but pmin() effectively has the same interface
  static Function min_("pmin", R_BaseEnv);
  return min_;
}

template< template <int, bool> class Tmpl>
Result* minmax_prototype(SEXP call, const ILazySubsets& subsets, int nargs) {
  // we only can handle 1 or two arguments
  if (nargs == 0 || nargs > 2) return 0;

  call = r_match_call(get_min(), call);

  // the first argument is the data to operate on
  SEXP arg = CADR(call);

  bool is_summary = false;
  if (TYPEOF(arg) == SYMSXP) {
    if (subsets.count(arg)) {
      is_summary = subsets.is_summary(arg);
      arg = subsets.get_variable(arg);
    }
    else return 0;
  } else {
    return 0;
  }

  if (nargs == 1) {
    return minmax_prototype_impl<Tmpl,false>(arg, is_summary);
  } else if (nargs == 2) {
    SEXP arg2 = CDDR(call);
    // we know how to handle fun( ., na.rm = TRUE/FALSE )
    if (TAG(arg2) == R_NaRmSymbol) {
      SEXP narme = CAR(arg2);
      SEXP narm = r_constfold(narme);
      if (TYPEOF(narm) == LGLSXP && LENGTH(narm) == 1) {
        if (LOGICAL(narm)[0] == TRUE) {
          return minmax_prototype_impl<Tmpl, true>(arg, is_summary);
        } else {
          return minmax_prototype_impl<Tmpl, false>(arg, is_summary);
        }
      }
    }
  }
  return 0;
}

void install_minmax_handlers(HybridHandlerMap& handlers) {
  handlers[Rf_install("min")] = minmax_prototype<dplyr::Min>;
  handlers[Rf_install("max")] = minmax_prototype<dplyr::Max>;
}
