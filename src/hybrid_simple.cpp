#include "pch.h"
#include <dplyr/main.h>

#include <dplyr/HybridHandlerMap.h>

#include <dplyr/Result/ILazySubsets.h>

#include <dplyr/Result/Mean.h>
#include <dplyr/Result/Sum.h>
#include <dplyr/Result/Var.h>
#include <dplyr/Result/Sd.h>

using namespace Rcpp;
using namespace dplyr;

template <template <int, bool> class Fun, bool narm>
Result* simple_prototype_impl(SEXP arg, bool is_summary) {
  // if not hybridable, just let R handle it
  if (!hybridable(arg)) return 0;

  switch (TYPEOF(arg)) {
  case INTSXP:
    return new Fun<INTSXP, narm>(arg, is_summary);
  case REALSXP:
    return new Fun<REALSXP, narm>(arg, is_summary);
  default:
    break;
  }
  return 0;
}

template <template <int, bool> class Fun>
Result* simple_prototype(SEXP call, const ILazySubsets& subsets, int nargs) {
  if (nargs == 0) return 0;
  SEXP arg = maybe_rhs(CADR(call));
  bool is_summary = false;
  if (TYPEOF(arg) == SYMSXP) {
    SymbolString name = SymbolString(Symbol(arg));
    if (subsets.has_variable(name)) {
      // we have a symbol from the data - great
      is_summary = subsets.is_summary(name);
      arg = subsets.get_variable(name);
    } else {
      // we have a symbol but we don't know about it, so we give up and let R evaluation handle it
      return 0;
    }
  } else {
    // anything else: expressions, constants ...
    // workaround for now : we just let R deal with it
    // of course this needs some specializations, i.e. sum(1) does not need R to get involved
    return 0;
  }

  if (nargs == 1) {
    return simple_prototype_impl<Fun, false>(arg, is_summary);
  } else if (nargs == 2) {
    SEXP arg2 = CDDR(call);
    // we know how to handle fun( ., na.rm = TRUE/FALSE )
    if (TAG(arg2) == R_NaRmSymbol) {
      SEXP narm = CAR(arg2);
      if (TYPEOF(narm) == LGLSXP && LENGTH(narm) == 1) {
        if (LOGICAL(narm)[0] == TRUE) {
          return simple_prototype_impl<Fun, true>(arg, is_summary);
        } else {
          return simple_prototype_impl<Fun, false>(arg, is_summary);
        }
      }
    }
  }
  return 0;
}

void install_simple_handlers(HybridHandlerMap& handlers) {
  handlers[ Rf_install("mean") ] = simple_prototype<dplyr::Mean>;
  handlers[ Rf_install("var") ] = simple_prototype<dplyr::Var>;
  handlers[ Rf_install("sd") ] = simple_prototype<dplyr::Sd>;
  handlers[ Rf_install("sum") ] = simple_prototype<dplyr::Sum>;
}
