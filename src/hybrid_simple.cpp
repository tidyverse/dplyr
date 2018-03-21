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
Result* simple_prototype_impl(SEXP arg) {
  // if not hybridable, just let R handle it
  if (!hybridable(arg)) return 0;

  switch (TYPEOF(arg)) {
  case INTSXP:
    return new Fun<INTSXP, narm>(arg);
  case REALSXP:
    return new Fun<REALSXP, narm>(arg);
  default:
    break;
  }
  return 0;
}

template <template <int, bool> class Fun>
Result* simple_prototype(SEXP call, const ILazySubsets& subsets, int nargs) {
  if (nargs == 0) return 0;
  SEXP arg = maybe_rhs(CADR(call));

  if (TYPEOF(arg) == SYMSXP) {
    SymbolString name = SymbolString(Symbol(arg));
    if (subsets.has_variable(name)) {
      // we have a symbol but it's the result of a summary, let R handle it
      // (why would you take a summary from a summary?)
      if (subsets.is_summary(name))
        return 0;
      // we have a vectorized symbol from the data - great
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
    return simple_prototype_impl<Fun, false>(arg);
  } else if (nargs == 2) {
    SEXP arg2 = CDDR(call);
    // we know how to handle fun( ., na.rm = TRUE/FALSE )
    if (TAG(arg2) == R_NaRmSymbol) {
      SEXP narm = CAR(arg2);
      if (TYPEOF(narm) == LGLSXP && LENGTH(narm) == 1) {
        if (LOGICAL(narm)[0] == TRUE) {
          return simple_prototype_impl<Fun, true>(arg);
        } else {
          return simple_prototype_impl<Fun, false>(arg);
        }
      }
    }
  }
  return 0;
}

void install_simple_handlers(HybridHandlerMap& handlers) {
  Environment ns_stats = Environment::namespace_env("stats") ;
  Environment ns_base = Environment::base_namespace() ;

  handlers[ Rf_install("mean") ] = HybridHandler(simple_prototype<dplyr::Mean>, ns_base["mean"]);
  handlers[ Rf_install("var") ] = HybridHandler(simple_prototype<dplyr::Var>, ns_stats["var"]);
  handlers[ Rf_install("sd") ] = HybridHandler(simple_prototype<dplyr::Sd>, ns_stats["sd"]);
  handlers[ Rf_install("sum") ] = HybridHandler(simple_prototype<dplyr::Sum>, ns_base["sum"]);
}
