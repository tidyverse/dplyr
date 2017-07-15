#include "pch.h"
#include <dplyr/main.h>

#include <dplyr/HybridHandlerMap.h>

#include <dplyr/Result/ILazySubsets.h>

#include <dplyr/Result/MinMax.h>

using namespace Rcpp;
using namespace dplyr;

template<template <int, bool, bool> class Tmpl, bool MINIMUM, bool NA_RM>
Result* minmax_prototype_impl(SEXP arg, bool is_summary) {
  arg = maybe_rhs(arg);
  if (!hybridable(arg)) return 0;

  switch (TYPEOF(arg)) {
  case INTSXP:
    return new Tmpl<INTSXP, MINIMUM, NA_RM>(arg, is_summary);
  case REALSXP:
    return new Tmpl<REALSXP, MINIMUM, NA_RM>(arg, is_summary);
  default:
    break;
  }
  return 0;
}

template<template <int, bool, bool> class Tmpl, bool MINIMUM>
Result* minmax_prototype(SEXP call, const ILazySubsets& subsets, int nargs) {
  using namespace dplyr;
  // we only can handle 1 or two arguments
  if (nargs == 0 || nargs > 2) return 0;

  // the first argument is the data to operate on
  SEXP arg = maybe_rhs(CADR(call));

  bool is_summary = false;
  if (TYPEOF(arg) == SYMSXP) {
    SymbolString name = SymbolString(Symbol(arg));
    if (subsets.has_variable(name)) {
      is_summary = subsets.is_summary(name);
      arg = subsets.get_variable(name);
    }
    else return 0;
  } else {
    return 0;
  }

  if (nargs == 1) {
    return minmax_prototype_impl<Tmpl, MINIMUM, false>(arg, is_summary);
  } else if (nargs == 2) {
    SEXP arg2 = CDDR(call);
    // we know how to handle fun( ., na.rm = TRUE/FALSE )
    if (TAG(arg2) == R_NaRmSymbol) {
      SEXP narm = CAR(arg2);
      if (TYPEOF(narm) == LGLSXP && LENGTH(narm) == 1) {
        if (LOGICAL(narm)[0] == TRUE) {
          return minmax_prototype_impl<Tmpl, MINIMUM, true>(arg, is_summary);
        } else {
          return minmax_prototype_impl<Tmpl, MINIMUM, false>(arg, is_summary);
        }
      }
    }
  }
  return 0;
}

void install_minmax_handlers(HybridHandlerMap& handlers) {
  handlers[Rf_install("min")] = minmax_prototype<dplyr::MinMax, true>;
  handlers[Rf_install("max")] = minmax_prototype<dplyr::MinMax, false>;
}
