#include "pch.h"
#include <dplyr/main.h>

#include <dplyr/HybridHandlerMap.h>

#include <dplyr/MultipleVectorVisitors.h>

#include <dplyr/Result/ILazySubsets.h>

#include <dplyr/Result/Count.h>
#include <dplyr/Result/Count_Distinct.h>

using namespace Rcpp;
using namespace dplyr;

Result* count_prototype(SEXP args, const ILazySubsets&, int) {
  if (Rf_length(args) != 1)
    stop("`n()` does not take arguments");
  return new Count;
}

Result* count_distinct_prototype(SEXP call, const ILazySubsets& subsets, int) {
  MultipleVectorVisitors visitors;
  bool na_rm = false;

  for (SEXP p = CDR(call); !Rf_isNull(p); p = CDR(p)) {
    SEXP x = maybe_rhs(CAR(p));
    if (!Rf_isNull(TAG(p)) && TAG(p) == Rf_install("na.rm")) {
      if (TYPEOF(x) == LGLSXP && Rf_length(x) == 1) {
        na_rm = LOGICAL(x)[0];
      } else {
        stop("incompatible value for `na.rm` argument");
      }
    } else if (TYPEOF(x) == SYMSXP) {
      SymbolString name = SymbolString(Symbol(x));
      visitors.push_back(subsets.get_variable(name));
    } else {
      return 0;
    }
  }

  if (visitors.size() == 0) {
    stop("Need at least one column for `n_distinct()`");
  }

  if (na_rm) {
    return new Count_Distinct_Narm<MultipleVectorVisitors>(visitors);
  } else {
    return new Count_Distinct<MultipleVectorVisitors>(visitors);
  }
}

void install_count_handlers(HybridHandlerMap& handlers) {
  handlers[ Rf_install("n") ] = count_prototype;
  handlers[ Rf_install("n_distinct") ] = count_distinct_prototype;
}
