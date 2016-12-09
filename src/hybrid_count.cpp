#include <dplyr/main.h>

#include <dplyr/HybridHandlerMap.h>

#include <dplyr/MultipleVectorVisitors.h>

#include <dplyr/Result/ILazySubsets.h>

#include <dplyr/Result/Count.h>
#include <dplyr/Result/Count_Distinct.h>

#include <tools/constfold.h>
#include <tools/match.h>
#include <tools/na_rm.h>

using namespace Rcpp;
using namespace dplyr;

Result* count_prototype(SEXP args, const ILazySubsets&, int) {
  if (Rf_length(args) != 1)
    stop("n does not take arguments");
  return new Count;
}

SEXP get_n_distinct() {
  static Function n_distinct("n_distinct", Environment::namespace_env("dplyr"));
  return n_distinct;
}

Result* count_distinct_prototype(SEXP call, const ILazySubsets& subsets, int nargs) {
  MultipleVectorVisitors visitors;
  bool na_rm = false;

  call = r_match_call(get_n_distinct(), call);

  for (SEXP p = CDR(call); !Rf_isNull(p); p = CDR(p)) {
    SEXP x = CAR(p);
    NaRmResult na_rm_check = eval_na_rm(p);
    if (na_rm_check != WRONG_TAG) {
      switch (na_rm_check) {
      case NA_RM_TRUE:
        na_rm = true;
        break;

      case NA_RM_FALSE:
        na_rm = false;
        break;

      default:
        LOG_VERBOSE;
        return 0;
      }
    } else if (TYPEOF(x) == SYMSXP) {
      if (!subsets.count(x)) {
        LOG_VERBOSE;
        return 0;
      }
      visitors.push_back(subsets.get_variable(x));
    } else {
      return 0;
    }
  }

  if (visitors.size() == 0) {
    LOG_VERBOSE;
    return 0;
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
