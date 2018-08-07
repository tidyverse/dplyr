#include "pch.h"
#include <dplyr/main.h>

#include <tools/hash.h>

#include <dplyr/Result/ILazySubsets.h>

#include <dplyr/DataMask_bindings_active.h>

using namespace Rcpp;
using namespace dplyr;

namespace dplyr {

struct FindFunData {
  const SEXP symbol;
  const SEXP env;
  SEXP res;
  bool forced;

  FindFunData(SEXP symbol_, SEXP env_) :
    symbol(symbol_),
    env(env_),
    res(R_NilValue),
    forced(false)
  {}

  Rboolean findFun();
  static void protected_findFun(void* data);
  void protected_findFun();
};

Rboolean FindFunData::findFun() {
  return R_ToplevelExec(protected_findFun, reinterpret_cast<void*>(this));
}

void FindFunData::protected_findFun(void* data) {
  FindFunData* find_data = reinterpret_cast<FindFunData*>(data);
  find_data->protected_findFun();
}

void FindFunData::protected_findFun() {
  SEXP rho = env;
  SEXP vl;

  while (rho != R_EmptyEnv) {
    vl = Rf_findVarInFrame3(rho, symbol, TRUE);

    if (vl != R_UnboundValue) {
      // a promise, we need to evaluate it to find out if it
      // is a function promise
      if (TYPEOF(vl) == PROMSXP) {
        PROTECT(vl);
        vl = Rf_eval(vl, rho);
        UNPROTECT(1);
      }

      // we found a function
      if (TYPEOF(vl) == CLOSXP || TYPEOF(vl) == BUILTINSXP || TYPEOF(vl) == SPECIALSXP) {
        res = vl;
        return;
      }

      // a missing, just let R evaluation work as we have no way to
      // assert if the missing argument would have evaluated to a function or data
      if (vl == R_MissingArg) {
        return;
      }
    }

    // go in the parent environment
    rho = ENCLOS(rho);
  }

  // we did not find a suitable function, so we force hybrid evaluation
  // that happens e.g. when dplyr is not loaded and we use n() in the expression
  forced = true;
  return;
}

IHybridCallback::~IHybridCallback() {}


}
