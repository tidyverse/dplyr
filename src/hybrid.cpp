#include "pch.h"
#include <dplyr/main.h>

#include <tools/hash.h>

#include <dplyr/Hybrid.h>
#include <dplyr/HybridHandlerMap.h>

#include <dplyr/Result/ILazySubsets.h>

#include <dplyr/DataMask_bindings_active.h>

using namespace Rcpp;
using namespace dplyr;

HybridHandlerMap& get_handlers() {
  static HybridHandlerMap handlers;
  if (!handlers.size()) {
    install_offset_handlers(handlers);
    install_in_handlers(handlers);
    install_debug_handlers(handlers);
  }
  return handlers;
}

void registerHybridHandler(const char* name, HybridHandler proto) {
  get_handlers()[Rf_install(name)] = proto;
}

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


bool HybridHandler::hybrid(SEXP symbol, SEXP rho) const {
  // the `protected_findFun` above might longjump so
  // we evaluate it in a top level context
  FindFunData find_data(symbol, rho);
  Rboolean success = find_data.findFun();

  // success longjumped so force hybrid
  if (!success) return true;

  if (find_data.forced) {
    if (origin == DPLYR && symbol != Rf_install("n")) {
      warning("hybrid evaluation forced for `%s`. Please use dplyr::%s() or library(dplyr) to remove this warning.", CHAR(PRINTNAME(symbol)), CHAR(PRINTNAME(symbol)));
    }
    return true;
  }

  return find_data.res == reference;
}

Result* get_handler(SEXP call, const ILazySubsets& subsets, const Environment& env) {
  LOG_INFO << "Looking up hybrid handler for call of type " << type2name(call);

  if (TYPEOF(call) == LANGSXP) {
    int depth = Rf_length(call);

    HybridHandlerMap& handlers = get_handlers();

    bool in_dplyr_namespace = false;
    SEXP fun_symbol = CAR(call);
    // interpret dplyr::fun() as fun(). #3309
    if (TYPEOF(fun_symbol) == LANGSXP &&
        CAR(fun_symbol) == Rf_install("::") &&
        CADR(fun_symbol) == Rf_install("dplyr")
       ) {
      fun_symbol = CADDR(fun_symbol);
      in_dplyr_namespace = true;
    }

    if (TYPEOF(fun_symbol) != SYMSXP) {
      LOG_VERBOSE << "Not a function: " << type2name(fun_symbol);
      return 0;
    }

    LOG_VERBOSE << "Searching hybrid handler for function " << CHAR(PRINTNAME(fun_symbol));

    // give up if the symbol is not known
    HybridHandlerMap::const_iterator it = handlers.find(fun_symbol);
    if (it == handlers.end()) {
      LOG_VERBOSE << "Not found";
      return 0;
    }

    if (!in_dplyr_namespace) {
      // no hybrid evaluation if the symbol evaluates to something else than
      // is expected. This would happen if e.g. the mean function has been shadowed
      // mutate( x = mean(x) )
      // if `mean` evaluates to something other than `base::mean` then no hybrid.

      if (!it->second.hybrid(fun_symbol, env)) return 0;
    }

    LOG_INFO << "Using hybrid handler for " << CHAR(PRINTNAME(fun_symbol));

    return it->second.handler(call, subsets, depth - 1);
  }
  return 0;
}

IHybridCallback::~IHybridCallback() {}


}
