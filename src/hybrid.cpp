#include "pch.h"
#include <dplyr/main.h>

#include <tools/hash.h>

#include <dplyr/Hybrid.h>
#include <dplyr/HybridHandlerMap.h>

#include <dplyr/Result/ILazySubsets.h>
#include <dplyr/Result/Rank.h>
#include <dplyr/Result/ConstantResult.h>

#include <dplyr/DataMask_bindings_active.h>

using namespace Rcpp;
using namespace dplyr;

bool has_no_class(const RObject& arg) {
  return RCPP_GET_CLASS(arg) == R_NilValue;
}

bool hybridable(RObject arg) {
  if (Rf_inherits(arg, "Date") || Rf_inherits(arg, "POSIXct") || Rf_inherits(arg, "difftime")) return true;

  if (arg.isObject() || arg.isS4()) return false;
  int type = arg.sexp_type();
  switch (type) {
  case INTSXP:
  case REALSXP:
  case LGLSXP:
  case STRSXP:
  case CPLXSXP:
  case RAWSXP:
    return has_no_class(arg);
  default:
    break;
  }
  return false;
}

template <template <int> class Templ>
Result* cumfun_prototype(SEXP call, const ILazySubsets& subsets, int nargs) {
  if (nargs != 1) return 0;
  RObject data(CADR(call));
  if (TYPEOF(data) == SYMSXP) {
    data = subsets.get_variable(SymbolString(Symbol(data)));
  }
  switch (TYPEOF(data)) {
  case INTSXP:
    return new Templ<INTSXP>(data);
  case REALSXP:
    return new Templ<REALSXP>(data);
  default:
    break;
  }
  return 0;
}

HybridHandlerMap& get_handlers() {
  static HybridHandlerMap handlers;
  if (!handlers.size()) {
    /*
    handlers[ Rf_install( "cumsum")      ] = HybridHandler( cumfun_prototype<CumSum>, R_NilValue );
    handlers[ Rf_install( "cummin")      ] = HybridHandler( cumfun_prototype<CumMin>, R_NilValue );
    handlers[ Rf_install( "cummax")      ] = HybridHandler( cumfun_prototype<CumMax>, R_NilValue );
    */

    install_minmax_handlers(handlers);
    install_window_handlers(handlers);
    install_offset_handlers(handlers);
    install_in_handlers(handlers);
    install_debug_handlers(handlers);
    install_group_handlers(handlers);
  }
  return handlers;
}

Result* constant_handler(SEXP constant) {
  switch (TYPEOF(constant)) {
  case INTSXP:
  {
    if (Rf_inherits(constant, "Date")) return new TypedConstantResult<INTSXP>(constant, get_date_classes());
    return new ConstantResult<INTSXP>(constant);
  }
  case REALSXP:
  {
    if (Rf_inherits(constant, "difftime")) return new DifftimeConstantResult<REALSXP>(constant);
    if (Rf_inherits(constant, "POSIXct")) return new TypedConstantResult<REALSXP>(constant, get_time_classes());
    if (Rf_inherits(constant, "Date")) return new TypedConstantResult<REALSXP>(constant, get_date_classes());
    return new ConstantResult<REALSXP>(constant);
  }
  case STRSXP:
    return new ConstantResult<STRSXP>(constant);
  case LGLSXP:
    return new ConstantResult<LGLSXP>(constant);
  case CPLXSXP:
    return new ConstantResult<CPLXSXP>(constant);
  default:
    return 0;
  }
}

class VariableResult : public Result {
public:
  VariableResult(const ILazySubsets& subsets_, const SymbolString& name_) : subsets(subsets_), name(name_)  {}

  SEXP process(const GroupedDataFrame&) {
    if (subsets.is_summary(name)) {
      // No need to check length since the summary has already been checked
      return subsets.get_variable(name);
    } else {
      stop("VariableResult::process() needs a summary variable");
    }
  }

  SEXP process(const RowwiseDataFrame&) {
    return subsets.get_variable(name);
  }

  virtual SEXP process(const SlicingIndex& index) {
    return subsets.get(name, index);
  }

private:
  const ILazySubsets& subsets;
  SymbolString name;
};

Result* variable_handler(const ILazySubsets& subsets, const SymbolString& variable) {
  return new VariableResult(subsets, variable);
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
  } else if (TYPEOF(call) == SYMSXP) {
    SymbolString sym = SymbolString(Symbol(call));

    LOG_VERBOSE << "Searching hybrid handler for symbol " << sym.get_utf8_cstring();

    if (subsets.has_variable(sym)) {
      if (!subsets.is_summary(sym)) return 0;

      LOG_VERBOSE << "Using hybrid variable handler";
      return variable_handler(subsets, sym);
    }
    else {
      SEXP data;
      try {
        data = env.find(sym.get_string());
      } catch (Rcpp::binding_not_found) {
        return NULL;
      }

      // Constants of length != 1 are handled via regular evaluation
      if (Rf_length(data) == 1) {
        LOG_VERBOSE << "Using hybrid constant handler";
        return constant_handler(data);
      }
    }
  } else {
    // TODO: perhaps deal with SYMSXP separately
    if (Rf_length(call) == 1) return constant_handler(call);
  }
  return 0;
}

IHybridCallback::~IHybridCallback() {}


}
