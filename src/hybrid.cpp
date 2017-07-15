#include "pch.h"
#include <dplyr/main.h>

#include <tools/hash.h>

#include <dplyr/Hybrid.h>
#include <dplyr/HybridHandlerMap.h>

#include <dplyr/Result/ILazySubsets.h>
#include <dplyr/Result/Rank.h>
#include <dplyr/Result/ConstantResult.h>

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
    handlers[ Rf_install( "cumsum")      ] = cumfun_prototype<CumSum>;
    handlers[ Rf_install( "cummin")      ] = cumfun_prototype<CumMin>;
    handlers[ Rf_install( "cummax")      ] = cumfun_prototype<CumMax>;
    */

    install_simple_handlers(handlers);
    install_minmax_handlers(handlers);
    install_count_handlers(handlers);
    install_nth_handlers(handlers);
    install_window_handlers(handlers);
    install_offset_handlers(handlers);
    install_in_handlers(handlers);
    install_debug_handlers(handlers);
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

  SEXP process(const GroupedDataFrame& gdf) {
    if (subsets.is_summary(name)) {
      // No need to check length since the summary has already been checked
      return subsets.get_variable(name);
    } else {
      check_length(gdf.max_group_size(), 1, "a summary value", name);
      return process(*gdf.group_begin());
    }
  }

  SEXP process(const RowwiseDataFrame&) {
    return subsets.get_variable(name);
  }

  virtual SEXP process(const FullDataFrame&) {
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

namespace dplyr {

Result* get_handler(SEXP call, const ILazySubsets& subsets, const Environment& env) {
  LOG_INFO << "Looking up hybrid handler for call of type " << type2name(call);

  if (TYPEOF(call) == LANGSXP) {
    int depth = Rf_length(call);
    HybridHandlerMap& handlers = get_handlers();
    SEXP fun_symbol = CAR(call);
    if (TYPEOF(fun_symbol) != SYMSXP) {
      LOG_VERBOSE << "Not a function: " << type2name(fun_symbol);
      return 0;
    }

    LOG_VERBOSE << "Searching hybrid handler for function " << CHAR(PRINTNAME(fun_symbol));

    HybridHandlerMap::const_iterator it = handlers.find(fun_symbol);
    if (it == handlers.end()) {
      LOG_VERBOSE << "Not found";
      return 0;
    }

    LOG_INFO << "Using hybrid handler for " << CHAR(PRINTNAME(fun_symbol));

    return it->second(call, subsets, depth - 1);
  } else if (TYPEOF(call) == SYMSXP) {
    SymbolString sym = SymbolString(Symbol(call));

    LOG_VERBOSE << "Searching hybrid handler for symbol " << sym.get_utf8_cstring();

    if (subsets.has_variable(sym)) {
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

}

void registerHybridHandler(const char* name, HybridHandler proto) {
  get_handlers()[Rf_install(name)] = proto;
}
