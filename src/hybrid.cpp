#include <dplyr/main.h>

#include <tools/hash.h>

#include <dplyr/Hybrid.h>
#include <dplyr/HybridHandlerMap.h>

#include <dplyr/Result/LazySubsets.h>
#include <dplyr/Result/Rank.h>
#include <dplyr/Result/ConstantResult.h>

#include <dplyr/Result/Lead.h>
#include <dplyr/Result/Lag.h>
#include <dplyr/Result/CumSum.h>
#include <dplyr/Result/CumMin.h>
#include <dplyr/Result/CumMax.h>
#include <dplyr/Result/In.h>

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

template < template <int> class Templ>
Result* cumfun_prototype(SEXP call, const LazySubsets& subsets, int nargs) {
  if (nargs != 1) return 0;
  RObject data(CADR(call));
  if (TYPEOF(data) == SYMSXP) {
    data = subsets.get_variable(data);
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

Result* in_prototype(SEXP call, const LazySubsets& subsets, int nargs) {
  SEXP lhs = CADR(call);
  SEXP rhs = CADDR(call);

  // if lhs is not a symbol, let R handle it
  if (TYPEOF(lhs) != SYMSXP) return 0;

  // if the lhs is not in the data, let R handle it
  if (!subsets.count(lhs)) return 0;

  SEXP v = subsets.get_variable(lhs);

  // if the type of the data is not the same as the type of rhs,
  // including if it needs evaluation, let R handle it
  if (TYPEOF(v) != TYPEOF(rhs)) return 0;

  // otherwise use hybrid version
  switch (TYPEOF(v)) {
  case STRSXP:
    return new In<STRSXP>(v, rhs);
  default:
    break;
  }

  // type not handled
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

    // handlers[ Rf_install( "%in%" ) ] = in_prototype;

    install_simple_handlers(handlers);
    install_minmax_handlers(handlers);
    install_count_handlers(handlers);
    install_nth_handlers(handlers);
    install_window_handlers(handlers);
    install_offset_handlers(handlers);
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
  }
  return 0;
}

namespace dplyr {

Result* get_handler(SEXP call, const LazySubsets& subsets, const Environment& env) {
  if (TYPEOF(call) == LANGSXP) {
    int depth = Rf_length(call);
    HybridHandlerMap& handlers = get_handlers();
    SEXP fun_symbol = CAR(call);
    if (TYPEOF(fun_symbol) != SYMSXP) return 0;

    HybridHandlerMap::const_iterator it = handlers.find(fun_symbol);
    if (it == handlers.end()) return 0;

    return it->second(call, subsets, depth - 1);
  } else if (TYPEOF(call) == SYMSXP) {
    if (!subsets.count(call)) {
      SEXP data = env.find(CHAR(PRINTNAME(call)));
      if (Rf_length(data) == 1) return constant_handler(data);
    }
  } else {
    // TODO: perhaps deal with SYMSXP separately
    if (Rf_length(call) == 1) return constant_handler(call);
  }
  return 0;
}

}

void registerHybridHandler(const char* name, HybridHandler proto) {
  get_handlers()[ Rf_install(name) ] = proto;
}

bool can_simplify(SEXP call) {
  if (TYPEOF(call) == LISTSXP) {
    bool res = can_simplify(CAR(call));
    if (res) return true;
    return can_simplify(CDR(call));
  }

  if (TYPEOF(call) == LANGSXP) {
    SEXP fun_symbol = CAR(call);
    if (TYPEOF(fun_symbol) != SYMSXP) return false;

    if (get_handlers().count(fun_symbol)) return true;

    return can_simplify(CDR(call));
  }
  return false;
}
