#include <dplyr.h>

#include <dplyr/RowwiseDataFrame.h>

#include <dplyr/MultipleVectorVisitors.h>

#include <dplyr/Hybrid.h>

#include <dplyr/Result/is_smaller.h>

#include <dplyr/Result/LazySubsets.h>
#include <dplyr/Result/Rank.h>
#include <dplyr/Result/ConstantResult.h>

#include <dplyr/Result/Count.h>
#include <dplyr/Result/Count_Distinct.h>
#include <dplyr/Result/Mean.h>
#include <dplyr/Result/Sum.h>
#include <dplyr/Result/Var.h>
#include <dplyr/Result/Sd.h>
#include <dplyr/Result/min.h>
#include <dplyr/Result/max.h>
#include <dplyr/Result/Lead.h>
#include <dplyr/Result/Lag.h>
#include <dplyr/Result/CumSum.h>
#include <dplyr/Result/CumMin.h>
#include <dplyr/Result/CumMax.h>
#include <dplyr/Result/In.h>

using namespace Rcpp;
using namespace dplyr;

typedef dplyr_hash_map<SEXP,HybridHandler> HybridHandlerMap;

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

template <template <int,bool> class Fun, bool narm>
Result* simple_prototype_impl(SEXP arg, bool is_summary) {
  // if not hybridable, just let R handle it
  if (!hybridable(arg)) return 0;

  switch (TYPEOF(arg)) {
  case INTSXP:
    return new Fun<INTSXP,narm>(arg, is_summary);
  case REALSXP:
    return new Fun<REALSXP,narm>(arg, is_summary);
  default:
    break;
  }
  return 0;
}

template <template <int,bool> class Fun>
Result* simple_prototype(SEXP call, const LazySubsets& subsets, int nargs) {
  if (nargs == 0) return 0;
  SEXP arg = CADR(call);
  bool is_summary = false;
  if (TYPEOF(arg) == SYMSXP) {
    if (subsets.count(arg)) {
      // we have a symbol from the data - great
      is_summary = subsets.is_summary(arg);
      arg = subsets.get_variable(arg);
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

template< template <int, bool> class Tmpl, bool narm>
Result* minmax_prototype_impl(SEXP arg, bool is_summary) {
  if (!hybridable(arg)) return 0;

  switch (TYPEOF(arg)) {
  case INTSXP:
    return new Tmpl<INTSXP,narm>(arg, is_summary);
  case REALSXP:
    return new Tmpl<REALSXP,narm>(arg, is_summary);
  default:
    break;
  }
  return 0;
}

template< template <int, bool> class Tmpl>
Result* minmax_prototype(SEXP call, const LazySubsets& subsets, int nargs) {
  using namespace dplyr;
  // we only can handle 1 or two arguments
  if (nargs == 0 || nargs > 2) return 0;

  // the first argument is the data to operate on
  SEXP arg = CADR(call);

  bool is_summary = false;
  if (TYPEOF(arg) == SYMSXP) {
    if (subsets.count(arg)) {
      is_summary = subsets.is_summary(arg);
      arg = subsets.get_variable(arg);
    }
    else return 0;
  } else {
    return 0;
  }

  if (nargs == 1) {
    return minmax_prototype_impl<Tmpl,false>(arg, is_summary);
  } else if (nargs == 2) {
    SEXP arg2 = CDDR(call);
    // we know how to handle fun( ., na.rm = TRUE/FALSE )
    if (TAG(arg2) == R_NaRmSymbol) {
      SEXP narm = CAR(arg2);
      if (TYPEOF(narm) == LGLSXP && LENGTH(narm) == 1) {
        if (LOGICAL(narm)[0] == TRUE) {
          return minmax_prototype_impl<Tmpl,true>(arg, is_summary);
        } else {
          return minmax_prototype_impl<Tmpl,false>(arg, is_summary);
        }
      }
    }
  }
  return 0;
}

Result* count_prototype(SEXP args, const LazySubsets&, int) {
  if (Rf_length(args) != 1)
    stop("n does not take arguments");
  return new Count;
}

Result* count_distinct_prototype(SEXP call, const LazySubsets& subsets, int nargs) {
  MultipleVectorVisitors visitors;
  bool na_rm = false;

  for (SEXP p = CDR(call); !Rf_isNull(p); p = CDR(p)) {
    SEXP x = CAR(p);
    if (!Rf_isNull(TAG(p)) && TAG(p) == Rf_install("na.rm")) {
      if (TYPEOF(x) == LGLSXP && Rf_length(x) == 1) {
        na_rm = LOGICAL(x)[0];
      } else {
        stop("incompatible value for `na.rm` parameter");
      }
    } else if (TYPEOF(x) == SYMSXP) {
      visitors.push_back(subsets.get_variable(x));
    } else {
      return 0;
    }
  }

  if (visitors.size() == 0) {
    stop("need at least one column for n_distinct()");
  }

  if (na_rm) {
    return new Count_Distinct_Narm<MultipleVectorVisitors>(visitors);
  } else {
    return new Count_Distinct<MultipleVectorVisitors>(visitors);
  }
}

Result* row_number_prototype(SEXP call, const LazySubsets& subsets, int nargs) {
  if (nargs >  1 || subsets.size() == 0) return 0;

  if (nargs == 0) return new RowNumber_0();

  RObject data(CADR(call));
  if (TYPEOF(data) == LANGSXP && CAR(data) == Rf_install("desc")) {
    data = CADR(data);

    if (TYPEOF(data) == SYMSXP) {
      if (subsets.count(data)) data = subsets.get_variable(data);
      else return 0;
    }
    if (Rf_length(data) == subsets.nrows()) {
      switch (TYPEOF(data)) {
      case INTSXP:
        return new RowNumber<INTSXP,  false>(data);
      case REALSXP:
        return new RowNumber<REALSXP, false>(data);
      case STRSXP:
        return new RowNumber<STRSXP,  false>(data);
      default:
        break;
      }
    }
    return 0;
  }
  if (TYPEOF(data) == SYMSXP) {
    if (subsets.count(data)) data = subsets.get_variable(data);
    else return 0;
  }
  if (Rf_length(data) == subsets.nrows()) {
    switch (TYPEOF(data)) {
    case INTSXP:
      return new RowNumber<INTSXP,true>(data);
    case REALSXP:
      return new RowNumber<REALSXP,true>(data);
    case STRSXP:
      return new RowNumber<STRSXP,true>(data);
    default:
      break;
    }
  }
  // we don't know how to handle it.
  return 0;
}

Result* ntile_prototype(SEXP call, const LazySubsets& subsets, int nargs) {
  if (nargs != 2) return 0;

  // handle 2nd arg
  SEXP ntiles = CADDR(call);
  double number_tiles;
  try {
    number_tiles = as<int>(ntiles);
  } catch (...) {
    stop("could not convert n to scalar integer");
  }

  RObject data(CADR(call));
  if (TYPEOF(data) == LANGSXP && CAR(data) == Rf_install("desc")) {
    data = CADR(data);

    if (TYPEOF(data) == SYMSXP) {
      if (subsets.count(data)) data = subsets.get_variable(data);
      else return 0;
    }
    switch (TYPEOF(data)) {
    case INTSXP:
      return new Ntile<INTSXP,  false>(data, number_tiles);
    case REALSXP:
      return new Ntile<REALSXP, false>(data, number_tiles);
    case STRSXP:
      return new Ntile<STRSXP,  false>(data, number_tiles);
    default:
      break;
    }
  }
  if (TYPEOF(data) == SYMSXP) {
    if (subsets.count(data)) data = subsets.get_variable(data);
    else return 0;
  }
  if (subsets.nrows() != Rf_length(data)) return 0;

  switch (TYPEOF(data)) {
  case INTSXP:
    return new Ntile<INTSXP ,true>(data, number_tiles);
  case REALSXP:
    return new Ntile<REALSXP,true>(data, number_tiles);
  case STRSXP:
    return new Ntile<STRSXP ,true>(data, number_tiles);
  default:
    break;
  }
  // we don't know how to handle it.
  return 0;
}

template <typename Increment>
Result* rank_impl_prototype(SEXP call, const LazySubsets& subsets, int nargs) {
  if (nargs != 1) return 0;
  RObject data(CADR(call));

  if (TYPEOF(data) == LANGSXP && CAR(data) == Rf_install("desc")) {
    data = CADR(data);
    if (TYPEOF(data) == SYMSXP) {
      if (subsets.count(data)) data = subsets.get_variable(data);
      else return 0;
    }

    switch (TYPEOF(data)) {
    case INTSXP:
      return new Rank_Impl<INTSXP,  Increment, false>(data);
    case REALSXP:
      return new Rank_Impl<REALSXP, Increment, false>(data);
    case STRSXP:
      return new Rank_Impl<STRSXP,  Increment, false>(data);
    default:
      break;
    }
  }

  if (TYPEOF(data) == SYMSXP) {
    if (subsets.count(data)) data = subsets.get_variable(data);
    else return 0;
  }
  switch (TYPEOF(data)) {
  case INTSXP:
    return new Rank_Impl<INTSXP,  Increment, true>(data);
  case REALSXP:
    return new Rank_Impl<REALSXP, Increment, true>(data);
  case STRSXP:
    return new Rank_Impl<STRSXP,  Increment, true>(data);
  default:
    break;
  }
  // we don't know how to handle it.
  return 0;
}

struct LeadLag {

  LeadLag(SEXP call) : data(R_NilValue), n(1), def(R_NilValue), ok(true) {

    SEXP p = CDR(call);
    SEXP tag = TAG(p);
    if (tag != R_NilValue && tag != Rf_install("x")) {
      ok = false;
      return;
    }
    data = CAR(p);

    p = CDR(p);
    while (p != R_NilValue) {
      tag = TAG(p);
      if (tag != R_NilValue && tag != Rf_install("n") && tag != Rf_install("default")) {
        ok = false;
        return;
      }
      if (tag == Rf_install("n") || tag == R_NilValue) {
        try {
          n = as<int>(CAR(p));
        } catch (...) {
          SEXP n_ = CADDR(call);
          std::stringstream s;
          stop("could not convert second argument to an integer. type=%s, length = %d",
               type2name(n_), Rf_length(n_));
        }
      }
      if (tag == Rf_install("default")) {
        def = CAR(p);
        if (TYPEOF(def) == LANGSXP) ok = false;
      }
      p = CDR(p);
    }
  }

  RObject data;
  int n;
  RObject def;

  bool ok;

};

template < template<int> class Templ>
Result* leadlag_prototype(SEXP call, const LazySubsets& subsets, int nargs) {
  LeadLag args(call);
  if (!args.ok) return 0;
  RObject& data = args.data;

  if (TYPEOF(data) == SYMSXP && subsets.count(data)) {
    bool is_summary = subsets.is_summary(data);
    int n = args.n;
    data = subsets.get_variable(data);

    switch (TYPEOF(data)) {
    case INTSXP:
      return new Templ<INTSXP> (data, n, args.def, is_summary);
    case REALSXP:
      return new Templ<REALSXP>(data, n, args.def, is_summary);
    case STRSXP:
      return new Templ<STRSXP> (data, n, args.def, is_summary);
    case LGLSXP:
      return new Templ<LGLSXP> (data, n, args.def, is_summary);
    default:
      break;
    }

  }
  return 0;
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

bool argmatch(const std::string& target, const std::string& s) {
  if (s.size() > target.size()) return false;
  return target.compare(0, s.size(), s) == 0;
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
    handlers[ Rf_install("n")         ] = count_prototype;
    handlers[ Rf_install("n_distinct")   ] = count_distinct_prototype;
    handlers[ Rf_install("row_number")   ] = row_number_prototype;
    handlers[ Rf_install("ntile")      ] = ntile_prototype;

    handlers[ Rf_install("min")      ] = minmax_prototype<dplyr::Min>;
    handlers[ Rf_install("max")      ] = minmax_prototype<dplyr::Max>;

    handlers[ Rf_install("mean")       ] = simple_prototype<dplyr::Mean>;
    handlers[ Rf_install("var")      ] = simple_prototype<dplyr::Var>;
    handlers[ Rf_install("sd")        ] = simple_prototype<dplyr::Sd>;
    handlers[ Rf_install("sum")      ] = simple_prototype<dplyr::Sum>;

    handlers[ Rf_install("min_rank")     ] = rank_impl_prototype<dplyr::internal::min_rank_increment>;
    handlers[ Rf_install("percent_rank")   ] = rank_impl_prototype<dplyr::internal::percent_rank_increment>;
    handlers[ Rf_install("dense_rank")   ] = rank_impl_prototype<dplyr::internal::dense_rank_increment>;
    handlers[ Rf_install("cume_dist")    ] = rank_impl_prototype<dplyr::internal::cume_dist_increment>;

    /*
    handlers[ Rf_install( "cumsum")      ] = cumfun_prototype<CumSum>;
    handlers[ Rf_install( "cummin")      ] = cumfun_prototype<CumMin>;
    handlers[ Rf_install( "cummax")      ] = cumfun_prototype<CumMax>;
    */

    handlers[ Rf_install("lead")       ] = leadlag_prototype<Lead>;
    handlers[ Rf_install("lag")      ] = leadlag_prototype<Lag>;

    handlers[ Rf_install("first")      ] = first_prototype;
    handlers[ Rf_install("last")       ] = last_prototype;
    handlers[ Rf_install("nth")      ] = nth_prototype;

    // handlers[ Rf_install( "%in%" ) ] = in_prototype;

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
