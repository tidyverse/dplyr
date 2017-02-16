#include <dplyr/main.h>

#include <dplyr/HybridHandlerMap.h>

#include <dplyr/Result/ILazySubsets.h>

#include <dplyr/Result/Lead.h>
#include <dplyr/Result/Lag.h>

using namespace Rcpp;
using namespace dplyr;

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
Result* leadlag_prototype(SEXP call, const ILazySubsets& subsets, int nargs) {
  LeadLag args(call);
  if (!args.ok) return 0;
  RObject& data = args.data;

  if (TYPEOF(data) != SYMSXP)
    return 0;

  SymbolString name = SymbolString(Symbol(data));
  if (subsets.count(name)) {
    bool is_summary = subsets.is_summary(name);
    int n = args.n;
    data = subsets.get_variable(name);

    switch (TYPEOF(data)) {
    case INTSXP:
      return new Templ<INTSXP>(data, n, args.def, is_summary);
    case REALSXP:
      return new Templ<REALSXP>(data, n, args.def, is_summary);
    case STRSXP:
      return new Templ<STRSXP>(data, n, args.def, is_summary);
    case LGLSXP:
      return new Templ<LGLSXP>(data, n, args.def, is_summary);
    default:
      break;
    }
  }
  return 0;
}

void install_offset_handlers(HybridHandlerMap& handlers) {
  handlers[ Rf_install("lead") ] = leadlag_prototype<Lead>;
  handlers[ Rf_install("lag") ] = leadlag_prototype<Lag>;
}
