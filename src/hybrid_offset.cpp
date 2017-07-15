#include "pch.h"
#include <dplyr/main.h>

#include <dplyr/HybridHandlerMap.h>

#include <dplyr/Result/ILazySubsets.h>

#include <dplyr/Result/Lead.h>
#include <dplyr/Result/Lag.h>

using namespace Rcpp;
using namespace dplyr;

struct LeadLag {

  explicit LeadLag(SEXP call) : data(R_NilValue), n(1), def(R_NilValue), ok(false) {

    SEXP p = CDR(call);
    SEXP tag = TAG(p);
    if (tag != R_NilValue && tag != Rf_install("x"))
      return;
    data = maybe_rhs(CAR(p));
    p = CDR(p);

    SEXP tag_default = Rf_install("default");
    SEXP tag_n = Rf_install("n");
    bool got_n = false;
    bool got_default = false;

    while (!Rf_isNull(p)) {
      tag = TAG(p);
      if (!Rf_isNull(tag) && tag != tag_n && tag != tag_default)
        return;
      if (!got_n && (Rf_isNull(tag) || tag == tag_n)) {
        SEXP n_ = CAR(p);
        if (TYPEOF(n_) != INTSXP && TYPEOF(n_) != REALSXP)
          return;
        n = as<int>(n_);
        got_n = true;
      }
      else if (!got_default && (Rf_isNull(tag) || tag == tag_default)) {
        def = CAR(p);
        if (TYPEOF(def) == LANGSXP) return;
        got_default = true;
      }
      else
        return;
      p = CDR(p);
    }

    ok = true;
  }

  RObject data;
  int n;
  RObject def;

  bool ok;

};

template < template<int> class Templ>
Result* leadlag_prototype(SEXP call, const ILazySubsets& subsets, int) {
  LeadLag args(call);
  if (!args.ok) return 0;
  RObject& data = args.data;

  if (TYPEOF(data) != SYMSXP)
    return 0;

  SymbolString name = SymbolString(Symbol(data));
  if (subsets.has_variable(name) == 0)
    return 0;

  bool is_summary = subsets.is_summary(name);
  int n = args.n;
  data = subsets.get_variable(name);

  switch (TYPEOF(data)) {
  case INTSXP:
    return new Templ<INTSXP>(data, n, args.def, is_summary);
  case REALSXP:
    return new Templ<REALSXP>(data, n, args.def, is_summary);
  case CPLXSXP:
    return new Templ<CPLXSXP>(data, n, args.def, is_summary);
  case STRSXP:
    return new Templ<STRSXP>(data, n, args.def, is_summary);
  case LGLSXP:
    return new Templ<LGLSXP>(data, n, args.def, is_summary);
  default:
    return 0;
  }
}

void install_offset_handlers(HybridHandlerMap& handlers) {
  handlers[ Rf_install("lead") ] = leadlag_prototype<Lead>;
  handlers[ Rf_install("lag") ] = leadlag_prototype<Lag>;
}
