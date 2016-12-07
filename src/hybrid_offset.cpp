#include <dplyr/main.h>

#include <dplyr/HybridHandlerMap.h>

#include <dplyr/Result/ILazySubsets.h>

#include <dplyr/Result/Lead.h>
#include <dplyr/Result/Lag.h>

#include <tools/match.h>
#include <tools/constfold.h>

using namespace Rcpp;
using namespace dplyr;

struct LeadLag {

  LeadLag(SEXP call) : data(R_NilValue), n(1), def(R_NilValue), ok(false) {

    call = r_match_call(get_lead(), call);

    SEXP p = CDR(call);
    SEXP tag = TAG(p);
    if (tag != Rf_install("x")) {
      LOG_VERBOSE;
      return;
    }
    data = CAR(p);

    p = CDR(p);
    tag = TAG(p);
    if (tag == Rf_install("n")) {
      SEXP n_e = CAR(p);
      SEXP n_ = r_constfold(n_e);
      try {
        n = as<int>(n_);
      } catch (...) {
        LOG_VERBOSE;
        return;
      }

      p = CDR(p);
      tag = TAG(p);
    }
    if (tag == Rf_install("default")) {
      SEXP defe = CAR(p);
      def = r_constfold(defe);
      p = CDR(p);
      tag = TAG(p);
    }
    if (tag == Rf_install("order_by")) {
      LOG_VERBOSE;
      return;
    }

    LOG_VERBOSE;
    ok = true;
  }

  static SEXP get_lead() {
    static Function lead = Function("lead", Environment::namespace_env("dplyr"));
    return lead;
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

  if (TYPEOF(data) == SYMSXP && subsets.count(data)) {
    bool is_summary = subsets.is_summary(data);
    int n = args.n;
    data = subsets.get_variable(data);

    if (!Rf_isNull(args.def) && TYPEOF(data) != TYPEOF(args.def)) {
      LOG_VERBOSE;
      return 0;
    }

    switch (TYPEOF(data)) {
    case INTSXP:
      return new Templ<INTSXP> (data, n, args.def, is_summary);
    case REALSXP:
      return new Templ<REALSXP>(data, n, args.def, is_summary);
    case CPLXSXP:
      return new Templ<CPLXSXP>(data, n, args.def, is_summary);
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

void install_offset_handlers(HybridHandlerMap& handlers) {
  handlers[ Rf_install("lead") ] = leadlag_prototype<Lead>;
  handlers[ Rf_install("lag") ] = leadlag_prototype<Lag>;
}
