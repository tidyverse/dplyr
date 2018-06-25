#include "pch.h"
#include <dplyr/main.h>

#include <dplyr/HybridHandlerMap.h>

#include <dplyr/Result/ILazySubsets.h>
#include <dplyr/Result/Rank.h>

using namespace Rcpp;
using namespace dplyr;

namespace dplyr {

template <typename Increment, bool ascending>
Result* rank_asc(const RObject& data) {
  switch (TYPEOF(data)) {
  case INTSXP:
    return new Rank_Impl<INTSXP, Increment, ascending>(data);
  case REALSXP:
    return new Rank_Impl<REALSXP, Increment, ascending>(data);
  case STRSXP:
    return new Rank_Impl<STRSXP, Increment, ascending>(data);
  default:
    return 0;
  }
}

template <typename Increment>
Result* rank(const RObject& data, bool ascending) {
  if (ascending) {
    return rank_asc<Increment, true>(data);
  }
  else {
    return rank_asc<Increment, false>(data);
  }
}

template <typename Increment>
Result* rank_impl_prototype(SEXP call, const ILazySubsets& subsets, int nargs) {
  if (nargs != 1) return 0;

  RObject data(maybe_rhs(CADR(call)));
  bool ascending = true;

  if (TYPEOF(data) == LANGSXP && CAR(data) == Rf_install("desc")) {
    data = maybe_rhs(CADR(data));
    ascending = false;
  }

  if (TYPEOF(data) == SYMSXP) {
    SymbolString name = SymbolString(Symbol(data));
    if (subsets.has_non_summary_variable(name)) data = subsets.get_variable(name);
    else return 0;
  }

  if (subsets.nrows() != Rf_length(data)) return 0;

  return rank<Increment>(data, ascending);
}

}

void install_window_handlers(HybridHandlerMap& handlers) {
  Environment ns_dplyr = Environment::namespace_env("dplyr");

  handlers[Rf_install("min_rank")] = HybridHandler(rank_impl_prototype<dplyr::internal::min_rank_increment>, HybridHandler::DPLYR, ns_dplyr["min_rank"]);
  handlers[Rf_install("percent_rank")] = HybridHandler(rank_impl_prototype<dplyr::internal::percent_rank_increment>, HybridHandler::DPLYR, ns_dplyr["percent_rank"]);
  handlers[Rf_install("dense_rank")] = HybridHandler(rank_impl_prototype<dplyr::internal::dense_rank_increment>, HybridHandler::DPLYR, ns_dplyr["dense_rank"]);
  handlers[Rf_install("cume_dist")] = HybridHandler(rank_impl_prototype<dplyr::internal::cume_dist_increment>, HybridHandler::DPLYR, ns_dplyr["cume_dist"]);
}
