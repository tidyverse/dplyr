#include "pch.h"
#include <dplyr/main.h>

#include <dplyr/HybridHandlerMap.h>

#include <dplyr/Result/ILazySubsets.h>
#include <dplyr/Result/Rank.h>

using namespace Rcpp;
using namespace dplyr;

namespace dplyr {

template <bool ascending>
Result* ntile_asc(const RObject& data, const int number_tiles) {
  switch (TYPEOF(data)) {
  case INTSXP:
    return new Ntile<INTSXP, ascending>(data, number_tiles);
  case REALSXP:
    return new Ntile<REALSXP, ascending>(data, number_tiles);
  case STRSXP:
    return new Ntile<STRSXP, ascending>(data, number_tiles);
  default:
    return 0;
  }
}

Result* ntile(const RObject& data, const int number_tiles, const bool ascending) {
  if (ascending) {
    return ntile_asc<true>(data, number_tiles);
  }
  else {
    return ntile_asc<false>(data, number_tiles);
  }
}

Result* ntile_prototype(SEXP call, const ILazySubsets& subsets, int nargs) {

  if (nargs == 1) {
    // only one arg. We only accept if it is named "n"
    SEXP cdr = CDR(call);
    if (TAG(cdr) != Rf_install("n")) return 0;

    SEXP ntiles = maybe_rhs(CADR(call));
    if (TYPEOF(ntiles) != INTSXP && TYPEOF(ntiles) != REALSXP) return 0;
    int number_tiles = as<int>(ntiles);
    if (number_tiles == NA_INTEGER) return 0;

    return new Ntile_1(number_tiles);
  }

  if (nargs == 2) {
    // handle 2nd arg
    SEXP ntiles = maybe_rhs(CADDR(call));
    if (TYPEOF(ntiles) != INTSXP && TYPEOF(ntiles) != REALSXP) return 0;
    int number_tiles = as<int>(ntiles);
    if (number_tiles == NA_INTEGER) return 0;

    RObject data(maybe_rhs(CADR(call)));
    bool ascending = true;
    if (TYPEOF(data) == LANGSXP && CAR(data) == Rf_install("desc")) {
      data = CADR(data);
      ascending = false;
    }

    if (TYPEOF(data) == SYMSXP) {
      SymbolString name = SymbolString(Symbol(data));
      if (subsets.has_non_summary_variable(name)) data = subsets.get_variable(name);
      else return 0;
    }

    if (subsets.nrows() != Rf_length(data)) return 0;

    return ntile(data, number_tiles, ascending);
  }

  return 0;
}

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

  handlers[Rf_install("ntile")] = HybridHandler(ntile_prototype, HybridHandler::DPLYR, ns_dplyr["ntile"]);
  handlers[Rf_install("min_rank")] = HybridHandler(rank_impl_prototype<dplyr::internal::min_rank_increment>, HybridHandler::DPLYR, ns_dplyr["min_rank"]);
  handlers[Rf_install("percent_rank")] = HybridHandler(rank_impl_prototype<dplyr::internal::percent_rank_increment>, HybridHandler::DPLYR, ns_dplyr["percent_rank"]);
  handlers[Rf_install("dense_rank")] = HybridHandler(rank_impl_prototype<dplyr::internal::dense_rank_increment>, HybridHandler::DPLYR, ns_dplyr["dense_rank"]);
  handlers[Rf_install("cume_dist")] = HybridHandler(rank_impl_prototype<dplyr::internal::cume_dist_increment>, HybridHandler::DPLYR, ns_dplyr["cume_dist"]);
}

namespace dplyr {

Ntile_1::Ntile_1(int ntiles_) : ntiles(ntiles_) {}

SEXP Ntile_1::process(const GroupedDataFrame& gdf) {
  int n  = gdf.nrows();
  if (n == 0) return IntegerVector(0);

  int ng = gdf.ngroups();

  GroupedDataFrame::group_iterator git = gdf.group_begin();
  IntegerVector out = no_init(n);
  for (int i = 0; i < ng; i++, ++git) {
    const SlicingIndex& index = *git;
    int m = index.size();

    for (int j = m - 1; j >= 0; j--) {
      out[ index[j] ] = (int)floor((ntiles * j) / m) + 1;
    }
  }
  return out;
}

SEXP Ntile_1::process(const RowwiseDataFrame& gdf) {
  return IntegerVector(gdf.nrows(), 1);
}

SEXP Ntile_1::process(const SlicingIndex& index) {
  int nrows = index.size();
  if (nrows == 0) return IntegerVector(0);

  IntegerVector out = no_init(nrows);
  for (int i = nrows - 1; i >= 0; i--) {
    out[ i ] = (int)floor(ntiles * i / nrows) + 1;
  }
  return out;
}

}
