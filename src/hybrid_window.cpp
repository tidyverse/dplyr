#include "pch.h"
#include <dplyr/main.h>

#include <dplyr/HybridHandlerMap.h>

#include <dplyr/Result/ILazySubsets.h>
#include <dplyr/Result/Rank.h>

using namespace Rcpp;
using namespace dplyr;

namespace dplyr {

template <bool ascending>
Result* row_number_asc(const RObject& data) {
  switch (TYPEOF(data)) {
  case INTSXP:
    return new RowNumber<INTSXP, ascending>(data);
  case REALSXP:
    return new RowNumber<REALSXP, ascending>(data);
  case STRSXP:
    return new RowNumber<STRSXP, ascending>(data);
  default:
    return 0;
  }
}

Result* row_number(const RObject& data, const bool ascending) {
  if (ascending) {
    return row_number_asc<true>(data);
  }
  else {
    return row_number_asc<false>(data);
  }
}

Result* row_number_prototype(SEXP call, const ILazySubsets& subsets, int nargs) {
  if (nargs > 1 || subsets.size() == 0) return 0;

  if (nargs == 0) return new RowNumber_0();

  RObject data(CADR(call));
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

  return row_number(data, ascending);
}

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
  if (nargs != 2) return 0;

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
  handlers[ Rf_install("row_number") ] = row_number_prototype;
  handlers[ Rf_install("ntile") ] = ntile_prototype;
  handlers[ Rf_install("min_rank") ] = rank_impl_prototype<dplyr::internal::min_rank_increment>;
  handlers[ Rf_install("percent_rank") ] = rank_impl_prototype<dplyr::internal::percent_rank_increment>;
  handlers[ Rf_install("dense_rank") ] = rank_impl_prototype<dplyr::internal::dense_rank_increment>;
  handlers[ Rf_install("cume_dist") ] = rank_impl_prototype<dplyr::internal::cume_dist_increment>;
}
