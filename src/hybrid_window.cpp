#include <dplyr/main.h>

#include <dplyr/HybridHandlerMap.h>

#include <dplyr/Result/ILazySubsets.h>
#include <dplyr/Result/Rank.h>

using namespace Rcpp;
using namespace dplyr;

Result* row_number_prototype(SEXP call, const ILazySubsets& subsets, int nargs) {
  if (nargs >  1 || subsets.size() == 0) return 0;

  if (nargs == 0) return new RowNumber_0();

  RObject data(CADR(call));
  if (TYPEOF(data) == LANGSXP && CAR(data) == Rf_install("desc")) {
    data = CADR(data);

    if (TYPEOF(data) == SYMSXP) {
      SymbolString name = SymbolString(Symbol(data));
      if (subsets.count(name)) data = subsets.get_variable(name);
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
    SymbolString name = SymbolString(Symbol(data));
    if (subsets.count(name)) data = subsets.get_variable(name);
    else return 0;
  }
  if (subsets.nrows() != Rf_length(data)) return 0;

  switch (TYPEOF(data)) {
  case INTSXP:
    return new RowNumber<INTSXP, true>(data);
  case REALSXP:
    return new RowNumber<REALSXP, true>(data);
  case STRSXP:
    return new RowNumber<STRSXP, true>(data);
  default:
    break;
  }
  // we don't know how to handle it.
  return 0;
}

Result* ntile_prototype(SEXP call, const ILazySubsets& subsets, int nargs) {
  if (nargs != 2) return 0;

  // handle 2nd arg
  SEXP ntiles = CADDR(call);
  if (TYPEOF(ntiles) != INTSXP && TYPEOF(ntiles) != REALSXP) return 0;
  int number_tiles = as<int>(ntiles);
  if (number_tiles == NA_INTEGER) return 0;

  RObject data(CADR(call));
  if (TYPEOF(data) == LANGSXP && CAR(data) == Rf_install("desc")) {
    data = CADR(data);

    if (TYPEOF(data) == SYMSXP) {
      SymbolString name = SymbolString(Symbol(data));
      if (subsets.count(name)) data = subsets.get_variable(name);
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
    SymbolString name = SymbolString(Symbol(data));
    if (subsets.count(name)) data = subsets.get_variable(name);
    else return 0;
  }
  if (subsets.nrows() != Rf_length(data)) return 0;

  switch (TYPEOF(data)) {
  case INTSXP:
    return new Ntile<INTSXP, true>(data, number_tiles);
  case REALSXP:
    return new Ntile<REALSXP, true>(data, number_tiles);
  case STRSXP:
    return new Ntile<STRSXP, true>(data, number_tiles);
  default:
    break;
  }
  // we don't know how to handle it.
  return 0;
}

template <typename Increment>
Result* rank_impl_prototype(SEXP call, const ILazySubsets& subsets, int nargs) {
  if (nargs != 1) return 0;
  RObject data(CADR(call));

  if (TYPEOF(data) == LANGSXP && CAR(data) == Rf_install("desc")) {
    data = CADR(data);
    if (TYPEOF(data) == SYMSXP) {
      SymbolString name = SymbolString(Symbol(data));
      if (subsets.count(name)) data = subsets.get_variable(name);
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
    SymbolString name = SymbolString(Symbol(data));
    if (subsets.count(name)) data = subsets.get_variable(name);
    else return 0;
  }
  if (subsets.nrows() != Rf_length(data)) return 0;

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

void install_window_handlers(HybridHandlerMap& handlers) {
  handlers[ Rf_install("row_number") ] = row_number_prototype;
  handlers[ Rf_install("ntile") ] = ntile_prototype;
  handlers[ Rf_install("min_rank") ] = rank_impl_prototype<dplyr::internal::min_rank_increment>;
  handlers[ Rf_install("percent_rank") ] = rank_impl_prototype<dplyr::internal::percent_rank_increment>;
  handlers[ Rf_install("dense_rank") ] = rank_impl_prototype<dplyr::internal::dense_rank_increment>;
  handlers[ Rf_install("cume_dist") ] = rank_impl_prototype<dplyr::internal::cume_dist_increment>;
}
