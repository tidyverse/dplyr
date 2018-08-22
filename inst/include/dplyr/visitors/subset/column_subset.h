#ifndef DPLY_VISITORS_SUBSET_column_subset_H
#define DPLY_VISITORS_SUBSET_column_subset_H

#include <tools/utils.h>
#include <tools/set_rownames.h>
#include <tools/is_lubridate_unsupported.h>
#include <tools/bad.h>
#include <tools/default_value.h>
#include <tools/SlicingIndex.h>

namespace dplyr {

// TODO: se can probably spare the `index[i] < 0 ?` business when we know for sure
//       e.g. for SlicingIndex implementations
//
//       negative values in index[i] are used with std::vector<int> in join situations

template <int RTYPE, typename Index>
SEXP column_subset_vector_impl(const Rcpp::Vector<RTYPE>& x, const Index& index) {
  typedef typename Rcpp::Vector<RTYPE>::stored_type STORAGE;
  int n = index.size();
  Rcpp::Vector<RTYPE> res = no_init(n);
  for (int i = 0; i < n; i++) {
    res[i] = index[i] < 0 ? default_value<RTYPE>() : (STORAGE)x[index[i]];
  }
  copy_most_attributes(res, x);
  return res;
}

template <>
inline SEXP column_subset_vector_impl<VECSXP, RowwiseSlicingIndex>(const List& x, const RowwiseSlicingIndex& index) {
  return x[index[0]];
}

template <int RTYPE, typename Index>
SEXP column_subset_matrix_impl(const Rcpp::Matrix<RTYPE>& x, const Index& index) {
  int n = index.size();
  int nc = x.ncol();
  Rcpp::Matrix<RTYPE> res = no_init(n, nc);
  for (int i = 0; i < n; i++) {
    if (index[i] >= 0) {
      res.row(i) = x.row(index[i]);
    } else {
      res.row(i) = Vector<RTYPE>(nc, default_value<RTYPE>());
    }
  }
  copy_most_attributes(res, x);
  return res;
}

template <int RTYPE, typename Index>
SEXP column_subset_impl(SEXP x, const Index& index) {
  if (Rf_isMatrix(x)) {
    return column_subset_matrix_impl<RTYPE, Index>(x, index);
  } else {
    return column_subset_vector_impl<RTYPE, Index>(x, index);
  }
}

template <typename Index>
DataFrame dataframe_subset(const List& data, const Index& index, CharacterVector classes);

template <typename Index>
SEXP column_subset(SEXP x, const Index& index) {
  if (Rf_inherits(x, "data.frame")) {
    return dataframe_subset(x, index, Rf_getAttrib(x, R_NamesSymbol));
  }
  if (is_lubridate_unsupported(x)) {
    stop("classes Period and Interval from lubridate are currently not supported.") ;
  }

  switch (TYPEOF(x)) {
  case LGLSXP:
    return column_subset_impl<LGLSXP, Index>(x, index);
  case RAWSXP:
    return column_subset_impl<RAWSXP, Index>(x, index);
  case INTSXP:
    return column_subset_impl<INTSXP, Index>(x, index);
  case STRSXP:
    return column_subset_impl<STRSXP, Index>(x, index);
  case REALSXP:
    return column_subset_impl<REALSXP, Index>(x, index);
  case CPLXSXP:
    return column_subset_impl<CPLXSXP, Index>(x, index);
  case VECSXP:
    return column_subset_impl<VECSXP, Index>(x, index);
  default:
    break;
  }

  stop("type not supported");
  return R_NilValue;
}

template <typename Index>
DataFrame dataframe_subset(const List& data, const Index& index, CharacterVector classes) {
  int nc = data.size();
  List res(nc);

  for (int i = 0; i < nc; i++) {
    res[i] = column_subset(data[i], index);
  }

  copy_most_attributes(res, data);
  set_class(res, classes);
  set_rownames(res, index.size());
  copy_names(res, data);
  return res;
}

}

#endif
