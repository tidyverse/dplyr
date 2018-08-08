#ifndef dplyr_hybrid_row_number_h
#define dplyr_hybrid_row_number_h

#include <dplyr/hybrid/HybridVectorVectorResult.h>
#include <dplyr/hybrid/Column.h>

#include <dplyr/visitors/SliceVisitor.h>
#include <dplyr/visitors/Comparer.h>

#include <dplyr/OrderVisitorImpl.h>

namespace dplyr {
namespace hybrid {

namespace internal {

template <typename Data>
class RowNumber0 : public HybridVectorVectorResult<INTSXP, Data, RowNumber0<Data> > {
public:
  typedef HybridVectorVectorResult<INTSXP, Data, RowNumber0<Data> > Parent;
  typedef typename Data::slicing_index Index;

  RowNumber0(const Data& data) : Parent(data) {}

  void fill(const Index& indices, Rcpp::IntegerVector& out) const {
    int n = indices.size();
    for (int i = 0; i < n; i++) {
      out[indices[i]] = i + 1 ;
    }
  }

};

template <typename Data, int RTYPE, bool ascending>
class RowNumber1 : public HybridVectorVectorResult<INTSXP, Data, RowNumber1<Data, RTYPE, ascending> > {
public:
  typedef HybridVectorVectorResult<INTSXP, Data, RowNumber1 > Parent;
  typedef typename Data::slicing_index Index;
  typedef typename Rcpp::Vector<RTYPE>::stored_type STORAGE;
  typedef visitors::SliceVisitor<Rcpp::Vector<RTYPE>, Index> SliceVisitor;
  typedef visitors::WriteSliceVisitor<Rcpp::IntegerVector, Index> WriteSliceVisitor;
  typedef visitors::Comparer<RTYPE, SliceVisitor, ascending> Comparer;

  RowNumber1(const Data& data, SEXP x) : Parent(data), vec(x) {}

  void fill(const Index& indices, Rcpp::IntegerVector& out) const {
    int n = indices.size();

    SliceVisitor slice(vec, indices);
    WriteSliceVisitor out_slice(out, indices);

    std::vector<int> idx(n);
    for (int i = 0; i < n; i++) idx[i] = i;

    // sort idx by vec in the subset given by indices
    std::sort(idx.begin(), idx.end(), Comparer(slice));

    // deal with NA
    int m = indices.size();
    int j = m - 1;
    for (; j >= 0; j--) {
      if (Rcpp::traits::is_na<RTYPE>(slice[idx[j]])) {
        out_slice[idx[j]] = NA_INTEGER;
      } else {
        break;
      }
    }
    for (; j >= 0; j--) {
      out_slice[idx[j]] = j + 1;
    }
  }

private:
  Rcpp::Vector<RTYPE> vec;
};

}

template <typename Data>
inline internal::RowNumber0<Data> row_number_(const Data& data) {
  return internal::RowNumber0<Data>(data);
}

template <typename Data, typename Operation>
inline SEXP row_number_1(const Data& data, Column column, const Operation& op) {
  SEXP x = column.data;
  switch (TYPEOF(x)) {
  case INTSXP:
    return op(internal::RowNumber1<Data, INTSXP, true>(data, x));
  case REALSXP:
    return op(internal::RowNumber1<Data, REALSXP, true>(data, x));
  default:
    break;
  }
  return R_UnboundValue;
}

}
}

#endif
