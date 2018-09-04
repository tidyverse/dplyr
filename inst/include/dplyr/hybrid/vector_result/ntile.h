#ifndef dplyr_hybrid_ntile_h
#define dplyr_hybrid_ntile_h


#include <dplyr/hybrid/HybridVectorSummaryRecycleResult.h>
#include <dplyr/hybrid/HybridVectorVectorResult.h>
#include <dplyr/hybrid/Column.h>
#include <dplyr/hybrid/Expression.h>

#include <dplyr/visitors/SliceVisitor.h>
#include <dplyr/visitors/Comparer.h>

namespace dplyr {
namespace hybrid {

namespace internal {

template <typename SlicedTibble>
class Ntile1 : public HybridVectorVectorResult<INTSXP, SlicedTibble, Ntile1<SlicedTibble> > {
public:
  typedef HybridVectorVectorResult<INTSXP, SlicedTibble, Ntile1> Parent;

  Ntile1(const SlicedTibble& data, int ntiles_): Parent(data), ntiles(ntiles_) {}

  void fill(const typename SlicedTibble::slicing_index& indices, Rcpp::IntegerVector& out) const {
    int m = indices.size();
    for (int j = m - 1; j >= 0; j--) {
      out[ indices[j] ] = (int)floor((ntiles * j) / m) + 1;
    }
  }

private:
  int ntiles;
};

template <typename SlicedTibble, int RTYPE>
class Ntile2_summary : public HybridVectorSummaryRecycleResult<INTSXP, SlicedTibble, Ntile2_summary<SlicedTibble, RTYPE> > {
public:
  typedef HybridVectorSummaryRecycleResult<INTSXP, SlicedTibble, Ntile2_summary> Parent;

  Ntile2_summary(const SlicedTibble& data, SEXP x) :
    Parent(data),
    vec(x)
  {}

  inline int value(const typename SlicedTibble::slicing_index& indices) const {
    return Rcpp::Vector<RTYPE>::is_na(vec[indices.group()]) ? NA_INTEGER : 1;
  }

private:
  Rcpp::Vector<RTYPE> vec;
};

template <typename SlicedTibble, int RTYPE, bool ascending>
class Ntile2 : public HybridVectorVectorResult<INTSXP, SlicedTibble, Ntile2<SlicedTibble, RTYPE, ascending> > {
public:
  typedef HybridVectorVectorResult<INTSXP, SlicedTibble, Ntile2> Parent;
  typedef visitors::SliceVisitor<Rcpp::Vector<RTYPE>, typename SlicedTibble::slicing_index> SliceVisitor;
  typedef visitors::WriteSliceVisitor<Rcpp::IntegerVector, typename SlicedTibble::slicing_index> WriteSliceVisitor;
  typedef visitors::Comparer<RTYPE, SliceVisitor, ascending> Comparer;

  Ntile2(const SlicedTibble& data, SEXP x, int ntiles_):
    Parent(data),
    vec(x),
    ntiles(ntiles_)
  {}

  void fill(const typename SlicedTibble::slicing_index& indices, Rcpp::IntegerVector& out) const {
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
        m--;
        out_slice[idx[j]] = NA_INTEGER;
      } else {
        break;
      }
    }
    for (; j >= 0; j--) {
      out_slice[idx[j]] = (int)floor((ntiles * j) / m) + 1;
    }
  }

private:
  Rcpp::Vector<RTYPE> vec;
  int ntiles;
};


template <typename SlicedTibble, typename Operation, int RTYPE>
inline SEXP ntile_2(const SlicedTibble& data, SEXP x, bool is_summary, bool is_desc, int n, const Operation& op) {
  if (is_summary) {
    return op(Ntile2_summary<SlicedTibble, RTYPE>(data, x));
  } else if (is_desc) {
    return op(Ntile2<SlicedTibble, RTYPE, false>(data, x, n));
  } else {
    return op(Ntile2<SlicedTibble, RTYPE, true>(data, x, n));
  }
}

}

template <typename SlicedTibble>
inline internal::Ntile1<SlicedTibble> ntile_1(const SlicedTibble& data, int ntiles) {
  return internal::Ntile1<SlicedTibble>(data, ntiles);
}

template <typename SlicedTibble, typename Operation>
inline SEXP ntile_2(const SlicedTibble& data, Column& column, int n, const Operation& op) {
  SEXP x = column.data;
  switch (TYPEOF(x)) {
  case INTSXP:
    return internal::ntile_2<SlicedTibble, Operation, INTSXP>(data, x, column.is_summary, column.is_desc, n, op);
  case REALSXP:
    return internal::ntile_2<SlicedTibble, Operation, REALSXP>(data, x, column.is_summary, column.is_desc, n, op);
  default:
    break;
  }
  return R_UnboundValue;
}

}
}

#endif
