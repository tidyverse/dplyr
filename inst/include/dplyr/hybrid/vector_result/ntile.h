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

template <typename Data>
class Ntile1 : public HybridVectorVectorResult<INTSXP, Data, Ntile1<Data> > {
public:
  typedef HybridVectorVectorResult<INTSXP, Data, Ntile1> Parent;
  typedef typename Data::slicing_index Index;

  Ntile1(const Data& data, int ntiles_): Parent(data), ntiles(ntiles_) {}

  void fill(const Index& indices, Rcpp::IntegerVector& out) const {
    int m = indices.size();
    for (int j = m - 1; j >= 0; j--) {
      out[ indices[j] ] = (int)floor((ntiles * j) / m) + 1;
    }
  }

private:
  int ntiles;
};

template <typename Data, int RTYPE>
class Ntile2_summary : public HybridVectorSummaryRecycleResult<INTSXP, Data, Ntile2_summary<Data, RTYPE> > {
public:
  typedef HybridVectorSummaryRecycleResult<INTSXP, Data, Ntile2_summary> Parent;
  typedef typename Data::slicing_index Index;
  typedef Rcpp::Vector<RTYPE> Vector;

  Ntile2_summary(const Data& data, SEXP x) :
    Parent(data),
    vec(x)
  {}

  inline int value(const Index& indices) const {
    return Vector::is_na(vec[indices.group()]) ? NA_INTEGER : 1;
  }

private:
  Vector vec;
};

template <typename Data, int RTYPE, bool ascending>
class Ntile2 : public HybridVectorVectorResult<INTSXP, Data, Ntile2<Data, RTYPE, ascending> > {
public:
  typedef HybridVectorVectorResult<INTSXP, Data, Ntile2> Parent;
  typedef typename Data::slicing_index Index;
  typedef visitors::SliceVisitor<Rcpp::Vector<RTYPE>, Index> SliceVisitor;
  typedef visitors::WriteSliceVisitor<Rcpp::IntegerVector, Index> WriteSliceVisitor;
  typedef visitors::Comparer<RTYPE, SliceVisitor, ascending> Comparer;

  Ntile2(const Data& data, SEXP x, int ntiles_):
    Parent(data),
    vec(x),
    ntiles(ntiles_)
  {}

  void fill(const Index& indices, Rcpp::IntegerVector& out) const {
    int n = indices.size();

    SliceVisitor slice(vec, indices);
    WriteSliceVisitor out_slice(out, indices);

    std::vector<int> idx(n);
    std::iota(idx.begin(), idx.end(), 0);

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


template <typename Data, typename Operation, int RTYPE>
inline SEXP ntile_2(const Data& data, SEXP x, bool is_summary, bool is_desc, int n, const Operation& op) {
  if (is_summary) {
    return op(Ntile2_summary<Data, RTYPE>(data, x));
  } else if (is_desc) {
    return op(Ntile2<Data, RTYPE, false>(data, x, n));
  } else {
    return op(Ntile2<Data, RTYPE, true>(data, x, n));
  }
}

}

template <typename Data>
inline internal::Ntile1<Data> ntile_1(const Data& data, int ntiles) {
  return internal::Ntile1<Data>(data, ntiles);
}

template <typename Data, typename Operation>
inline SEXP ntile_2(const Data& data, Column& column, int n, const Operation& op) {
  SEXP x = column.data;
  switch (TYPEOF(x)) {
  case INTSXP:
    return internal::ntile_2<Data, Operation, INTSXP>(data, x, column.is_summary, column.is_desc, n, op);
  case REALSXP:
    return internal::ntile_2<Data, Operation, REALSXP>(data, x, column.is_summary, column.is_desc, n, op);
  default:
    break;
  }
  return R_UnboundValue;
}

// template <typename Data, typename Operation>
// inline SEXP row_number_1(const Data& data, Column column, const Operation& op){
//   SEXP x = column.data;
//   switch(TYPEOF(x)){
//   case INTSXP: return op(internal::RowNumber1<Data, INTSXP>(data, x));
//   case REALSXP: return op(internal::RowNumber1<Data, REALSXP>(data, x));
//   default: break;
//   }
//   return R_UnboundValue;
// }

}
}

#endif
