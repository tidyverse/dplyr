#ifndef dplyr_hybrid_lead_lag_h
#define dplyr_hybrid_lead_lag_h

#include <dplyr/hybrid/HybridVectorVectorResult.h>
#include <dplyr/hybrid/HybridVectorSummaryRecycleResult.h>
#include <dplyr/hybrid/vector_result/echo.h>

#include <dplyr/hybrid/Column.h>
#include <tools/default_value.h>
#include <dplyr/visitors/SliceVisitor.h>

namespace dplyr {
namespace hybrid {

namespace internal {

template <typename SlicedTibble, int RTYPE>
class Lead : public HybridVectorVectorResult<RTYPE, SlicedTibble, Lead<SlicedTibble, RTYPE> > {
public:
  typedef HybridVectorVectorResult<RTYPE, SlicedTibble, Lead> Parent;

  typedef Rcpp::Vector<RTYPE> Vector;
  typedef visitors::SliceVisitor<Vector, typename SlicedTibble::slicing_index> SliceVisitor;
  typedef visitors::WriteSliceVisitor<Vector, typename SlicedTibble::slicing_index> WriteSliceVisitor;

  Lead(const SlicedTibble& data, SEXP x, int n_) :
    Parent(data),
    vec(x),
    n(n_)
  {}

  void fill(const typename SlicedTibble::slicing_index& indices, Vector& out) const {
    int chunk_size = indices.size();
    SliceVisitor vec_slice(vec, indices);
    WriteSliceVisitor out_slice(out, indices);
    int i = 0;
    for (; i < chunk_size - n; i++) {
      out_slice[i] = vec_slice[i + n];
    }
    for (; i < chunk_size; i++) {
      out_slice[i] = default_value<RTYPE>();
    }
  }

private:
  Vector vec;
  int n;
};

template <typename SlicedTibble, int RTYPE>
class Lag : public HybridVectorVectorResult<RTYPE, SlicedTibble, Lag<SlicedTibble, RTYPE> > {
public:
  typedef HybridVectorVectorResult<RTYPE, SlicedTibble, Lag> Parent;
  typedef Rcpp::Vector<RTYPE> Vector;

  typedef visitors::SliceVisitor<Vector, typename SlicedTibble::slicing_index> SliceVisitor;
  typedef visitors::WriteSliceVisitor<Vector, typename SlicedTibble::slicing_index> WriteSliceVisitor;

  Lag(const SlicedTibble& data, SEXP x, int n_) :
    Parent(data),
    vec(x),
    n(n_)
  {}

  void fill(const typename SlicedTibble::slicing_index& indices, Vector& out) const {
    int chunk_size = indices.size();
    SliceVisitor vec_slice(vec, indices);
    WriteSliceVisitor out_slice(out, indices);
    int n_def = std::min(chunk_size, n);

    int i = 0;
    for (; i < n_def; ++i) {
      out_slice[i] = default_value<RTYPE>();
    }
    for (; i < chunk_size; ++i) {
      out_slice[i] = vec_slice[i - n];
    }
  }

private:
  Vector vec;
  int n;
};


template <typename SlicedTibble, typename Operation, template <typename, int> class Impl>
inline SEXP lead_lag_dispatch3(const SlicedTibble& data, SEXP x, int n, const Operation& op) {
  switch (TYPEOF(x)) {
  case LGLSXP:
    return op(Impl<SlicedTibble, LGLSXP>(data, x, n));
  case RAWSXP:
    return op(Impl<SlicedTibble, RAWSXP>(data, x, n));
  case INTSXP:
    return op(Impl<SlicedTibble, INTSXP>(data, x, n));
  case REALSXP:
    return op(Impl<SlicedTibble, REALSXP>(data, x, n));
  case STRSXP:
    return op(Impl<SlicedTibble, STRSXP>(data, x, n));
  case CPLXSXP:
    return op(Impl<SlicedTibble, CPLXSXP>(data, x, n));
  case VECSXP:
    return op(Impl<SlicedTibble, VECSXP>(data, x, n));
  default:
    break;
  }
  return R_UnboundValue;
}


template <typename SlicedTibble, typename Operation, template <typename, int> class Impl>
inline SEXP lead_lag(const SlicedTibble& data, Column column, int n, const Operation& op) {
  if (n == 0) {
    return echo(column.data, op);
  }
  return lead_lag_dispatch3<SlicedTibble, Operation, Impl>(data, column.data, n, op);
}

}

template <typename SlicedTibble, typename Operation>
inline SEXP lead_1(const SlicedTibble& data, Column column, int n, const Operation& op) {
  return internal::lead_lag<SlicedTibble, Operation, internal::Lead>(data, column, n, op);
}

template <typename SlicedTibble, typename Operation>
inline SEXP lag_1(const SlicedTibble& data, Column column, int n, const Operation& op) {
  return internal::lead_lag<SlicedTibble, Operation, internal::Lag>(data, column, n, op);
}


}
}

#endif
