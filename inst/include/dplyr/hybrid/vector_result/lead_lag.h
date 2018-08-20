#ifndef dplyr_hybrid_lead_lag_h
#define dplyr_hybrid_lead_lag_h

#include <dplyr/hybrid/HybridVectorVectorResult.h>
#include <dplyr/hybrid/HybridVectorSummaryRecycleResult.h>
#include <dplyr/hybrid/Column.h>
#include <tools/default_value.h>

#include <dplyr/visitors/SliceVisitor.h>

namespace dplyr {
namespace hybrid {

namespace internal {

template <typename Data, int RTYPE>
class LeadLagSummary : public HybridVectorSummaryRecycleResult<RTYPE, Data, LeadLagSummary<Data, RTYPE> > {
public:
  typedef HybridVectorSummaryRecycleResult<RTYPE, Data, LeadLagSummary> Parent;
  typedef typename Data::slicing_index Index;
  typedef Rcpp::Vector<RTYPE> Vector;

  LeadLagSummary(const Data& data, SEXP /*x*/, int /* n */) :
    Parent(data)
  {}

  inline typename Vector::stored_type value(const Index& indices) const {
    return default_value<RTYPE>();
  }
};

template <typename Data, int RTYPE>
class Lead : public HybridVectorVectorResult<RTYPE, Data, Lead<Data, RTYPE> > {
public:
  typedef HybridVectorVectorResult<RTYPE, Data, Lead> Parent;
  typedef typename Data::slicing_index Index;
  typedef Rcpp::Vector<RTYPE> Vector;

  typedef visitors::SliceVisitor<Vector, Index> SliceVisitor;
  typedef visitors::WriteSliceVisitor<Vector, Index> WriteSliceVisitor;

  Lead(const Data& data, SEXP x, int n_) :
    Parent(data),
    vec(x),
    n(n_)
  {}

  void fill(const Index& indices, Vector& out) const {
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

template <typename Data, int RTYPE>
class Lag : public HybridVectorVectorResult<RTYPE, Data, Lag<Data, RTYPE> > {
public:
  typedef HybridVectorVectorResult<RTYPE, Data, Lag> Parent;
  typedef typename Data::slicing_index Index;
  typedef Rcpp::Vector<RTYPE> Vector;

  typedef visitors::SliceVisitor<Vector, Index> SliceVisitor;
  typedef visitors::WriteSliceVisitor<Vector, Index> WriteSliceVisitor;

  Lag(const Data& data, SEXP x, int n_) :
    Parent(data),
    vec(x),
    n(n_)
  {}

  void fill(const Index& indices, Vector& out) const {
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


template <typename Data, typename Operation, template <typename, int> class Impl>
inline SEXP lead_lag_dispatch3(const Data& data, SEXP x, int n, const Operation& op) {
  switch (TYPEOF(x)) {
  case LGLSXP:
    return op(Impl<Data, LGLSXP>(data, x, n));
  case RAWSXP:
    return op(Impl<Data, RAWSXP>(data, x, n));
  case INTSXP:
    return op(Impl<Data, INTSXP>(data, x, n));
  case REALSXP:
    return op(Impl<Data, REALSXP>(data, x, n));
  case STRSXP:
    return op(Impl<Data, STRSXP>(data, x, n));
  case CPLXSXP:
    return op(Impl<Data, CPLXSXP>(data, x, n));
  case VECSXP:
    return op(Impl<Data, VECSXP>(data, x, n));
  default:
    break;
  }
  return R_UnboundValue;
}


template <typename Data, typename Operation, template <typename, int> class Impl>
inline SEXP lead_lag(const Data& data, Column column, int n, const Operation& op) {
  SEXP x = column.data;

  if (column.is_summary) {
    return lead_lag_dispatch3<Data, Operation, LeadLagSummary>(data, x, n, op);
  }

  // not sure what to do with desc, just ignoring for now
  return lead_lag_dispatch3<Data, Operation, Impl>(data, x, n, op);
}


}

template <typename Data, typename Operation>
inline SEXP lead_1(const Data& data, Column column, int n, const Operation& op) {
  return internal::lead_lag<Data, Operation, internal::Lead>(data, column, n, op);
}

template <typename Data, typename Operation>
inline SEXP lag_1(const Data& data, Column column, int n, const Operation& op) {
  return internal::lead_lag<Data, Operation, internal::Lag>(data, column, n, op);
}


}
}

#endif
