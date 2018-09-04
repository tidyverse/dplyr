#ifndef dplyr_hybrid_first_last_h
#define dplyr_hybrid_first_last_h

#include <dplyr/hybrid/HybridVectorScalarResult.h>
#include <dplyr/hybrid/Column.h>
#include <tools/default_value.h>

namespace dplyr {
namespace hybrid {

namespace internal {

template <int RTYPE, typename SlicedTibble>
class Nth2 : public HybridVectorScalarResult<RTYPE, SlicedTibble, Nth2<RTYPE, SlicedTibble> > {
public:
  typedef HybridVectorScalarResult<RTYPE, SlicedTibble, Nth2> Parent ;
  typedef typename Rcpp::Vector<RTYPE>::stored_type STORAGE;

  Nth2(const SlicedTibble& data, Column column_, int pos_):
    Parent(data),
    column(column_.data),
    is_summary(column_.is_summary),
    pos(pos_),
    def(default_value<RTYPE>())
  {}

  Nth2(const SlicedTibble& data, Column column_, int pos_, SEXP def_):
    Parent(data),
    column(column_.data),
    is_summary(column_.is_summary),
    pos(pos_),
    def(Rcpp::internal::r_vector_start<RTYPE>(def_)[0])
  {}

  inline STORAGE process(const typename SlicedTibble::slicing_index& indices) const {
    if (is_summary) {
      if (pos == 1 || pos == -1) {
        return column[indices.group()];
      } else {
        return def;
      }
    }
    int n = indices.size();
    if (n == 0) return def ;

    if (pos > 0 && pos <= n) {
      return column[pos - 1];
    } else if (pos < 0 && pos >= -n) {
      return column[n - pos];
    }

    return def;
  }

private:
  Rcpp::Vector<RTYPE> column;
  bool is_summary;
  int pos;
  STORAGE def;
};

template <int RTYPE, typename SlicedTibble>
class First1 : public HybridVectorScalarResult<RTYPE, SlicedTibble, First1<RTYPE, SlicedTibble> > {
public:
  typedef HybridVectorScalarResult<RTYPE, SlicedTibble, First1> Parent ;
  typedef typename Rcpp::Vector<RTYPE>::stored_type STORAGE;

  First1(const SlicedTibble& data, Column column_):
    Parent(data),
    column(column_.data),
    is_summary(column_.is_summary),
    def(default_value<RTYPE>())
  {}

  First1(const SlicedTibble& data, Column column_, STORAGE def_):
    Parent(data),
    column(column_.data),
    is_summary(column_.is_summary),
    def(def_)
  {}

  inline STORAGE process(const typename SlicedTibble::slicing_index& indices) const {
    return is_summary ? (STORAGE)column[indices.group()] : (indices.size() ? (STORAGE)column[0] : def);
  }

private:
  Rcpp::Vector<RTYPE> column;
  STORAGE def;
  bool is_summary;
};

template <int RTYPE, typename SlicedTibble>
class Last1 : public HybridVectorScalarResult<RTYPE, SlicedTibble, Last1<RTYPE, SlicedTibble> > {
public:
  typedef HybridVectorScalarResult<RTYPE, SlicedTibble, Last1> Parent ;
  typedef typename Rcpp::Vector<RTYPE>::stored_type STORAGE;

  Last1(const SlicedTibble& data, Column column_):
    Parent(data),
    column(column_.data),
    is_summary(column_.is_summary),
    def(default_value<RTYPE>())
  {}

  Last1(const SlicedTibble& data, Column column_, STORAGE def_):
    Parent(data),
    column(column_.data),
    is_summary(column_.is_summary),
    def(def_)
  {}

  inline STORAGE process(const typename SlicedTibble::slicing_index& indices) const {
    return is_summary ? (STORAGE)column[indices.group()] : (indices.size() ? (STORAGE)column[indices.size() - 1] : def);
  }

private:
  Rcpp::Vector<RTYPE> column;
  bool is_summary;
  STORAGE def;
};
}

template <typename SlicedTibble, typename Operation, template <int, typename> class Impl>
SEXP firstlast_1(const SlicedTibble& data, Column x, const Operation& op) {
  switch (TYPEOF(x.data)) {
  case LGLSXP:
    return op(Impl<LGLSXP, SlicedTibble>(data, x));
  case RAWSXP:
    return op(Impl<RAWSXP, SlicedTibble>(data, x));
  case INTSXP:
    return op(Impl<INTSXP, SlicedTibble>(data, x));
  case REALSXP:
    return op(Impl<REALSXP, SlicedTibble>(data, x));
  case CPLXSXP:
    return op(Impl<CPLXSXP, SlicedTibble>(data, x));
  case STRSXP:
    return op(Impl<STRSXP, SlicedTibble>(data, x));
  case VECSXP:
    return op(Impl<VECSXP, SlicedTibble>(data, x));
  default:
    break;
  }
  return R_UnboundValue;
}

// first( <column> )
template <typename SlicedTibble, typename Operation>
SEXP first1_(const SlicedTibble& data, Column x, const Operation& op) {
  return firstlast_1<SlicedTibble, Operation, internal::First1>(data, x, op);
}

// last( <column> )
template <typename SlicedTibble, typename Operation>
SEXP last1_(const SlicedTibble& data, Column x, const Operation& op) {
  return firstlast_1<SlicedTibble, Operation, internal::Last1>(data, x, op);
}

template <typename SlicedTibble, typename Operation, template <int, typename> class Impl>
SEXP firstlast_2_default(const SlicedTibble& data, Column x, SEXP def, const Operation& op) {
  if (TYPEOF(x.data) != TYPEOF(def) || Rf_length(def) != 1) return R_UnboundValue;

  switch (TYPEOF(x.data)) {
  case LGLSXP:
    return op(Impl<LGLSXP, SlicedTibble>(data, x, Rcpp::Vector<LGLSXP>(def)[0]));
  case RAWSXP:
    return op(Impl<RAWSXP, SlicedTibble>(data, x, Rcpp::Vector<RAWSXP>(def)[0]));
  case INTSXP:
    return op(Impl<INTSXP, SlicedTibble>(data, x, Rcpp::Vector<INTSXP>(def)[0]));
  case REALSXP:
    return op(Impl<REALSXP, SlicedTibble>(data, x, Rcpp::Vector<REALSXP>(def)[0]));
  case CPLXSXP:
    return op(Impl<CPLXSXP, SlicedTibble>(data, x, Rcpp::Vector<CPLXSXP>(def)[0]));
  case STRSXP:
    return op(Impl<STRSXP, SlicedTibble>(data, x, Rcpp::Vector<STRSXP>(def)[0]));
  case VECSXP:
    return op(Impl<VECSXP, SlicedTibble>(data, x, Rcpp::Vector<VECSXP>(def)[0]));
  default:
    break;
  }

  return R_UnboundValue;
}

// first( <column>, default = <*> )
template <typename SlicedTibble, typename Operation>
SEXP first2_default(const SlicedTibble& data, Column x, SEXP def, const Operation& op) {
  return firstlast_2_default<SlicedTibble, Operation, internal::First1>(data, x, def, op);
}

// last( <column>, default = <*> )
template <typename SlicedTibble, typename Operation>
SEXP last2_default(const SlicedTibble& data, Column x, SEXP def, const Operation& op) {
  return firstlast_2_default<SlicedTibble, Operation, internal::Last1>(data, x, def, op);
}

// nth( <column>, n = <int|double> )
template <typename SlicedTibble, typename Operation>
SEXP nth2_(const SlicedTibble& data, Column x, SEXP n, const Operation& op) {
  int pos = 0 ;

  switch (TYPEOF(n)) {
  case INTSXP:
    pos = INTEGER(n)[0];
    break;
  case REALSXP:
    pos = Rcpp::internal::r_coerce<REALSXP, INTSXP>(REAL(n)[0]);
    break;
  default:
    return R_UnboundValue;
  }

  switch (TYPEOF(x.data)) {
  case LGLSXP:
    return op(internal::Nth2<LGLSXP, SlicedTibble>(data, x, pos));
  case RAWSXP:
    return op(internal::Nth2<RAWSXP, SlicedTibble>(data, x, pos));
  case INTSXP:
    return op(internal::Nth2<INTSXP, SlicedTibble>(data, x, pos));
  case REALSXP:
    return op(internal::Nth2<REALSXP, SlicedTibble>(data, x, pos));
  case CPLXSXP:
    return op(internal::Nth2<CPLXSXP, SlicedTibble>(data, x, pos));
  case STRSXP:
    return op(internal::Nth2<STRSXP, SlicedTibble>(data, x, pos));
  case VECSXP:
    return op(internal::Nth2<VECSXP, SlicedTibble>(data, x, pos));
  default:
    break;
  }

  return R_UnboundValue;
}

// nth( <column>, n = <int|double> )
template <typename SlicedTibble, typename Operation>
SEXP nth3_default(const SlicedTibble& data, Column x, SEXP n, SEXP def, const Operation& op) {
  if (TYPEOF(x.data) != TYPEOF(def) || Rf_length(def) != 1) return R_UnboundValue;

  int pos = 0 ;
  switch (TYPEOF(n)) {
  case INTSXP:
    pos = INTEGER(n)[0];
    break;
  case REALSXP:
    pos = Rcpp::internal::r_coerce<REALSXP, INTSXP>(REAL(n)[0]);
    break;
  default:
    return R_UnboundValue;
  }

  switch (TYPEOF(x.data)) {
  case LGLSXP:
    return op(internal::Nth2<LGLSXP, SlicedTibble>(data, x, pos, def));
  case RAWSXP:
    return op(internal::Nth2<RAWSXP, SlicedTibble>(data, x, pos, def));
  case INTSXP:
    return op(internal::Nth2<INTSXP, SlicedTibble>(data, x, pos, def));
  case REALSXP:
    return op(internal::Nth2<REALSXP, SlicedTibble>(data, x, pos, def));
  case CPLXSXP:
    return op(internal::Nth2<CPLXSXP, SlicedTibble>(data, x, pos, def));
  case STRSXP:
    return op(internal::Nth2<STRSXP, SlicedTibble>(data, x, pos, def));
  case VECSXP:
    return op(internal::Nth2<VECSXP, SlicedTibble>(data, x, pos, def));
  default:
    break;
  }

  return R_UnboundValue;
}


}
}

#endif
