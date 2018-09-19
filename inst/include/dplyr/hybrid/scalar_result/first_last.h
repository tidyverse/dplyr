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
    pos(pos_),
    def(default_value<RTYPE>())
  {}

  Nth2(const SlicedTibble& data, Column column_, int pos_, SEXP def_):
    Parent(data),
    column(column_.data),
    pos(pos_),
    def(Rcpp::internal::r_vector_start<RTYPE>(def_)[0])
  {}

  inline STORAGE process(const typename SlicedTibble::slicing_index& indices) const {
    int n = indices.size();
    if (n == 0) return def ;

    if (pos > 0 && pos <= n) {
      return column[pos - 1];
    } else if (pos < 0 && pos >= -n) {
      return column[n + pos];
    }

    return def;
  }

private:
  Rcpp::Vector<RTYPE> column;
  int pos;
  STORAGE def;
};

}

// nth( <column>, n = <int|double> )
template <typename SlicedTibble, typename Operation>
SEXP nth2_(const SlicedTibble& data, Column x, int pos, const Operation& op) {
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

// first( <column> )
template <typename SlicedTibble, typename Operation>
SEXP first1_(const SlicedTibble& data, Column x, const Operation& op) {
  return nth2_(data, x, 1, op);
}

// first( <column> )
template <typename SlicedTibble, typename Operation>
SEXP last1_(const SlicedTibble& data, Column x, const Operation& op) {
  return nth2_(data, x, -1, op);
}


// nth( <column>, n = <int|double> )
template <typename SlicedTibble, typename Operation>
SEXP nth3_default(const SlicedTibble& data, Column x, int pos, SEXP def, const Operation& op) {
  if (TYPEOF(x.data) != TYPEOF(def) || Rf_length(def) != 1) return R_UnboundValue;

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

// first( <column>, default = <scalar> )
template <typename SlicedTibble, typename Operation>
SEXP first2_(const SlicedTibble& data, Column x, SEXP def, const Operation& op) {
  return nth3_default(data, x, 1, def, op);
}
// last( <column>, default = <scalar> )
template <typename SlicedTibble, typename Operation>
SEXP last2_(const SlicedTibble& data, Column x, SEXP def, const Operation& op) {
  return nth3_default(data, x, -1, def, op);
}

}
}

#endif
