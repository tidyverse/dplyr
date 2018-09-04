#ifndef dplyr_hybrid_min_max_h
#define dplyr_hybrid_min_max_h

#include <dplyr/hybrid/HybridVectorScalarResult.h>
#include <dplyr/hybrid/Column.h>
#include <tools/default_value.h>

namespace dplyr {
namespace hybrid {

namespace internal {

template <int RTYPE, typename SlicedTibble, bool MINIMUM, bool NA_RM>
class MinMax : public HybridVectorScalarResult<REALSXP, SlicedTibble, MinMax<RTYPE, SlicedTibble, MINIMUM, NA_RM> > {
public:
  typedef HybridVectorScalarResult<REALSXP, SlicedTibble, MinMax> Parent ;
  typedef typename Rcpp::Vector<RTYPE>::stored_type STORAGE;

  MinMax(const SlicedTibble& data, Column column_):
    Parent(data),
    column(column_.data),
    is_summary(column_.is_summary)
  {}

  inline double process(const typename SlicedTibble::slicing_index& indices) const {
    if (is_summary) {
      return column[indices.group()];
    }
    const int n = indices.size();
    double res = Inf;

    for (int i = 0; i < n; ++i) {
      STORAGE current = column[indices[i]];

      if (Rcpp::Vector<RTYPE>::is_na(current)) {
        if (NA_RM)
          continue;
        else
          return NA_REAL;
      }
      else {
        double current_res = current;
        if (is_better(current_res, res))
          res = current_res;
      }
    }

    return res;
  }

private:
  Rcpp::Vector<RTYPE> column;
  bool is_summary;

  static const double Inf;

  inline static bool is_better(const double current, const double res) {
    if (MINIMUM)
      return current < res;
    else
      return res < current ;
  }
};

template <int RTYPE, typename SlicedTibble, bool MINIMUM, bool NA_RM>
const double MinMax<RTYPE, SlicedTibble, MINIMUM, NA_RM>::Inf = (MINIMUM ? R_PosInf : R_NegInf);

}

// min( <column> )
template <typename SlicedTibble, typename Operation, bool MINIMUM, bool NARM>
SEXP minmax_narm(const SlicedTibble& data, Column x, const Operation& op) {

  // only handle basic number types, anything else goes through R
  switch (TYPEOF(x.data)) {
  case RAWSXP:
    return op(internal::MinMax<RAWSXP, SlicedTibble, MINIMUM, NARM>(data, x));
  case INTSXP:
    return op(internal::MinMax<INTSXP, SlicedTibble, MINIMUM, NARM>(data, x));
  case REALSXP:
    return op(internal::MinMax<REALSXP, SlicedTibble, MINIMUM, NARM>(data, x));
  default:
    break;
  }

  return R_UnboundValue;
}

template <typename SlicedTibble, typename Operation, bool MINIMUM>
SEXP minmax_(const SlicedTibble& data, Column x, bool narm, const Operation& op) {
  if (narm) {
    return minmax_narm<SlicedTibble, Operation, MINIMUM, true>(data, x, op) ;
  } else {
    return minmax_narm<SlicedTibble, Operation, MINIMUM, false>(data, x, op) ;
  }
}

template <typename SlicedTibble, typename Operation>
SEXP min_(const SlicedTibble& data, Column x, bool narm, const Operation& op) {
  return minmax_<SlicedTibble, Operation, true>(data, x, narm, op) ;
}

template <typename SlicedTibble, typename Operation>
SEXP max_(const SlicedTibble& data, Column x, bool narm, const Operation& op) {
  return minmax_<SlicedTibble, Operation, false>(data, x, narm, op) ;
}

}
}

#endif
