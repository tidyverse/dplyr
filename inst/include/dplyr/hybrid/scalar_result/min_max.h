#ifndef dplyr_hybrid_min_max_h
#define dplyr_hybrid_min_max_h

#include <dplyr/hybrid/HybridVectorScalarResult.h>
#include <dplyr/hybrid/Column.h>
#include <dplyr/default_value.h>

namespace dplyr {
namespace hybrid {

namespace internal {

template <int RTYPE, typename Data, bool MINIMUM, bool NA_RM>
class MinMax : public HybridVectorScalarResult<REALSXP, Data, MinMax<RTYPE, Data, MINIMUM, NA_RM> > {
public:
  typedef HybridVectorScalarResult<REALSXP, Data, MinMax> Parent ;
  typedef typename Data::slicing_index Index;
  typedef typename Rcpp::Vector<RTYPE>::stored_type STORAGE;

  MinMax(const Data& data, Column column_):
    Parent(data),
    column(column_.data),
    is_summary(column_.is_summary)
  {}

  inline double process(const Index& indices) const {
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
          return Rcpp::Vector<RTYPE>::get_na();
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

template <int RTYPE, typename Data, bool MINIMUM, bool NA_RM>
const double MinMax<RTYPE, Data, MINIMUM, NA_RM>::Inf = (MINIMUM ? R_PosInf : R_NegInf);

}

// min( <column> )
template <typename Data, typename Operation, bool MINIMUM, bool NARM>
SEXP minmax_narm(const Data& data, Column x, const Operation& op) {

  // only handle basic number types, anything else goes through R
  switch (TYPEOF(x.data)) {
  case RAWSXP:
    return op(internal::MinMax<RAWSXP, Data, MINIMUM, NARM>(data, x));
  case INTSXP:
    return op(internal::MinMax<INTSXP, Data, MINIMUM, NARM>(data, x));
  case REALSXP:
    return op(internal::MinMax<REALSXP, Data, MINIMUM, NARM>(data, x));
  default:
    break;
  }

  return R_UnboundValue;
}

template <typename Data, typename Operation, bool MINIMUM>
SEXP minmax_(const Data& data, Column x, bool narm, const Operation& op) {
  if (narm) {
    return minmax_narm<Data, Operation, MINIMUM, true>(data, x, op) ;
  } else {
    return minmax_narm<Data, Operation, MINIMUM, false>(data, x, op) ;
  }
}

template <typename Data, typename Operation>
SEXP min_(const Data& data, Column x, bool narm, const Operation& op) {
  return minmax_<Data, Operation, true>(data, x, narm, op) ;
}

template <typename Data, typename Operation>
SEXP max_(const Data& data, Column x, bool narm, const Operation& op) {
  return minmax_<Data, Operation, false>(data, x, narm, op) ;
}

}
}

#endif
