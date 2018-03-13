#ifndef dplyr_Result_Sum_H
#define dplyr_Result_Sum_H

#include <dplyr/Result/Processor.h>

namespace dplyr {

namespace internal {

template <int RTYPE, bool NA_RM, typename Index>
struct Sum {
  typedef typename Rcpp::traits::storage_type<RTYPE>::type STORAGE;
  static STORAGE process(typename Rcpp::traits::storage_type<RTYPE>::type* ptr,  const Index& indices) {
    long double res = 0;
    int n = indices.size();
    for (int i = 0; i < n; i++) {
      double value = ptr[indices[i]];

      // !NA_RM: we don't test for NA here because += NA will give NA
      // this is faster in the most common case where there are no NA
      // if there are NA, we could return quicker as in the version for
      // INTSXP, but we would penalize the most common case
      if (NA_RM && Rcpp::traits::is_na<RTYPE>(value)) {
        continue;
      }

      res += value;
    }

    return (STORAGE)res;
  }
};

// Special case for INTSXP:
template <bool NA_RM, typename Index>
struct Sum<INTSXP, NA_RM, Index> {
  enum { RTYPE = INTSXP };
  typedef typename Rcpp::traits::storage_type<RTYPE>::type STORAGE;
  static STORAGE process(typename Rcpp::traits::storage_type<RTYPE>::type* ptr,  const Index& indices) {
    long double res = 0;
    int n = indices.size();
    for (int i = 0; i < n; i++) {
      double value = ptr[indices[i]];

      if (Rcpp::traits::is_na<RTYPE>(value)) {
        if (NA_RM) {
          continue;
        }

        return Rcpp::traits::get_na<RTYPE>();
      }

      res += value;
    }

    if (res > INT_MAX || res <= INT_MIN) {
      warning("integer overflow - use sum(as.numeric(.))");
      return Rcpp::traits::get_na<RTYPE>();
    }

    return (STORAGE)res;
  }
};

} // namespace internal

template <int RTYPE, bool NA_RM>
class Sum : public Processor< RTYPE, Sum<RTYPE, NA_RM> > {
public:
  typedef Processor< RTYPE, Sum<RTYPE, NA_RM> > Base;
  typedef typename Rcpp::traits::storage_type<RTYPE>::type STORAGE;

  Sum(SEXP x) :
    Base(x),
    data_ptr(Rcpp::internal::r_vector_start<RTYPE>(x))
  {}
  ~Sum() {}

  inline STORAGE process_chunk(const SlicingIndex& indices) {
    return internal::Sum<RTYPE, NA_RM, SlicingIndex>::process(data_ptr, indices);
  }

  STORAGE* data_ptr;
};

}

#endif
