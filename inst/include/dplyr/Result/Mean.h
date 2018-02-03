#ifndef dplyr_Result_Mean_H
#define dplyr_Result_Mean_H

#include <dplyr/Result/Processor.h>

namespace dplyr {

namespace internal {

template <int RTYPE, bool NA_RM, typename Index>
struct Mean_internal {
  static double process(typename Rcpp::traits::storage_type<RTYPE>::type* ptr,  const Index& indices) {
    typedef typename Rcpp::traits::storage_type<RTYPE>::type STORAGE;
    long double res = 0.0;
    int n = indices.size();
    int m = n;
    for (int i = 0; i < n; i++) {
      STORAGE value = ptr[ indices[i] ];

      // REALSXP and !NA_RM: we don't test for NA here because += NA will give NA
      // this is faster in the most common case where there are no NA
      // if there are NA, we could return quicker as in the version for
      // INTSXP, but we would penalize the most common case
      //
      // INTSXP: no shortcut, need to test
      if (NA_RM || RTYPE == INTSXP) {
        if (Rcpp::traits::is_na<RTYPE>(value)) {
          if (!NA_RM) {
            return NA_REAL;
          }

          --m;
          continue;
        }
      }

      res += value;
    }
    if (m == 0) return R_NaN;
    res /= m;

    // Correcting accuracy of result, see base R implementation
    if (R_FINITE(res)) {
      long double t = 0.0;
      for (int i = 0; i < n; i++) {
        STORAGE value = ptr[indices[i]];
        if (!NA_RM || ! Rcpp::traits::is_na<RTYPE>(value)) {
          t += value - res;
        }
      }
      res += t / m;
    }

    return (double)res;
  }
};

} // namespace internal

template <int RTYPE, bool NA_RM>
class Mean : public Processor< REALSXP, Mean<RTYPE, NA_RM> > {
public:
  typedef Processor< REALSXP, Mean<RTYPE, NA_RM> > Base;
  typedef typename Rcpp::traits::storage_type<RTYPE>::type STORAGE;

  Mean(SEXP x) :
    Base(x),
    data_ptr(Rcpp::internal::r_vector_start<RTYPE>(x))
  {}
  ~Mean() {}

  inline double process_chunk(const SlicingIndex& indices) {
    return internal::Mean_internal<RTYPE, NA_RM, SlicingIndex>::process(data_ptr, indices);
  }

private:
  STORAGE* data_ptr;
};

}

#endif
