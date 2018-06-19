#ifndef dplyr_hybrid_sum_h
#define dplyr_hybrid_sum_h

#include <dplyr/hybrid/HybridVectorScalarResult.h>
#include <dplyr/hybrid/Dispatch.h>

namespace dplyr {
namespace hybrid {

namespace internal {

// General case (for INTSXP and LGLSXP)
template <int RTYPE, bool NA_RM, typename Index>
struct SumImpl {
  typedef typename Rcpp::traits::storage_type<RTYPE>::type STORAGE;
  static STORAGE process(typename Rcpp::traits::storage_type<RTYPE>::type* ptr,  const Index& indices) {
    long double res = 0;
    int n = indices.size();
    for (int i = 0; i < n; i++) {
      STORAGE value = ptr[indices[i]];

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

// special case for REALSXP because it treats NA differently
template <bool NA_RM, typename Index>
struct SumImpl<REALSXP, NA_RM, Index> {
  static double process(double* ptr,  const Index& indices) {
    long double res = 0;
    int n = indices.size();
    for (int i = 0; i < n; i++) {
      double value = ptr[indices[i]];

      if (NA_RM && Rcpp::traits::is_na<REALSXP>(value)) {
        continue;
      }

      if (!NA_RM && Rcpp::traits::is_na<REALSXP>(value)) {
        return NA_REAL;
      }

      res += value;
    }

    return (double)res;
  }
};

} // namespace internal


template <int RTYPE, bool NA_RM, typename Data>
class Sum : public HybridVectorScalarResult<RTYPE == LGLSXP ? INTSXP : RTYPE, Data, Sum<RTYPE, NA_RM, Data> > {
public:
  static const int rtype = RTYPE == LGLSXP ? INTSXP : RTYPE;
  typedef typename Rcpp::Vector<RTYPE>::stored_type STORAGE;

  typedef HybridVectorScalarResult<rtype, Data, Sum > Parent ;
  typedef typename Data::slicing_index Index;

  Sum(const Data& data, SEXP vec) : Parent(data), data_ptr(Rcpp::internal::r_vector_start<RTYPE>(vec)) {}

  STORAGE process(const Index& indices) const {
    return internal::SumImpl<RTYPE, NA_RM, Index>::process(data_ptr, indices);
  }

private:
  STORAGE* data_ptr;
} ;

}
}


#endif
