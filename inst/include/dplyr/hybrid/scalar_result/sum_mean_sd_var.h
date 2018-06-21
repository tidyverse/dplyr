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
  static STORAGE process(typename Rcpp::traits::storage_type<RTYPE>::type* ptr,  const Index& indices, bool is_summary) {
    // already summarised, e.g. when summarise( x = ..., y = sum(x))
    if (is_summary) return ptr[indices.group()];

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
  static double process(double* ptr, const Index& indices, bool is_summary) {
    // already summarised, e.g. when summarise( x = ..., y = sum(x))
    if (is_summary) return ptr[indices.group()];

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

// ------- mean

template <int RTYPE, bool NA_RM, typename Index>
struct MeanImpl {
  static double process(typename Rcpp::traits::storage_type<RTYPE>::type* ptr,  const Index& indices, bool is_summary) {
    typedef typename Rcpp::traits::storage_type<RTYPE>::type STORAGE;

    // already summarised, e.g. when summarise( x = ..., y = mean(x))
    // we need r coercion as opposed to a simple cast to double because of NA
    if (is_summary) {
      return Rcpp::internal::r_coerce<RTYPE,REALSXP>(ptr[indices.group()]);
    }

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
      // INTSXP, LGLSXP: no shortcut, need to test
      if (NA_RM || RTYPE == INTSXP || RTYPE == LGLSXP) {
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

// ------------- var

inline double square(double x) {
  return x * x;
}

template <int RTYPE, bool NA_RM, typename Index>
struct VarImpl {
  typedef typename Rcpp::Vector<RTYPE>::stored_type STORAGE;

  static double process(typename Rcpp::traits::storage_type<RTYPE>::type* data_ptr,  const Index& indices, bool is_summary) {
    // already summarised, e.g. when summarise( x = ..., y = var(x)), so x is of length 1 -> NA
    if (is_summary) {
      return NA_REAL;
    }

    int n = indices.size();
    if (n <= 1) return NA_REAL;
    double m = MeanImpl<RTYPE, NA_RM, Index>::process(data_ptr, indices, is_summary);

    if (!R_FINITE(m)) return m;

    double sum = 0.0;
    int count = 0;
    for (int i = 0; i < n; i++) {
      STORAGE current = data_ptr[indices[i]];
      if (NA_RM && Rcpp::Vector<RTYPE>::is_na(current)) continue;
      sum += internal::square(current - m);
      count++;
    }
    if (count <= 1) return NA_REAL;
    return sum / (count - 1);
  }

};

template <int RTYPE, bool NA_RM, typename Index>
struct SdImpl {
  static double process(typename Rcpp::traits::storage_type<RTYPE>::type* data_ptr,  const Index& indices, bool is_summary) {
    return sqrt(VarImpl<RTYPE,NA_RM,Index>::process(data_ptr, indices, is_summary));
  }
};


} // namespace internal

template <typename Data>
SimpleDispatch<Data, internal::SumImpl> sum_( const Data& data, Column variable, bool narm){
  return SimpleDispatch<Data, internal::SumImpl>(data, variable, narm);
}

template <typename Data>
SimpleDispatch<Data, internal::MeanImpl> mean_( const Data& data, Column variable, bool narm){
  return SimpleDispatch<Data, internal::MeanImpl>(data, variable, narm);
}

template <typename Data>
SimpleDispatch<Data, internal::VarImpl> var_( const Data& data, Column variable, bool narm){
  return SimpleDispatch<Data, internal::VarImpl>(data, variable, narm);
}

template <typename Data>
SimpleDispatch<Data, internal::SdImpl> sd_( const Data& data, Column variable, bool narm){
  return SimpleDispatch<Data, internal::SdImpl>(data, variable, narm);
}

}
}


#endif
