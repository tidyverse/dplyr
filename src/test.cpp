#include "pch.h"

#include <tools/comparisons.h>

#include <dplyr/main.h>

// [[Rcpp::export(rng = false)]]
Rcpp::LogicalVector test_comparisons() {
  typedef dplyr::comparisons<REALSXP> comp;
  return Rcpp::LogicalVector::create(
           comp::is_less(1.0, 2.0),
           !comp::is_less(2.0, 1.0),
           comp::is_less(NA_REAL, R_NaN),
           !comp::is_less(R_NaN, NA_REAL),
           !comp::is_less(NA_REAL, 1.0),
           !comp::is_less(R_NaN, 1.0),
           comp::is_less(1.0, NA_REAL),
           comp::is_less(1.0, R_NaN)
         );
}

// [[Rcpp::export(rng = false)]]
Rcpp::LogicalVector test_length_wrap() {
  R_xlen_t small = R_LEN_T_MAX / 2;

  Rcpp::RObject wrap_small(Rcpp::wrap(small));

#ifdef LONG_VECTOR_SUPPORT
  R_xlen_t large = (R_xlen_t)(R_LEN_T_MAX * 2.0);
  R_xlen_t missing = NA_INTEGER;

  Rcpp::RObject wrap_large(Rcpp::wrap(large));
  Rcpp::RObject wrap_missing(Rcpp::wrap(missing));

  return
    Rcpp::LogicalVector::create(
      Rcpp::as<double>(wrap_small) == (double)small,
      Rcpp::as<double>(wrap_large) == (double)large,
      Rcpp::as<double>(wrap_missing) == (double)missing
    );
#else
  return
    Rcpp::LogicalVector::create(
      Rcpp::as<double>(wrap_small) == (double)small
    );
#endif
}
