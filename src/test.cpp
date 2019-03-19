#include "pch.h"

#include <tools/comparisons.h>

#include <dplyr/main.h>

#include <dplyr/visitors/join/join_match.h>

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
Rcpp::List test_matches() {
  typedef dplyr::join_match<INTSXP, INTSXP, true> int_int_na;
  typedef dplyr::join_match<REALSXP, REALSXP, true> real_real_na;
  typedef dplyr::join_match<INTSXP, REALSXP, true> int_real_na;
  typedef dplyr::join_match<REALSXP, INTSXP, true> real_int_na;
  typedef dplyr::join_match<INTSXP, INTSXP, false> int_int;
  typedef dplyr::join_match<REALSXP, REALSXP, false> real_real;
  typedef dplyr::join_match<INTSXP, REALSXP, false> int_real;
  typedef dplyr::join_match<REALSXP, INTSXP, false> real_int;
  return
    Rcpp::List::create(
      Rcpp::LogicalVector::create(
        int_int_na::is_match(1, 1),
        !int_int_na::is_match(1, 2),
        !int_int_na::is_match(1, NA_INTEGER),
        !int_int_na::is_match(NA_INTEGER, 1),
        int_int_na::is_match(NA_INTEGER, NA_INTEGER),
        int_int::is_match(1, 1),
        !int_int::is_match(1, 2),
        !int_int::is_match(1, NA_INTEGER),
        !int_int::is_match(NA_INTEGER, 1),
        !int_int::is_match(NA_INTEGER, NA_INTEGER)
      ),

      Rcpp::LogicalVector::create(
        real_real_na::is_match(1, 1),
        !real_real_na::is_match(1, 2),
        !real_real_na::is_match(1, NA_REAL),
        !real_real_na::is_match(NA_REAL, 1),
        !real_real_na::is_match(1, R_NaN),
        !real_real_na::is_match(R_NaN, 1),
        !real_real_na::is_match(R_NaN, NA_REAL),
        !real_real_na::is_match(NA_REAL, R_NaN),
        real_real_na::is_match(NA_REAL, NA_REAL),
        real_real_na::is_match(R_NaN, R_NaN),
        real_real::is_match(1, 1),
        !real_real::is_match(1, 2),
        !real_real::is_match(1, NA_REAL),
        !real_real::is_match(NA_REAL, 1),
        !real_real::is_match(1, R_NaN),
        !real_real::is_match(R_NaN, 1),
        !real_real::is_match(R_NaN, NA_REAL),
        !real_real::is_match(NA_REAL, R_NaN),
        !real_real::is_match(NA_REAL, NA_REAL),
        !real_real::is_match(R_NaN, R_NaN)
      ),

      Rcpp::LogicalVector::create(
        int_real_na::is_match(1, 1),
        !int_real_na::is_match(1, 2),
        !int_real_na::is_match(1, NA_REAL),
        !int_real_na::is_match(NA_INTEGER, 1),
        !int_real_na::is_match(1, R_NaN),
        !int_real_na::is_match(NA_INTEGER, R_NaN),
        int_real_na::is_match(NA_INTEGER, NA_REAL),
        int_real::is_match(1, 1),
        !int_real::is_match(1, 2),
        !int_real::is_match(1, NA_REAL),
        !int_real::is_match(NA_INTEGER, 1),
        !int_real::is_match(1, R_NaN),
        !int_real::is_match(NA_INTEGER, R_NaN),
        !int_real::is_match(NA_INTEGER, NA_REAL)
      ),

      Rcpp::LogicalVector::create(
        real_int_na::is_match(1, 1),
        !real_int_na::is_match(1, 2),
        !real_int_na::is_match(1, NA_INTEGER),
        !real_int_na::is_match(NA_REAL, 1),
        !real_int_na::is_match(R_NaN, 1),
        !real_int_na::is_match(R_NaN, NA_INTEGER),
        real_int_na::is_match(NA_REAL, NA_INTEGER),
        real_int::is_match(1, 1),
        !real_int::is_match(1, 2),
        !real_int::is_match(1, NA_INTEGER),
        !real_int::is_match(NA_REAL, 1),
        !real_int::is_match(R_NaN, 1),
        !real_int::is_match(R_NaN, NA_INTEGER),
        !real_int::is_match(NA_REAL, NA_INTEGER)
      )
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
