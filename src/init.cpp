#include "pch.h"
#include <dplyr/main.h>

#include <dplyr/registration.h>
#include <dplyr/HybridHandler.h>

using namespace Rcpp;
using namespace dplyr;

SEXP get_cache() {
  static SEXP cache = 0;
  if (!cache) {
    SEXP vec = PROTECT(Rf_allocVector(VECSXP, 2));
    SEXP date_classes = PROTECT(Rf_mkString("Date"));
    SET_VECTOR_ELT(vec, 0, date_classes);
    CharacterVector time_classes = CharacterVector::create("POSIXct", "POSIXt");
    SET_VECTOR_ELT(vec, 1, time_classes);
    UNPROTECT(2);
    R_PreserveObject(vec);
    cache = vec;
  }
  return cache;
}

// [[Rcpp::interfaces(cpp)]]
// [[Rcpp::export]]
SEXP get_date_classes() {
  return VECTOR_ELT(get_cache(), 0);
}

// [[Rcpp::interfaces(cpp)]]
// [[Rcpp::export]]
SEXP get_time_classes() {
  return VECTOR_ELT(get_cache(), 1);
}

// work around a problem (?) in Rcpp
// [[Rcpp::interfaces(cpp)]]
// [[Rcpp::export]]
DataFrame build_index_cpp(DataFrame data);
