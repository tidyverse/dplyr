#include <dplyr/main.h>

#include <dplyr/registration.h>
#include <dplyr/HybridHandler.h>

using namespace Rcpp;
using namespace dplyr;

#define DPLYR_REGISTER(__FUN__) do { R_RegisterCCallable( "dplyr", #__FUN__, (DL_FUNC)__FUN__ ); } while(0);

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

// [[Rcpp::interfaces(cpp)]]
// [[Rcpp::export]]
void registerHybridHandler(const char* name, HybridHandler proto);

// work around a problem (?) in Rcpp
// [[Rcpp::interfaces(cpp)]]
// [[Rcpp::export]]
DataFrame build_index_cpp(DataFrame data);


extern "C" void R_init_dplyr(BOOST_ATTRIBUTE_UNUSED DllInfo* info) {
  DPLYR_REGISTER(build_index_cpp);
  DPLYR_REGISTER(registerHybridHandler);

  DPLYR_REGISTER(get_time_classes);
  DPLYR_REGISTER(get_date_classes);
}

