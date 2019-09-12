#include <Rcpp.h>

#include <dplyr/symbols.h>

namespace dplyr {

SEXP get_classes_vctrs_list_of() {
  static Rcpp::CharacterVector klasses(2);
  klasses[0] = "vctrs_list_of";
  klasses[1] = "vctrs_vctr";
  return klasses;
}

SEXP get_empty_int_vector() {
  SEXP x = Rf_allocVector(INTSXP, 0);
  R_PreserveObject(x);
  return x;
}

SEXP symbols::ptype = Rf_install("ptype");
SEXP symbols::levels = Rf_install("levels");

SEXP vectors::classes_vctrs_list_of = get_classes_vctrs_list_of();
SEXP vectors::empty_int_vector = get_empty_int_vector();

}
