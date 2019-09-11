#include <Rcpp.h>

#include <dplyr/rlang.h>
#include <dplyr/symbols.h>

namespace dplyr {

const char* address(SEXP x) {
  static char buffer[20];
  snprintf(buffer, 20, "%p", reinterpret_cast<void*>(x));
  return (const char*)buffer;
}
}

// [[Rcpp::export(rng = false)]]
Rcpp::CharacterVector loc(SEXP data) {
  return Rf_mkString(dplyr::address(data));
}

// [[Rcpp::export(rng = false)]]
Rcpp::CharacterVector dfloc(Rcpp::List df) {
  int n = df.size();
  Rcpp::CharacterVector pointers(n);
  for (int i = 0; i < n; i++) {
    pointers[i] = dplyr::address(df[i]);
  }
  Rf_namesgets(pointers, Rf_getAttrib(df, R_NamesSymbol));
  return pointers;
}

// [[Rcpp::export(rng = false)]]
Rcpp::CharacterVector plfloc(Rcpp::Pairlist data) {
  int n = data.size();
  Rcpp::CharacterVector pointers(n), names(n);
  SEXP p = data;
  int i = 0;
  while (! Rf_isNull(p)) {
    pointers[i] = dplyr::address(CAR(p));
    names[i] = PRINTNAME(TAG(p));
    p = CDR(p);
    i++;
  }
  Rf_namesgets(pointers, names);
  return pointers;
}
