#include "pch.h"

#include <tools/utils.h>

#include <dplyr/main.h>
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


//' Enable internal logging
//'
//' Log entries, depending on the log level, will be printed to the standard
//' error stream.
//'
//' @param log_level A character value, one of "WARN", "INFO", "DEBUG", "VERB",
//'   or "NONE".
//'
//' @keywords internal
// [[Rcpp::export(rng = false)]]
void init_logging(const std::string& log_level) {
  plog::init_r(log_level);
}
