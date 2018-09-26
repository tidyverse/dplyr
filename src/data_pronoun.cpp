#include "pch.h"
#include <dplyr/main.h>

// simple dplyr specific data pronoun that needs to look in two environments
// stored in a list
SEXP data_pronoun(SEXP data_mask) {
  SEXP pronoun = PROTECT(Rf_allocVector(VECSXP, 2));
  SEXP resolved = Rf_findVarInFrame3(data_mask, Rf_install(".top_env"), FALSE);
  SEXP active = ENCLOS(resolved);
  SET_VECTOR_ELT(pronoun, 0, resolved);
  SET_VECTOR_ELT(pronoun, 1, active);
  Rf_setAttrib(pronoun, R_ClassSymbol, Rf_mkString("dplyr_data_pronoun"));
  UNPROTECT(1);
  return pronoun;
}

// [[Rcpp::export]]
SEXP dollar_data_pronoun(SEXP x, SEXP symbol) {
  SEXP v;

  // first look in resolved
  SEXP resolved = VECTOR_ELT(x, 0);
  v = Rf_findVarInFrame3(resolved, symbol, FALSE);
  if (v != R_UnboundValue) {
    return v;
  }

  // then in active
  SEXP active = VECTOR_ELT(x, 1);
  v = Rf_findVarInFrame3(active, symbol, FALSE);
  if (v != R_UnboundValue) {
    return v;
  }

  // give up
  stop("Object `%s` not found in `.data`", CHAR(PRINTNAME(symbol)));
}
