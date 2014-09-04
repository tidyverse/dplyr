#define USE_RINTERNALS

#include <R.h>
#include <Rinternals.h>

SEXP setattr(SEXP object, SEXP attr, SEXP value) {
  Rf_setAttrib(object, attr, value);
  return R_NilValue;
}

SEXP set_string_elt(SEXP vector, SEXP index, SEXP value) {
  SET_STRING_ELT(vector, INTEGER(index)[0], STRING_ELT(value, 0));
  return R_NilValue;
}


SEXP set_vector_elt(SEXP vector, SEXP index, SEXP value) {
  SET_VECTOR_ELT(vector, INTEGER(index)[0], value);
  return R_NilValue;
}

#undef USE_RINTERNALS
