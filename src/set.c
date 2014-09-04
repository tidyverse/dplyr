#include <R.h>
#include <Rdefines.h>

SEXP setattr_(SEXP object, SEXP attr, SEXP value) {
  Rf_setAttrib(object, attr, value);
  return R_NilValue;
}

SEXP set_string_elt_(SEXP vector, SEXP index, SEXP value) {
  SET_STRING_ELT(vector, INTEGER(index)[0], STRING_ELT(value, 0));
  return R_NilValue;
}

SEXP set_vector_elt_(SEXP vector, SEXP index, SEXP value) {
  SET_VECTOR_ELT(vector, INTEGER(index)[0], value);
  return R_NilValue;
}
