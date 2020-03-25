#include "dplyr.h"

SEXP dplyr_group_indices(SEXP rows, SEXP s_nr) {
  R_xlen_t nr = INTEGER(s_nr)[0];
  R_xlen_t ng = XLENGTH(rows);

  SEXP indices = PROTECT(Rf_allocVector(INTSXP, nr));
  int* p_indices = INTEGER(indices);
  for (R_xlen_t i = 0; i < ng; i++) {
    SEXP rows_i = VECTOR_ELT(rows, i);
    R_xlen_t n_i = XLENGTH(rows_i);
    int* p_rows_i = INTEGER(rows_i);
    for (R_xlen_t j = 0; j < n_i; j++, ++p_rows_i) {
      p_indices[*p_rows_i - 1] = i + 1;
    }
  }

  UNPROTECT(1);
  return indices;
}

SEXP dplyr_group_keys(SEXP group_data) {
  R_xlen_t n = XLENGTH(group_data) - 1;
  SEXP old_names = Rf_getAttrib(group_data, R_NamesSymbol);
  SEXP new_names = PROTECT(Rf_allocVector(STRSXP, n));
  SEXP keys = PROTECT(Rf_allocVector(VECSXP, n));
  for (R_xlen_t i=0; i<n; i++){
    SET_STRING_ELT(new_names, i, STRING_ELT(old_names, i));
    SET_VECTOR_ELT(keys, i, VECTOR_ELT(group_data, i));
  }
  Rf_copyMostAttrib(group_data, keys);
  Rf_setAttrib(keys, R_NamesSymbol, new_names);
  Rf_setAttrib(keys, dplyr::symbols::dot_drop, R_NilValue);
  UNPROTECT(2);
  return keys;
}
