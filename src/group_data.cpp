#include "dplyr.h"

SEXP dplyr_group_indices(SEXP data, SEXP rows) {
  R_xlen_t nr = vctrs::short_vec_size(data);
  if (nr == 0) {
    return dplyr::vectors::empty_int_vector;
  }

  SEXP indices = PROTECT(Rf_allocVector(INTSXP, nr));
  int* p_indices = INTEGER(indices);
  R_xlen_t ng = XLENGTH(rows);
  const SEXP* p_rows = VECTOR_PTR_RO(rows);

  for (R_xlen_t i = 0; i < ng; i++) {
    SEXP rows_i = p_rows[i];
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
  SEXP old_names = PROTECT(Rf_getAttrib(group_data, R_NamesSymbol));
  SEXP new_names = PROTECT(Rf_allocVector(STRSXP, n));
  SEXP keys = PROTECT(Rf_allocVector(VECSXP, n));
  const SEXP* p_old_names = STRING_PTR_RO(old_names);
  for (R_xlen_t i=0; i<n; i++){
    SET_STRING_ELT(new_names, i, p_old_names[i]);
    SET_VECTOR_ELT(keys, i, VECTOR_ELT(group_data, i));
  }
  Rf_copyMostAttrib(group_data, keys);
  Rf_setAttrib(keys, R_NamesSymbol, new_names);
  Rf_setAttrib(keys, dplyr::symbols::dot_drop, R_NilValue);
  UNPROTECT(3);
  return keys;
}
