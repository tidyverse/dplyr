#include "pch.h"

#include <dplyr/symbols.h>

// [[Rcpp::export(rng = false)]]
SEXP filter_update_rows(int n_rows, SEXP group_indices, SEXP keep, SEXP new_rows_sizes) {
  R_xlen_t n_groups = XLENGTH(new_rows_sizes);

  SEXP new_rows = PROTECT(Rf_allocVector(VECSXP, n_groups));
  Rf_setAttrib(new_rows, R_ClassSymbol, dplyr::vectors::classes_vctrs_list_of);
  Rf_setAttrib(new_rows, dplyr::symbols::ptype, dplyr::vectors::empty_int_vector);

  // allocate each new_rows element
  int* p_new_rows_sizes = INTEGER(new_rows_sizes);
  std::vector<int> tracks(n_groups);
  std::vector<int*> p_new_rows(n_groups);
  for (R_xlen_t i = 0; i < n_groups; i++) {
    SEXP new_rows_i = Rf_allocVector(INTSXP, p_new_rows_sizes[i]);
    SET_VECTOR_ELT(new_rows, i, new_rows_i);
    p_new_rows[i] = INTEGER(new_rows_i);
  }

  // traverse group_indices and keep to fill new_rows
  int* p_group_indices = INTEGER(group_indices);
  int* p_keep = LOGICAL(keep);
  int j = 1;
  for (R_xlen_t i = 0; i < n_rows; i++) {
    if (p_keep[i] == TRUE) {
      int g = p_group_indices[i];
      int track = tracks[g - 1]++;
      p_new_rows[g - 1][track] = j++;
    }
  }

  UNPROTECT(1);

  return new_rows;
}
