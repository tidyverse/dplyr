#include "dplyr.h"

SEXP dplyr_vec_chop(SEXP x, SEXP indices) {
  return vctrs::vec_chop(x, indices);
}

SEXP bundle_rows(SEXP old_rows) {
  R_xlen_t n = XLENGTH(old_rows);
  SEXP new_rows = PROTECT(Rf_allocVector(VECSXP, n));

  R_len_t k = 0;
  const SEXP* p_old_rows = VECTOR_PTR_RO(old_rows);
  for (R_xlen_t i = 0; i < n; i++) {
    R_len_t size = LENGTH(p_old_rows[i]);
    SET_VECTOR_ELT(new_rows, i, vctrs::compact_seq(k, size, true));
    k += size;
  }

  UNPROTECT(1);
  return new_rows;
}
