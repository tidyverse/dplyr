#include "dplyr.h"

// new_rows <- vector("list", length(old_rows))
//   breaks <- cumsum(c(1L, list_sizes(old_rows)))
//   start <- breaks[-length(breaks)]
// end <- breaks[-1] - 1L

SEXP bundle_rows(SEXP old_rows) {
  R_xlen_t n = XLENGTH(old_rows);
  SEXP new_rows = PROTECT(Rf_allocVector(VECSXP, n));

  R_len_t k = 0;
  const SEXP* p_old_rows = VECTOR_PTR_RO(old_rows);
  for (R_xlen_t i = 0; i < n; i++) {
    R_len_t size = LENGTH(p_old_rows[i]);
    if (size > 0) {
      SET_VECTOR_ELT(new_rows, i, vctrs::compact_seq(k, size, true));
      k += size;
    } else {
      SET_VECTOR_ELT(new_rows, i, dplyr::vectors::empty_int_vector);
    }
  }

  UNPROTECT(1);
  return new_rows;
}
