#include "dplyr.h"

SEXP dplyr_across_recycle(SEXP x) {
  R_xlen_t n = XLENGTH(x);
  if (n == 0) {
    return x;
  }

  bool all_same = true;
  R_len_t size = vctrs::short_vec_size(VECTOR_ELT(x, 0));
  for (R_xlen_t i=1; i<n; i++) {
    R_len_t size_i = vctrs::short_vec_size(VECTOR_ELT(x, i));
    if (size_i != size) {
      all_same = false;
      if (size_i > size) {
        size = size_i;
      }
    }
  }

  if (all_same) {
    return x;
  }

  // actually do some recycling
  SEXP out = PROTECT(Rf_allocVector(VECSXP, n));
  for (R_xlen_t i=0; i < n; i++) {
    SET_VECTOR_ELT(out, i,
      vctrs::short_vec_recycle(VECTOR_ELT(x, i), size)
    );
  }
  UNPROTECT(1);

  return out;
}
