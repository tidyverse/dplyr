#include "dplyr.h"

SEXP dplyr_mask_eval_all_summarise(SEXP quo, SEXP env_private) {
  DPLYR_MASK_INIT();

  SEXP chunks = PROTECT(Rf_allocVector(VECSXP, ngroups));
  for (R_xlen_t i = 0; i < ngroups; i++) {
    DPLYR_MASK_SET_GROUP(i);

    SEXP result_i = PROTECT(DPLYR_MASK_EVAL(quo));
    SET_VECTOR_ELT(chunks, i, result_i);

    if (!vctrs::vec_is_vector(result_i)) {
      dplyr::stop_summarise_unsupported_type(result_i);
    }

    UNPROTECT(1);
  }
  DPLYR_MASK_FINALISE();

  UNPROTECT(1);
  return chunks;
}

SEXP dplyr_vec_sizes(SEXP chunks) {
  R_xlen_t n = XLENGTH(chunks);
  SEXP res = PROTECT(Rf_allocVector(INTSXP, n));
  int* p_res = INTEGER(res);

  for (R_xlen_t i = 0; i < n; i++, ++p_res) {
    *p_res = vctrs::short_vec_size(VECTOR_ELT(chunks, i));
  }

  UNPROTECT(1);
  return res;
}

SEXP dplyr_validate_summarise_sizes(SEXP size, SEXP chunks) {
  R_xlen_t nchunks = XLENGTH(chunks);

  if (XLENGTH(size) == 1 && INTEGER(size)[0] == 1) {
    // we might not have to allocate the vector of sizes if
    // all the chunks are of size 1

    R_xlen_t i = 0;
    for (; i < nchunks; i++) {
      if (vctrs::short_vec_size(VECTOR_ELT(chunks, i)) != 1) {
        break;
      }
    }

    if (i == nchunks) {
      // we can just return the input size
      return size;
    }

    // we need to return a vector to track the new sizes
    size = PROTECT(Rf_allocVector(INTSXP, nchunks));
    int* p_size = INTEGER(size);

    // until i, all sizes are 1
    for (R_xlen_t j = 0; j < i; j++, ++p_size) {
      *p_size = 1;
    }

    // then finish with i
    for (; i < nchunks; i++, ++p_size) {
      *p_size = vctrs::short_vec_size(VECTOR_ELT(chunks, i));
    }
    UNPROTECT(1);
    return size;
  } else {
    // size is already a vector, we need to check if the sizes of chunks
    // matches
    int* p_size = INTEGER(size);
    for (R_xlen_t i = 0; i < nchunks; i++, ++p_size) {
      int size_i = vctrs::short_vec_size(VECTOR_ELT(chunks, i));
      if (size_i != *p_size && size_i != 1) {
        dplyr::stop_summarise_incompatible_size(size_i, i);
      }
    }
    return size;
  }
}
