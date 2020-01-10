#include "dplyr.h"

SEXP dplyr_mask_eval_all_summarise(SEXP quo, SEXP env_private, SEXP env_context, SEXP dots_names, SEXP sexp_i) {
  SEXP rows = PROTECT(Rf_findVarInFrame(env_private, dplyr::symbols::rows));
  R_xlen_t ngroups = XLENGTH(rows);

  SEXP mask = PROTECT(Rf_findVarInFrame(env_private, dplyr::symbols::mask));
  SEXP caller = PROTECT(Rf_findVarInFrame(env_private, dplyr::symbols::caller));

  SEXP chunks = PROTECT(Rf_allocVector(VECSXP, ngroups));
  for (R_xlen_t i = 0; i < ngroups; i++) {
    SEXP rows_i = VECTOR_ELT(rows, i);
    R_xlen_t n_i = XLENGTH(rows_i);
    SEXP current_group = PROTECT(Rf_ScalarInteger(i + 1));
    Rf_defineVar(dplyr::symbols::current_group, current_group, env_private);
    Rf_defineVar(dplyr::symbols::dot_dot_group_size, Rf_ScalarInteger(n_i), env_context);
    Rf_defineVar(dplyr::symbols::dot_dot_group_number, current_group, env_context);

    SEXP result_i = PROTECT(rlang::eval_tidy(quo, mask, caller));
    if (!vctrs::vec_is_vector(result_i)) {
      dplyr::stop_summarise_unsupported_type(result_i);
    }

    SET_VECTOR_ELT(chunks, i, result_i);

    UNPROTECT(2);
  }

  UNPROTECT(4);
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
        dplyr::stop_summarise_incompatible_size(size_i);
      }
    }
    return size;
  }
}
