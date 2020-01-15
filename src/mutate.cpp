#include "dplyr.h"

namespace dplyr {
void stop_mutate_mixed_NULL() {
  SEXP sym_stop_mutate_mixed_NULL = Rf_install("stop_mutate_mixed_NULL");
  SEXP call = Rf_lang1(sym_stop_mutate_mixed_NULL);
  Rf_eval(call, dplyr::envs::ns_dplyr);
}

void stop_mutate_not_vector(SEXP result) {
  SEXP sym_stop_mutate_not_vector = Rf_install("stop_mutate_not_vector");
  SEXP call = Rf_lang2(sym_stop_mutate_not_vector, result);
  Rf_eval(call, dplyr::envs::ns_dplyr);
}
}

SEXP dplyr_mask_eval_all_mutate(SEXP quo, SEXP env_private, SEXP env_context, SEXP dots_names, SEXP sexp_i) {
  DPLYR_MASK_INIT();

  SEXP res = PROTECT(Rf_allocVector(VECSXP, 2));
  SEXP chunks = PROTECT(Rf_allocVector(VECSXP, ngroups));
  bool seen_vec = false;
  bool needs_recycle = false;
  bool seen_null = false;

  for (R_xlen_t i = 0; i < ngroups; i++) {
    DPLYR_MASK_SET_GROUP(i);

    SEXP result_i = PROTECT(DPLYR_MASK_EVAL(quo));
    SET_VECTOR_ELT(chunks, i, result_i);

    if (Rf_isNull(result_i)) {
      seen_null = true;

      if (seen_vec) {
        dplyr::stop_mutate_mixed_NULL();
      }

    } else if (vctrs::vec_is_vector(result_i)) {
      seen_vec = true;

      if (vctrs::short_vec_size(result_i) != n_i) {
        needs_recycle = true;
      }

    } else {
      dplyr::stop_mutate_not_vector(result_i);
    }

    UNPROTECT(1);
  }

  if (seen_null && seen_vec) {
    // find out the first time the group was NULL so that the error will
    // be associated with this group
    for (int i = 0; i < ngroups; i++) {
      if (Rf_isNull(VECTOR_ELT(chunks, i))) {
        DPLYR_MASK_SET_GROUP(i);
        dplyr::stop_mutate_mixed_NULL();
      }
    }
  }

  // there was only NULL results
  if (ngroups > 0 && !seen_vec) {
    chunks = R_NilValue;
  }
  SET_VECTOR_ELT(res, 0, chunks);
  SET_VECTOR_ELT(res, 1, Rf_ScalarLogical(needs_recycle));

  UNPROTECT(2);
  DPLYR_MASK_FINALISE();

  return res;
}
