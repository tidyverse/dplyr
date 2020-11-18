#include "dplyr.h"

namespace dplyr {
void stop_mutate_recycle_incompatible_size(R_len_t n_result_i) {
  DPLYR_ERROR_INIT(1);
    DPLYR_ERROR_SET(0, "x_size", Rf_ScalarInteger(n_result_i));

  DPLYR_ERROR_MESG_INIT(0);

  DPLYR_ERROR_THROW("dplyr:::mutate_incompatible_size");
}

void stop_mutate_mixed_null() {
  DPLYR_ERROR_INIT(0);
  DPLYR_ERROR_MESG_INIT(0);
  DPLYR_ERROR_THROW("dplyr:::mutate_mixed_null");
}

void stop_mutate_not_vector(SEXP result) {
  DPLYR_ERROR_INIT(1);
    DPLYR_ERROR_SET(0, "result", result);
  DPLYR_ERROR_MESG_INIT(0);
  DPLYR_ERROR_THROW("dplyr:::mutate_not_vector");
}
}

SEXP dplyr_mask_eval_all_mutate(SEXP quo, SEXP env_private) {
  DPLYR_MASK_INIT();

  SEXP chunks = PROTECT(Rf_allocVector(VECSXP, ngroups));
  bool seen_vec = false;
  bool seen_null = false;

  const SEXP* p_rows = VECTOR_PTR_RO(rows);
  for (R_xlen_t i = 0; i < ngroups; i++) {
    DPLYR_MASK_SET_GROUP(i);
    R_xlen_t n_i = XLENGTH(p_rows[i]);
    SEXP result_i = PROTECT(DPLYR_MASK_EVAL(quo));
    SET_VECTOR_ELT(chunks, i, result_i);

    if (Rf_isNull(result_i)) {
      seen_null = true;

      if (seen_vec) {
        dplyr::stop_mutate_mixed_null();
      }

    } else if (vctrs::vec_is_vector(result_i)) {
      seen_vec = true;

      R_len_t n_result_i = vctrs::short_vec_size(result_i);

      if (n_result_i != n_i) {
        // only allow sizes 1 and n_i are allowed
        if (n_result_i != 1) {
          dplyr::stop_mutate_recycle_incompatible_size(n_result_i);
        } else {
          SET_VECTOR_ELT(chunks, i, vctrs::short_vec_recycle(result_i, n_i));
        }
      }

    } else {
      dplyr::stop_mutate_not_vector(result_i);
    }

    UNPROTECT(1);
  }

  if (seen_null && seen_vec) {
    // find out the first time the group was NULL so that the error will
    // be associated with this group
    const SEXP* p_chunks = VECTOR_PTR_RO(chunks);
    for (int i = 0; i < ngroups; i++) {
      if (Rf_isNull(p_chunks[i])) {
        DPLYR_MASK_SET_GROUP(i);
        dplyr::stop_mutate_mixed_null();
      }
    }
  }

  // there was only NULL results
  if (ngroups > 0 && !seen_vec) {
    chunks = R_NilValue;
  }

  UNPROTECT(1);
  DPLYR_MASK_FINALISE();

  return chunks;
}
