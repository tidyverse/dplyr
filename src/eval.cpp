#include "dplyr.h"

// 1 quosure evaluated across all groups
//
// - Returns a list containing the results of evaluating `quo` on each group
SEXP dplyr_mask_eval_quo(SEXP quo, SEXP env_private) {
  DPLYR_MASK_INIT();

  SEXP out = PROTECT(Rf_allocVector(VECSXP, ngroups));

  for (R_xlen_t i = 0; i < ngroups; i++) {
    DPLYR_MASK_ITERATION_INIT();
    DPLYR_MASK_SET_GROUP(i);
    SET_VECTOR_ELT(out, i, DPLYR_MASK_EVAL(quo));
    DPLYR_MASK_ITERATION_FINALISE();
  }

  UNPROTECT(1);
  DPLYR_MASK_FINALISE();

  return out;
}

// N quosures evaluated across all groups
//
// - `quo_index` is updated in place so if an error occurs, you know which
//   quosure it errored in
// - Returns a list the same length as `quos` where each element is another
//   list containing the results of evaluating that `quo` on each group.
SEXP dplyr_mask_eval_quos(SEXP quos, SEXP env_private, SEXP quo_index) {
  DPLYR_MASK_INIT();

  const R_xlen_t n_quos = Rf_xlength(quos);
  SEXP const* v_quos = VECTOR_PTR_RO(quos);

  // Outer list the same size as the number of expressions
  SEXP out = PROTECT(Rf_allocVector(VECSXP, n_quos));
  SEXP const* v_out = VECTOR_PTR_RO(out);

  // Inner lists the same size as the number of groups
  for (R_xlen_t i = 0; i < n_quos; ++i) {
    SET_VECTOR_ELT(out, i, Rf_allocVector(VECSXP, ngroups));
  }

  // Passed from the R side to update in place for error messaging
  int* p_quo_index = INTEGER(quo_index);

  for (R_xlen_t i = 0; i < ngroups; ++i) {
    DPLYR_MASK_ITERATION_INIT();
    DPLYR_MASK_SET_GROUP(i);

    for (R_xlen_t j = 0; j < n_quos; ++j) {
      *p_quo_index = j + 1;
      SEXP elt = DPLYR_MASK_EVAL(v_quos[j]);
      SET_VECTOR_ELT(v_out[j], i, elt);
    }

    DPLYR_MASK_ITERATION_FINALISE();
  }

  UNPROTECT(1);
  DPLYR_MASK_FINALISE();

  return out;
}
