#include "dplyr.h"

SEXP dplyr_mask_eval_all(SEXP quo, SEXP env_private, SEXP env_context) {
  SEXP rows = PROTECT(Rf_findVarInFrame(env_private, dplyr::symbols::rows));
  R_xlen_t ngroups = XLENGTH(rows);

  SEXP mask = PROTECT(Rf_findVarInFrame(env_private, dplyr::symbols::mask));
  SEXP caller = PROTECT(Rf_findVarInFrame(env_private, dplyr::symbols::caller));

  SEXP chunks = PROTECT(Rf_allocVector(VECSXP, ngroups));

  for (R_xlen_t i = 0; i < ngroups; i++) {
    SEXP current_group = PROTECT(Rf_ScalarInteger(i + 1));
    Rf_defineVar(dplyr::symbols::current_group, current_group, env_private);
    Rf_defineVar(dplyr::symbols::dot_dot_group_size, Rf_ScalarInteger(XLENGTH(VECTOR_ELT(rows, i))), env_context);
    Rf_defineVar(dplyr::symbols::dot_dot_group_number, current_group, env_context);

    SET_VECTOR_ELT(chunks, i, rlang::eval_tidy(quo, mask, caller));

    UNPROTECT(1);
  }

  UNPROTECT(4);

  return chunks;
}
