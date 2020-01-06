#include "dplyr.h"

SEXP dplyr_mask_eval_all_mutate(SEXP quo, SEXP env_private, SEXP env_context, SEXP dots_names, SEXP sexp_i) {
  SEXP rows = PROTECT(Rf_findVarInFrame(env_private, dplyr::symbols::rows));
  R_xlen_t ngroups = XLENGTH(rows);

  SEXP mask = PROTECT(Rf_findVarInFrame(env_private, dplyr::symbols::mask));
  SEXP caller = PROTECT(Rf_findVarInFrame(env_private, dplyr::symbols::caller));

  SEXP res = PROTECT(Rf_allocVector(VECSXP, 2));
  SEXP chunks = PROTECT(Rf_allocVector(VECSXP, ngroups));
  bool seen_vec = false;
  bool needs_recycle = false;

  for (R_xlen_t i = 0; i < ngroups; i++) {
    SEXP rows_i = VECTOR_ELT(rows, i);
    R_xlen_t n_i = XLENGTH(rows_i);
    SEXP current_group = PROTECT(Rf_ScalarInteger(i + 1));
    Rf_defineVar(dplyr::symbols::current_group, current_group, env_private);
    Rf_defineVar(dplyr::symbols::dot_dot_group_size, Rf_ScalarInteger(n_i), env_context);
    Rf_defineVar(dplyr::symbols::dot_dot_group_number, current_group, env_context);

    SEXP result_i = PROTECT(rlang::eval_tidy(quo, mask, caller));
    if (Rf_isNull(result_i)) {
      if (seen_vec) {
        // the current chunk is NULL but there were some non NULL
        // chunks, so this is an error
        Rf_errorcall(R_NilValue, "incompatible results for mutate(), some results are NULL");
      } else {
        UNPROTECT(2);
        continue;
      }
    } else {
      seen_vec = true;
    }

    if (!vctrs::vec_is_vector(result_i)) {
      if (!Rf_isNull(dots_names)) {
        SEXP name = STRING_ELT(dots_names, i);
        if (XLENGTH(name) > 0) {
          Rf_errorcall(R_NilValue, "Unsupported type for result `%s`", CHAR(name));
        }
      }
      int i = INTEGER(sexp_i)[0];
      Rf_errorcall(R_NilValue, "Unsupported type at index %d", i);
    }

    if (!needs_recycle && vctrs::short_vec_size(result_i) != n_i) {
      needs_recycle = true;
    }

    SET_VECTOR_ELT(chunks, i, result_i);

    UNPROTECT(2);
  }


  // there was only NULL results
  if (ngroups > 0 && !seen_vec) {
    chunks = R_NilValue;
  }
  SET_VECTOR_ELT(res, 0, chunks);
  SET_VECTOR_ELT(res, 1, Rf_ScalarLogical(needs_recycle));

  UNPROTECT(5);

  return res;
}
