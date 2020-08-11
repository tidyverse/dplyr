#include "dplyr.h"

SEXP dplyr_mask_set(SEXP env_private, SEXP name, SEXP chunks) {
  // we assume control over these
  SEXP resolved = Rf_findVarInFrame(env_private, dplyr::symbols::resolved);
  SEXP names_resolved = PROTECT(Rf_getAttrib(resolved, R_NamesSymbol));
  SEXP used = Rf_findVarInFrame(env_private, dplyr::symbols::used);

  // search for position of name
  SEXP char_name = STRING_ELT(name, 0);
  R_xlen_t n = XLENGTH(resolved);
  R_xlen_t i_name = 0;
  for (; i_name < n; i_name++) {
    if (char_name == STRING_ELT(names_resolved, i_name)) break;
  }
  UNPROTECT(1); // names_resolved

  if (i_name == n && chunks == R_NilValue) {
    // early return, as this is removing a resolved that wasn't
    // so it does nothing
    return R_NilValue;
  }

  // update used
  LOGICAL(used)[i_name] = chunks != R_NilValue;
  SET_VECTOR_ELT(resolved, i_name, chunks);

  // count how many are used
  int* p_used = LOGICAL(used);
  R_xlen_t n_used = 0;
  for (R_xlen_t i = 0; i < n; i++, ++p_used) {
    n_used += *p_used;
  }

  // update which_used
  SEXP which_used = PROTECT(Rf_allocVector(INTSXP, n_used));
  int* p_which_used = INTEGER(which_used);
  p_used = LOGICAL(used);
  for (R_xlen_t i = 0; i < n; i++, ++p_used) {
    if (*p_used) {
      *p_which_used = i + 1;
      ++p_which_used;
    }
  }
  Rf_defineVar(dplyr::symbols::which_used, which_used, env_private);

  UNPROTECT(1); // which_used
  return R_NilValue;
}
