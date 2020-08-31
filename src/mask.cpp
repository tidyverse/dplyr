#include "dplyr.h"

SEXP as_utf8(SEXP s) {
  if (!IS_UTF8(s) && !IS_ASCII(s)) {
    s = Rf_mkCharCE(Rf_translateCharUTF8(s), CE_UTF8);
  }
  return s;
}

R_xlen_t find_first(SEXP haystack, SEXP needle) {
  needle = as_utf8(needle);
  R_xlen_t n = XLENGTH(haystack);
  R_xlen_t i_name = 0;
  for (; i_name < n; i_name++) {
    if (needle == as_utf8(STRING_ELT(haystack, i_name))) break;
  }

  return i_name;
}

SEXP integers_append(SEXP ints, int x) {
  R_xlen_t n = XLENGTH(ints);
  SEXP new_ints = PROTECT(Rf_allocVector(INTSXP, n + 1));
  int* p_ints = INTEGER(ints);
  int* p_new_ints = INTEGER(new_ints);
  for (R_xlen_t i = 0; i < n; i++) {
    p_new_ints[i] = p_ints[i];
  }
  p_new_ints[n] = x;
  UNPROTECT(1);
  return new_ints;
}

SEXP dplyr_mask_add(SEXP env_private, SEXP s_name, SEXP chunks) {
  SEXP name = STRING_ELT(s_name, 0);

  // we assume control over these
  SEXP resolved = Rf_findVarInFrame(env_private, dplyr::symbols::resolved);
  SEXP names_resolved = PROTECT(Rf_getAttrib(resolved, R_NamesSymbol));
  SEXP used = Rf_findVarInFrame(env_private, dplyr::symbols::used);
  SEXP which_used = Rf_findVarInFrame(env_private, dplyr::symbols::which_used);

  // search for position of name
  R_xlen_t n = XLENGTH(names_resolved);
  R_xlen_t i_name = find_first(names_resolved, name);

  int* p_used = LOGICAL(used);
  bool is_new_column = i_name == n;
  if (is_new_column) {
    SEXP new_used = PROTECT(Rf_allocVector(LGLSXP, n + 1));
    SEXP new_resolved = PROTECT(Rf_allocVector(VECSXP, n + 1));
    SEXP new_names_resolved = PROTECT(Rf_allocVector(STRSXP, n + 1));
    int* p_new_used = LOGICAL(new_used);

    for (R_xlen_t i = 0; i < n; i++) {
      SET_VECTOR_ELT(new_resolved, i, VECTOR_ELT(resolved, i));
      SET_STRING_ELT(new_names_resolved, i, STRING_ELT(names_resolved, i));
      p_new_used[i] = p_used[i];
    }
    SET_VECTOR_ELT(new_resolved, n, chunks);
    SET_STRING_ELT(new_names_resolved, n, name);
    p_new_used[n] = TRUE;

    SEXP new_which_used = PROTECT(integers_append(which_used, n + 1));

    Rf_namesgets(new_resolved, new_names_resolved);
    Rf_defineVar(dplyr::symbols::resolved, new_resolved, env_private);
    Rf_defineVar(dplyr::symbols::used, new_used, env_private);
    Rf_defineVar(dplyr::symbols::which_used, new_which_used, env_private);

    UNPROTECT(4);
  } else {
    SET_VECTOR_ELT(resolved, i_name, chunks);
    p_used[i_name] = TRUE;

    SEXP new_which_used = PROTECT(integers_append(which_used, i_name + 1));
    Rf_defineVar(dplyr::symbols::which_used, new_which_used, env_private);
    UNPROTECT(1);
  }
  UNPROTECT(1); // names_resolved
  return R_NilValue;
}

SEXP dplyr_mask_set(SEXP env_private, SEXP s_name, SEXP chunks) {
  SEXP name = STRING_ELT(s_name, 0);

  // we assume control over these
  SEXP resolved = Rf_findVarInFrame(env_private, dplyr::symbols::resolved);
  SEXP names_resolved = PROTECT(Rf_getAttrib(resolved, R_NamesSymbol));
  SEXP used = Rf_findVarInFrame(env_private, dplyr::symbols::used);

  // search for position of name
  R_xlen_t n = XLENGTH(names_resolved);
  R_xlen_t i_name = find_first(names_resolved, name);
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
  for (R_xlen_t i = 0, j = 0; i < n; i++) {
    if (p_used[i]) {
      p_which_used[j++] = i + 1;
    }
  }
  Rf_defineVar(dplyr::symbols::which_used, which_used, env_private);

  UNPROTECT(1); // which_used
  return R_NilValue;
}
