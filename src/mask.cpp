#include "dplyr.h"

SEXP as_utf8(SEXP s) {
  if (!IS_UTF8(s) && !IS_ASCII(s)) {
    s = Rf_mkCharCE(Rf_translateCharUTF8(s), CE_UTF8);
  }
  return s;
}

R_xlen_t find_first(SEXP haystack, SEXP needle) {
  SEXP needle_utf8 = PROTECT(as_utf8(needle));
  R_xlen_t n = XLENGTH(haystack);
  R_xlen_t i_name = 0;
  for (; i_name < n; i_name++) {
    if (needle_utf8 == as_utf8(STRING_ELT(haystack, i_name))) break;
  }
  UNPROTECT(1);
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

SEXP dplyr_mask_add(SEXP env_private, SEXP s_name, SEXP ptype, SEXP chunks) {
  SEXP name = STRING_ELT(s_name, 0);

  // we assume control over these
  SEXP current_data = PROTECT(Rf_findVarInFrame(env_private, dplyr::symbols::current_data));
  SEXP current_vars = PROTECT(Rf_getAttrib(current_data, R_NamesSymbol));

  // search for position of name
  R_xlen_t n = XLENGTH(current_data);
  R_xlen_t i_name = find_first(current_vars, name);

  bool is_new_column = i_name == n;
  if (is_new_column) {
    SEXP new_current_vars = PROTECT(Rf_allocVector(STRSXP, n + 1));
    SEXP new_current_data = PROTECT(Rf_allocVector(VECSXP, n + 1));

    for (R_xlen_t i = 0; i < n; i++) {
      SET_STRING_ELT(new_current_vars, i, STRING_ELT(current_vars, i));
      SET_VECTOR_ELT(new_current_data, i, VECTOR_ELT(current_data, i));
    }
    SET_STRING_ELT(new_current_vars, n, name);
    SET_VECTOR_ELT(new_current_data, n, ptype);
    Rf_namesgets(new_current_data, new_current_vars);

    Rf_defineVar(dplyr::symbols::current_data, new_current_data, env_private);

    UNPROTECT(2);
  } else {
    SET_VECTOR_ELT(current_data, i_name, ptype);
  }

  SEXP sym_name = PROTECT(rlang::str_as_symbol(name));
  SEXP chops = PROTECT(Rf_findVarInFrame(env_private, dplyr::symbols::chops));
  Rf_defineVar(sym_name, chunks, chops);

  SEXP mask = PROTECT(Rf_findVarInFrame(env_private, dplyr::symbols::mask));
  add_mask_binding(sym_name, ENCLOS(mask), chops);

  UNPROTECT(5);
  return R_NilValue;
}

SEXP dplyr_mask_remove(SEXP env_private, SEXP s_name) {
  SEXP name = STRING_ELT(s_name, 0);

  SEXP current_data = PROTECT(Rf_findVarInFrame(env_private, dplyr::symbols::current_data));
  SEXP current_vars = PROTECT(Rf_getAttrib(current_data, R_NamesSymbol));

  // search for position of name
  R_xlen_t n = XLENGTH(current_vars);
  R_xlen_t i_name = find_first(current_vars, name);

  if (i_name != n) {
    SEXP new_current_data = PROTECT(Rf_allocVector(VECSXP, n - 1));
    SEXP new_current_vars = PROTECT(Rf_allocVector(STRSXP, n - 1));

    for (R_xlen_t i = 0, j = 0; i < n; i++) {
      if (i == i_name) continue;
      SET_STRING_ELT(new_current_vars, j, STRING_ELT(current_vars, i));
      SET_VECTOR_ELT(new_current_data, j, VECTOR_ELT(current_data, i));
      j++;
    }
    Rf_namesgets(new_current_data, new_current_vars);
    Rf_defineVar(dplyr::symbols::current_data, new_current_data, env_private);

    SEXP chops = PROTECT(Rf_findVarInFrame(env_private, dplyr::symbols::chops));
    SEXP sym_name = PROTECT(rlang::str_as_symbol(name));
    rlang::env_unbind(chops, sym_name);

    SEXP mask = PROTECT(Rf_findVarInFrame(env_private, dplyr::symbols::mask));
    rlang::env_unbind(ENCLOS(mask), sym_name);

    UNPROTECT(5);
  }

  UNPROTECT(2);
  return R_NilValue;
}
