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
  SEXP all_vars = Rf_findVarInFrame(env_private, dplyr::symbols::all_vars);

  // search for position of name
  R_xlen_t n = XLENGTH(all_vars);
  R_xlen_t i_name = find_first(all_vars, name);

  bool is_new_column = i_name == n;
  if (is_new_column) {
    SEXP new_all_vars = PROTECT(Rf_allocVector(STRSXP, n + 1));

    for (R_xlen_t i = 0; i < n; i++) {
      SET_STRING_ELT(new_all_vars, i, STRING_ELT(all_vars, i));
    }
    SET_STRING_ELT(new_all_vars, n, name);

    Rf_defineVar(dplyr::symbols::all_vars, new_all_vars, env_private);

    UNPROTECT(1);
  }

  SEXP sym_name = Rf_installChar(name);
  SEXP chops = Rf_findVarInFrame(env_private, Rf_install("chops"));
  Rf_defineVar(sym_name, chunks, chops);

  SEXP masks = Rf_findVarInFrame(env_private, Rf_install("masks"));
  R_xlen_t n_groups = XLENGTH(masks);
  for (R_xlen_t i = 0; i < n_groups; i++) {
    Rf_defineVar(sym_name, VECTOR_ELT(chunks, i), ENCLOS(VECTOR_ELT(masks, i)));
  }

  return R_NilValue;
}

SEXP dplyr_mask_remove(SEXP env_private, SEXP s_name) {
  SEXP name = STRING_ELT(s_name, 0);

  SEXP all_vars = Rf_findVarInFrame(env_private, dplyr::symbols::all_vars);

  // search for position of name
  R_xlen_t n = XLENGTH(all_vars);
  R_xlen_t i_name = find_first(all_vars, name);

  if (i_name != n) {
    // all_vars <- setdiff(all_vars, name)
    SEXP new_all_vars = Rf_allocVector(STRSXP, n - 1);
    for (R_xlen_t i = 0, j = 0; i < n; i++) {
      if (i == i_name) continue;
      SET_STRING_ELT(new_all_vars, j++, STRING_ELT(all_vars, i));
    }

    //  no C-api for rm() so callback to R :shrug:
    SEXP chops = Rf_findVarInFrame(env_private, dplyr::symbols::chops);

    SEXP sym_name = Rf_installChar(name);
    SEXP rm_call = PROTECT(Rf_lang3(Rf_install("rm"), sym_name, chops));
    SET_TAG(CDDR(rm_call), Rf_install("envir"));
    Rf_eval(rm_call, R_BaseEnv);

    SEXP masks = Rf_findVarInFrame(env_private, dplyr::symbols::masks);
    R_xlen_t n = XLENGTH(masks);
    for (R_xlen_t i = 0; i < n; i++) {
      SETCAR(CDDR(rm_call), ENCLOS(VECTOR_ELT(masks, i)));
      Rf_eval(rm_call, R_BaseEnv);
    }

    UNPROTECT(1);
  }

  return R_NilValue;
}
