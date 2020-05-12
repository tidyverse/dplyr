#include "dplyr.h"

bool all_lgl_columns(SEXP data) {
  R_xlen_t nc = XLENGTH(data);

  for (R_xlen_t i = 0; i < nc; i++) {
    if (TYPEOF(VECTOR_ELT(data, i)) != LGLSXP) return false;
  }

  return true;
}

void reduce_lgl(SEXP reduced, SEXP x, int n) {
  R_xlen_t nres = XLENGTH(x);
  int* p_reduced = LOGICAL(reduced);
  if (nres == 1) {
    if (LOGICAL(x)[0] != TRUE) {
      for (R_xlen_t i = 0; i < n; i++, ++p_reduced) {
        *p_reduced = FALSE;
      }
    }
  } else {
    int* p_x = LOGICAL(x);
    for (R_xlen_t i = 0; i < n; i++, ++p_reduced, ++p_x) {
      *p_reduced = *p_reduced == TRUE && *p_x == TRUE ;
    }
  }
}

void filter_check_size(SEXP res, int i, R_xlen_t n, SEXP quos) {
  R_xlen_t nres = vctrs::short_vec_size(res);
  if (nres != n && nres != 1) {
    dplyr::stop_filter_incompatible_size(i, quos, nres, n);
  }
}

void filter_check_type(SEXP res, R_xlen_t i, SEXP quos) {
  if (TYPEOF(res) == LGLSXP) return;

  if (Rf_inherits(res, "data.frame")) {
    R_xlen_t ncol = XLENGTH(res);
    if (ncol == 0) return;

    for (R_xlen_t j=0; j<ncol; j++) {
      SEXP res_j = VECTOR_ELT(res, j);
      if (TYPEOF(res_j) != LGLSXP) {
        SEXP colnames = PROTECT(Rf_getAttrib(res, R_NamesSymbol));
        SEXP colnames_j = PROTECT(Rf_allocVector(STRSXP, 1));
        SET_STRING_ELT(colnames_j, 0, STRING_ELT(colnames, j));
        dplyr::stop_filter_incompatible_type(i, quos, colnames_j, res_j);
      }
    }
  } else {
    dplyr::stop_filter_incompatible_type(i, quos, R_NilValue, res);
  }
}

SEXP eval_filter_one(SEXP quos, SEXP mask, SEXP caller, R_xlen_t n, SEXP env_filter) {
  // then reduce to a single logical vector of size n
  SEXP reduced = PROTECT(Rf_allocVector(LGLSXP, n));

  // init with TRUE
  int* p_reduced = LOGICAL(reduced);
  for (R_xlen_t i = 0; i < n ; i++, ++p_reduced) {
    *p_reduced = TRUE;
  }

  // reduce
  R_xlen_t nquos = XLENGTH(quos);
  for (R_xlen_t i=0; i < nquos; i++) {
    SEXP current_expression = PROTECT(Rf_ScalarInteger(i+1));
    Rf_defineVar(dplyr::symbols::current_expression, current_expression, env_filter);

    SEXP res = PROTECT(rlang::eval_tidy(VECTOR_ELT(quos, i), mask, caller));

    filter_check_size(res, i, n, quos);
    filter_check_type(res, i, quos);

    if (TYPEOF(res) == LGLSXP) {
      reduce_lgl(reduced, res, n);
    } else if(Rf_inherits(res, "data.frame")) {
      R_xlen_t ncol = XLENGTH(res);
      for (R_xlen_t j=0; j<ncol; j++) {
        reduce_lgl(reduced, VECTOR_ELT(res, j), n);
      }
    }

    UNPROTECT(2);
  }

  UNPROTECT(1);
  return reduced;
}

SEXP dplyr_mask_eval_all_filter(SEXP quos, SEXP env_private, SEXP s_n, SEXP env_filter) {
  DPLYR_MASK_INIT();

  R_xlen_t n = Rf_asInteger(s_n);
  SEXP keep = PROTECT(Rf_allocVector(LGLSXP, n));
  int* p_keep = LOGICAL(keep);

  for (R_xlen_t i = 0; i < ngroups; i++) {
    DPLYR_MASK_SET_GROUP(i);
    SEXP rows_i = VECTOR_ELT(rows, i);
    R_xlen_t n_i = XLENGTH(rows_i);

    SEXP result_i = PROTECT(eval_filter_one(quos, mask, caller, n_i, env_filter));

    int* p_rows_i = INTEGER(rows_i);
    int* p_result_i = LOGICAL(result_i);
    for (R_xlen_t j = 0; j < n_i; j++, ++p_rows_i, ++p_result_i) {
      p_keep[*p_rows_i - 1] = *p_result_i == TRUE;
    }

    UNPROTECT(1);
  }

  UNPROTECT(1);
  DPLYR_MASK_FINALISE();

  return keep;
}
