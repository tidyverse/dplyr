#include "dplyr.h"
#include <string.h>

namespace dplyr {

void stop_filter_incompatible_size(R_xlen_t i, SEXP quos, R_xlen_t nres, R_xlen_t n) {
  DPLYR_ERROR_INIT(3);
    DPLYR_ERROR_SET(0, "index", Rf_ScalarInteger(i + 1));
    DPLYR_ERROR_SET(1, "size", Rf_ScalarInteger(nres));
    DPLYR_ERROR_SET(2, "expected_size", Rf_ScalarInteger(n));

  DPLYR_ERROR_MESG_INIT(1);
    DPLYR_ERROR_MSG_SET(0, "Input `..{index}` must be of size {or_1(expected_size)}, not size {size}.");

  DPLYR_ERROR_THROW("dplyr:::filter_incompatible_size");
}

void stop_filter_incompatible_type(R_xlen_t i, SEXP quos, SEXP column_name, SEXP result){
  DPLYR_ERROR_INIT(3);
    DPLYR_ERROR_SET(0, "index", Rf_ScalarInteger(i + 1));
    DPLYR_ERROR_SET(1, "column_name", column_name);
    DPLYR_ERROR_SET(2, "result", result);

  DPLYR_ERROR_MESG_INIT(1);
    if (column_name == R_NilValue) {
      DPLYR_ERROR_MSG_SET(0, "Input `..{index}` must be a logical vector, not a {vec_ptype_full(result)}.");
    } else {
      DPLYR_ERROR_MSG_SET(0, "Input `..{index}${column_name}` must be a logical vector, not a {vec_ptype_full(result)}.");
    }

  DPLYR_ERROR_THROW("dplyr:::filter_incompatible_type");
}

}

bool all_lgl_columns(SEXP data) {
  R_xlen_t nc = XLENGTH(data);

  const SEXP* p_data = VECTOR_PTR_RO(data);
  for (R_xlen_t i = 0; i < nc; i++) {
    if (TYPEOF(p_data[i]) != LGLSXP) return false;
  }

  return true;
}

void reduce_lgl_and(SEXP reduced, SEXP x, int n) {
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

SEXP reduce_lgl_or(SEXP df, int n) {
  R_xlen_t ncols = XLENGTH(df);
  if (ncols == 0) {
    SEXP res = PROTECT(Rf_allocVector(LGLSXP, n));
    int* p_res = LOGICAL(res);
    for (int i = 0; i < n; i++) {
      p_res[i] = FALSE;
    }
    UNPROTECT(1);
    return res;
  }

  if (ncols == 1) {
    return VECTOR_ELT(df, 0);
  }

  SEXP reduced = PROTECT(Rf_allocVector(LGLSXP, n));
  int* p_reduced = LOGICAL(reduced);
  for (int i = 0; i < n; i++) {
    p_reduced[i] = FALSE;
  }
  const SEXP* p_df = VECTOR_PTR_RO(df);
  for (R_xlen_t j = 0; j < ncols; j++) {
    int* p_df_j = LOGICAL(p_df[j]);
    for (int i = 0; i < n; i++) {
      if (p_df_j[i] == TRUE) {
        p_reduced[i] = TRUE;
      }
    }
  }

  UNPROTECT(1);
  return reduced;
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

    const SEXP* p_res = VECTOR_PTR_RO(res);
    for (R_xlen_t j=0; j<ncol; j++) {
      SEXP res_j = p_res[j];
      if (TYPEOF(res_j) != LGLSXP) {
        SEXP colnames = PROTECT(Rf_getAttrib(res, R_NamesSymbol));
        SEXP colnames_j = PROTECT(Rf_ScalarString(STRING_ELT(colnames, j)));
        dplyr::stop_filter_incompatible_type(i, quos, colnames_j, res_j);
        UNPROTECT(2);
      }
    }
  } else {
    dplyr::stop_filter_incompatible_type(i, quos, R_NilValue, res);
  }
}

SEXP eval_filter_one(SEXP quos, SEXP mask, SEXP caller, R_xlen_t n, SEXP env_filter, bool first) {
  // then reduce to a single logical vector of size n
  SEXP reduced = PROTECT(Rf_allocVector(LGLSXP, n));

  // init with TRUE
  int* p_reduced = LOGICAL(reduced);
  for (R_xlen_t i = 0; i < n ; i++, ++p_reduced) {
    *p_reduced = TRUE;
  }

  // reduce
  R_xlen_t nquos = XLENGTH(quos);
  for (R_xlen_t i = 0; i < nquos; i++) {
    SEXP current_expression = PROTECT(Rf_ScalarInteger(i+1));
    Rf_defineVar(dplyr::symbols::current_expression, current_expression, env_filter);

    SEXP res = PROTECT(rlang::eval_tidy(VECTOR_ELT(quos, i), mask, caller));

    filter_check_size(res, i, n, quos);
    filter_check_type(res, i, quos);

    if (TYPEOF(res) == LGLSXP) {
      reduce_lgl_and(reduced, res, n);
    } else if(Rf_inherits(res, "data.frame")) {
      SEXP combine = PROTECT(Rf_getAttrib(res, dplyr::symbols::filter_combine));
      bool warn = true;
      bool combine_and = true;
      if (TYPEOF(combine) == STRSXP && XLENGTH(combine) == 1) {
        if (strcmp(CHAR(STRING_ELT(combine, 0)), "and") == 0) {
          warn = false;
        } else if (strcmp(CHAR(STRING_ELT(combine, 0)), "or") == 0){
          warn = false;
          combine_and = false;
        }
      }
      if (first && warn) {
        SEXP expr = rlang::quo_get_expr(VECTOR_ELT(quos, i));
        bool across = TYPEOF(expr) == LANGSXP && CAR(expr) == dplyr::symbols::across;
        if (across) {
          Rf_warningcall(R_NilValue, "Using `across()` in `filter()` is deprecated, use `if_any()` or `if_all()`");
        } else {
          Rf_warningcall(R_NilValue, "data frame results in `filter()` are deprecated, use `if_any()` or `if_all()`");
        }
      }

      if (combine_and) {
        const SEXP* p_res = VECTOR_PTR_RO(res);
        R_xlen_t ncol = XLENGTH(res);
        for (R_xlen_t j = 0; j < ncol; j++) {
          reduce_lgl_and(reduced, p_res[j], n);
        }
      } else {
        reduce_lgl_and(
          reduced,
          reduce_lgl_or(res, n),
          n
        );
      }
      UNPROTECT(1);
    }

    UNPROTECT(2);
  }

  UNPROTECT(1);
  return reduced;
}

SEXP dplyr_mask_eval_all_filter(SEXP quos, SEXP env_private, SEXP s_n, SEXP env_filter) {
  DPLYR_MASK_INIT();
  const SEXP* p_rows = VECTOR_PTR_RO(rows);

  R_xlen_t n = Rf_asInteger(s_n);
  SEXP keep = PROTECT(Rf_allocVector(LGLSXP, n));
  int* p_keep = LOGICAL(keep);

  for (R_xlen_t i = 0; i < ngroups; i++) {
    DPLYR_MASK_SET_GROUP(i);
    SEXP rows_i = p_rows[i];
    R_xlen_t n_i = XLENGTH(rows_i);

    SEXP result_i = PROTECT(eval_filter_one(quos, mask, caller, n_i, env_filter, i == 0));

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
