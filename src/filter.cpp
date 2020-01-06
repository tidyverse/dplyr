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

void filter_check_size(SEXP res, int i, R_xlen_t n, R_xlen_t group_index, SEXP data) {
  R_xlen_t nres = vctrs::short_vec_size(res);
  if (nres != n && nres != 1) {
    dplyr::stop_filter_incompatible_size(i, group_index, nres, n, data);
  }
}

void filter_check_type(SEXP res, R_xlen_t i, R_xlen_t group_index, SEXP data) {
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
        dplyr::stop_filter_incompatible_type(i, colnames_j, group_index, res_j, data);
      }
    }
  } else {
    dplyr::stop_filter_incompatible_type(i, R_NilValue, group_index, res, data);
  }
}

SEXP eval_filter_one(SEXP quos, SEXP mask, SEXP caller, R_xlen_t nquos, R_xlen_t n, R_xlen_t group_index, SEXP full_data, SEXP env_filter) {
  // then reduce to a single logical vector of size n
  SEXP reduced = PROTECT(Rf_allocVector(LGLSXP, n));

  // init with TRUE
  int* p_reduced = LOGICAL(reduced);
  for (R_xlen_t i = 0; i < n ; i++, ++p_reduced) {
    *p_reduced = TRUE;
  }

  // reduce
  for (R_xlen_t i=0; i < nquos; i++) {
    SEXP current_expression = PROTECT(Rf_ScalarInteger(i+1));
    Rf_defineVar(dplyr::symbols::current_expression, current_expression, env_filter);

    SEXP res = PROTECT(rlang::eval_tidy(VECTOR_ELT(quos, i), mask, caller));

    filter_check_size(res, i, n, group_index, full_data);
    filter_check_type(res, i, group_index, full_data);

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

SEXP dplyr_mask_eval_all_filter(SEXP quos, SEXP env_private, SEXP env_context, SEXP s_n, SEXP full_data, SEXP env_filter) {
  R_xlen_t nquos = XLENGTH(quos);
  SEXP rows = PROTECT(Rf_findVarInFrame(env_private, dplyr::symbols::rows));
  R_xlen_t ngroups = XLENGTH(rows);

  SEXP mask = PROTECT(Rf_findVarInFrame(env_private, dplyr::symbols::mask));
  SEXP caller = PROTECT(Rf_findVarInFrame(env_private, dplyr::symbols::caller));

  R_xlen_t n = Rf_asInteger(s_n);
  SEXP keep = PROTECT(Rf_allocVector(LGLSXP, n));
  int* p_keep = LOGICAL(keep);
  SEXP new_group_sizes = PROTECT(Rf_allocVector(INTSXP, ngroups));
  int* p_new_group_sizes = INTEGER(new_group_sizes);

  SEXP group_indices = PROTECT(Rf_allocVector(INTSXP, n));
  int* p_group_indices = INTEGER(group_indices);

  SEXP res = PROTECT(Rf_allocVector(VECSXP, 3));
  SET_VECTOR_ELT(res, 0, keep);
  SET_VECTOR_ELT(res, 1, new_group_sizes);
  SET_VECTOR_ELT(res, 2, group_indices);

  for (R_xlen_t i = 0; i < ngroups; i++) {
    SEXP rows_i = VECTOR_ELT(rows, i);
    R_xlen_t n_i = XLENGTH(rows_i);
    SEXP current_group = PROTECT(Rf_ScalarInteger(i + 1));
    Rf_defineVar(dplyr::symbols::current_group, current_group, env_private);
    Rf_defineVar(dplyr::symbols::dot_dot_group_size, Rf_ScalarInteger(n_i), env_context);
    Rf_defineVar(dplyr::symbols::dot_dot_group_number, current_group, env_context);

    SEXP result_i = PROTECT(eval_filter_one(quos, mask, caller, nquos, n_i, i, full_data, env_filter));

    // sprinkle back to overall logical vector
    int* p_rows_i = INTEGER(rows_i);
    int* p_result_i = LOGICAL(result_i);
    int nkeep = 0;
    for (R_xlen_t j = 0; j < n_i; j++, ++p_rows_i, ++p_result_i) {
      p_keep[*p_rows_i - 1] = *p_result_i == TRUE;
      p_group_indices[*p_rows_i - 1] = i + 1;
      nkeep += (*p_result_i == TRUE);
    }
    p_new_group_sizes[i] = nkeep;

    UNPROTECT(2);
  }

  UNPROTECT(7);

  return res;
}

SEXP dplyr_filter_update_rows(SEXP s_n_rows, SEXP group_indices, SEXP keep, SEXP new_rows_sizes) {
  int n_rows = INTEGER(s_n_rows)[0];
  R_xlen_t n_groups = XLENGTH(new_rows_sizes);

  SEXP new_rows = PROTECT(Rf_allocVector(VECSXP, n_groups));
  Rf_setAttrib(new_rows, R_ClassSymbol, dplyr::vectors::classes_vctrs_list_of);
  Rf_setAttrib(new_rows, dplyr::symbols::ptype, dplyr::vectors::empty_int_vector);

  // allocate each new_rows element
  int* p_new_rows_sizes = INTEGER(new_rows_sizes);
  SEXP tracks = PROTECT(Rf_allocVector(INTSXP, n_groups));
  int* p_tracks = INTEGER(tracks);
  memset(p_tracks, 0, n_groups * sizeof(int));
  int** p_new_rows = (int**)R_alloc(n_groups, sizeof(int*));

  for (R_xlen_t i = 0; i < n_groups; i++) {
    SEXP new_rows_i = Rf_allocVector(INTSXP, p_new_rows_sizes[i]);
    SET_VECTOR_ELT(new_rows, i, new_rows_i);
    p_new_rows[i] = INTEGER(new_rows_i);
  }

  // traverse group_indices and keep to fill new_rows
  p_tracks = INTEGER(tracks);
  int* p_group_indices = INTEGER(group_indices);
  int* p_keep = LOGICAL(keep);
  R_xlen_t j = 1;
  for (R_xlen_t i = 0; i < n_rows; i++) {
    if (p_keep[i] == TRUE) {
      int g = p_group_indices[i];
      int track = p_tracks[g - 1]++;
      p_new_rows[g - 1][track] = j++;
    }
  }

  UNPROTECT(2);

  return new_rows;
}
