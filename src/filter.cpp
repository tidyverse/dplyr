#include "dplyr.h"

namespace dplyr {

static inline
void stop_filter_incompatible_size(R_xlen_t i,
                                   SEXP quos,
                                   R_xlen_t nres,
                                   R_xlen_t n) {
  DPLYR_ERROR_INIT(3);
  DPLYR_ERROR_SET(0, "index", Rf_ScalarInteger(i + 1));
  DPLYR_ERROR_SET(1, "size", Rf_ScalarInteger(nres));
  DPLYR_ERROR_SET(2, "expected_size", Rf_ScalarInteger(n));
  DPLYR_ERROR_THROW("dplyr:::filter_incompatible_size");
}

static inline
void stop_filter_incompatible_type(R_xlen_t i,
                                   SEXP quos,
                                   SEXP column_name,
                                   SEXP result){
  DPLYR_ERROR_INIT(3);
  DPLYR_ERROR_SET(0, "index", Rf_ScalarInteger(i + 1));
  DPLYR_ERROR_SET(1, "column_name", column_name);
  DPLYR_ERROR_SET(2, "result", result);
  DPLYR_ERROR_THROW("dplyr:::filter_incompatible_type");
}

static inline
void signal_filter(const char* cls) {
  SEXP ffi_cls = PROTECT(Rf_mkString(cls));
  SEXP ffi_call = PROTECT(Rf_lang2(dplyr::symbols::dplyr_internal_signal, ffi_cls));
  Rf_eval(ffi_call, dplyr::envs::ns_dplyr);
  UNPROTECT(2);
}
static
void signal_filter_one_column_matrix() {
  signal_filter("dplyr:::signal_filter_one_column_matrix");
}
static
void signal_filter_across() {
  signal_filter("dplyr:::signal_filter_across");
}
static
void signal_filter_data_frame() {
  signal_filter("dplyr:::signal_filter_data_frame");
}

}

// Reduces using logical `&`
static inline
void filter_lgl_reduce(SEXP x, R_xlen_t n, int* p_reduced) {
  const R_xlen_t n_x = Rf_xlength(x);
  const int* p_x = LOGICAL_RO(x);

  if (n_x == 1) {
    if (p_x[0] != TRUE) {
      for (R_xlen_t i = 0; i < n; ++i) {
        p_reduced[i] = FALSE;
      }
    }
  } else {
    for (R_xlen_t i = 0; i < n; ++i) {
      p_reduced[i] = (p_reduced[i] == TRUE) && (p_x[i] == TRUE);
    }
  }
}

static inline
bool filter_is_valid_lgl(SEXP x, bool first) {
  if (TYPEOF(x) != LGLSXP) {
    return false;
  }

  SEXP dim = PROTECT(Rf_getAttrib(x, R_DimSymbol));

  if (dim == R_NilValue) {
    // Bare logical vector
    UNPROTECT(1);
    return true;
  }

  const R_xlen_t dimensionality = Rf_xlength(dim);

  if (dimensionality == 1) {
    // 1 dimension array. We allow these because many things in R produce them.
    UNPROTECT(1);
    return true;
  }

  const int* p_dim = INTEGER(dim);

  if (dimensionality == 2 && p_dim[1] == 1) {
    // 1 column matrix. We allow these with a warning that this will be
    // deprecated in the future.
    if (first) {
      dplyr::signal_filter_one_column_matrix();
    }
    UNPROTECT(1);
    return true;
  }

  UNPROTECT(1);
  return false;
}

static inline
void filter_df_reduce(SEXP x,
                      R_xlen_t n,
                      bool first,
                      R_xlen_t i_quo,
                      SEXP quos,
                      int* p_reduced) {
  if (first) {
    SEXP expr = rlang::quo_get_expr(VECTOR_ELT(quos, i_quo));
    const bool across = TYPEOF(expr) == LANGSXP && CAR(expr) == dplyr::symbols::across;

    if (across) {
      dplyr::signal_filter_across();
    } else {
      dplyr::signal_filter_data_frame();
    }
  }

  const SEXP* p_x = VECTOR_PTR_RO(x);
  const R_xlen_t n_col = Rf_xlength(x);

  for (R_xlen_t i = 0; i < n_col; ++i) {
    SEXP col = p_x[i];

    if (!filter_is_valid_lgl(col, first)) {
      SEXP names = PROTECT(Rf_getAttrib(x, R_NamesSymbol));
      SEXP name = PROTECT(Rf_ScalarString(STRING_ELT(names, i)));
      dplyr::stop_filter_incompatible_type(i_quo, quos, name, col);
      UNPROTECT(2);
    }

    filter_lgl_reduce(col, n, p_reduced);
  }
}

static
SEXP eval_filter_one(SEXP quos,
                     SEXP mask,
                     SEXP caller,
                     R_xlen_t n,
                     SEXP env_filter,
                     bool first) {
  // Reduce to a single logical vector of size `n`

  SEXP reduced = PROTECT(Rf_allocVector(LGLSXP, n));
  int* p_reduced = LOGICAL(reduced);

  // Init with `TRUE`
  for (R_xlen_t i = 0; i < n; ++i) {
    p_reduced[i] = TRUE;
  }

  const R_xlen_t n_quos = Rf_xlength(quos);
  SEXP const* p_quos = VECTOR_PTR_RO(quos);

  // Reduce loop
  for (R_xlen_t i = 0; i < n_quos; ++i) {
    SEXP current_expression = PROTECT(Rf_ScalarInteger(i + 1));
    Rf_defineVar(dplyr::symbols::current_expression, current_expression, env_filter);

    SEXP res = PROTECT(rlang::eval_tidy(p_quos[i], mask, caller));

    const R_xlen_t res_size = vctrs::short_vec_size(res);
    if (res_size != n && res_size != 1) {
      dplyr::stop_filter_incompatible_size(i, quos, res_size, n);
    }

    if (filter_is_valid_lgl(res, first)) {
      filter_lgl_reduce(res, n, p_reduced);
    } else if (Rf_inherits(res, "data.frame")) {
      filter_df_reduce(res, n, first, i, quos, p_reduced);
    } else {
      dplyr::stop_filter_incompatible_type(i, quos, R_NilValue, res);
    }

    UNPROTECT(2);
  }

  UNPROTECT(1);
  return reduced;
}

SEXP dplyr_mask_eval_all_filter(SEXP quos,
                                SEXP env_private,
                                SEXP s_n,
                                SEXP env_filter) {
  DPLYR_MASK_INIT();
  const SEXP* p_rows = VECTOR_PTR_RO(rows);

  const R_xlen_t n = Rf_asInteger(s_n);

  SEXP keep = PROTECT(Rf_allocVector(LGLSXP, n));
  int* p_keep = LOGICAL(keep);

  for (R_xlen_t i = 0; i < ngroups; ++i) {
    DPLYR_MASK_ITERATION_INIT();
    DPLYR_MASK_SET_GROUP(i);

    const bool first = i == 0;

    SEXP rows_i = p_rows[i];
    R_xlen_t n_i = Rf_xlength(rows_i);

    SEXP result_i = PROTECT(eval_filter_one(
      quos,
      mask,
      caller,
      n_i,
      env_filter,
      first
    ));

    const int* p_rows_i = INTEGER(rows_i);
    const int* p_result_i = LOGICAL(result_i);

    for (R_xlen_t j = 0; j < n_i; ++j) {
      p_keep[p_rows_i[j] - 1] = p_result_i[j];
    }

    UNPROTECT(1);
    DPLYR_MASK_ITERATION_FINALISE();
  }

  UNPROTECT(1);
  DPLYR_MASK_FINALISE();

  return keep;
}
