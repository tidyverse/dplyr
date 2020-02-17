#include "dplyr.h"

namespace dplyr {
void stop_mutate_mixed_NULL() {
  SEXP sym_stop_mutate_mixed_NULL = Rf_install("stop_mutate_mixed_NULL");
  SEXP call = Rf_lang1(sym_stop_mutate_mixed_NULL);
  Rf_eval(call, dplyr::envs::ns_dplyr);
}

void stop_mutate_not_vector(SEXP result) {
  SEXP sym_stop_mutate_not_vector = Rf_install("stop_mutate_not_vector");
  SEXP call = Rf_lang2(sym_stop_mutate_not_vector, result);
  Rf_eval(call, dplyr::envs::ns_dplyr);
}
}

SEXP dplyr_vec_unchop(SEXP chunks, SEXP rows, SEXP nrows,
  SEXP ptype) {
  SEXP result = PROTECT(vctrs::short_vec_init(ptype, Rf_asInteger(nrows)));

  R_xlen_t n = XLENGTH(rows);
  SEXP x_arg = PROTECT(Rf_mkString("x"));
  SEXP to_arg = PROTECT(Rf_mkString("to"));
  for (R_xlen_t i=0; i<n; i++) {
    SEXP rows_i = VECTOR_ELT(rows, i);
    int ni = vctrs::short_vec_size(rows_i);

    SEXP x = PROTECT(vctrs::vec_cast(VECTOR_ELT(chunks, i), ptype, x_arg, to_arg));
    if (vctrs::short_vec_size(x) == ni) {
      // x and rows_i are the same size
      vctrs::vec_assign_impl(result, rows_i, x, false);
    } else {
      // recycling `x` ni times
      SEXP rows_i_j = PROTECT(Rf_ScalarInteger(0));
      int* p_rows_i_j = INTEGER(rows_i_j);
      int* p_rows_i = INTEGER(rows_i);
      for (int j = 0; j < ni; ++j, ++p_rows_i) {
        *p_rows_i_j = *p_rows_i;
        vctrs::vec_assign_impl(result, rows_i_j, x, false);
      }
      UNPROTECT(1);
    }

    UNPROTECT(1);
  }

  // deal with names when sprinkling vectors
  if (!Rf_inherits(ptype, "data.frame")) {
    bool have_names = false;
    for (R_xlen_t i=0; i<n; i++) {
      SEXP x = VECTOR_ELT(chunks, i);
      if (Rf_getAttrib(x, R_NamesSymbol) != R_NilValue) {
        have_names = true;
        break;
      }
    }

    if (have_names) {
      // sprinkle the names
      R_xlen_t n_result = XLENGTH(result);
      SEXP result_names = PROTECT(Rf_allocVector(STRSXP, n_result));

      for (R_xlen_t i = 0; i < n; ++i) {
        SEXP names_i = Rf_getAttrib(VECTOR_ELT(chunks, i), R_NamesSymbol);
        if (names_i != R_NilValue) {
          vctrs::vec_assign_impl(result_names, VECTOR_ELT(rows, i), names_i, false);
        }
      }
      Rf_namesgets(result, result_names);
      UNPROTECT(1);
    }
  }

  UNPROTECT(3);
  return result;
}

SEXP dplyr_mask_eval_all_mutate(SEXP quo, SEXP env_private) {
  DPLYR_MASK_INIT();

  SEXP res = PROTECT(Rf_allocVector(VECSXP, 2));
  SEXP chunks = PROTECT(Rf_allocVector(VECSXP, ngroups));
  bool seen_vec = false;
  bool needs_recycle = false;
  bool seen_null = false;

  for (R_xlen_t i = 0; i < ngroups; i++) {
    DPLYR_MASK_SET_GROUP(i);
    R_xlen_t n_i = XLENGTH(VECTOR_ELT(rows, i));

    SEXP result_i = PROTECT(DPLYR_MASK_EVAL(quo));
    SET_VECTOR_ELT(chunks, i, result_i);

    if (Rf_isNull(result_i)) {
      seen_null = true;

      if (seen_vec) {
        dplyr::stop_mutate_mixed_NULL();
      }

    } else if (vctrs::vec_is_vector(result_i)) {
      seen_vec = true;

      if (vctrs::short_vec_size(result_i) != n_i) {
        needs_recycle = true;
      }

    } else {
      dplyr::stop_mutate_not_vector(result_i);
    }

    UNPROTECT(1);
  }

  if (seen_null && seen_vec) {
    // find out the first time the group was NULL so that the error will
    // be associated with this group
    for (int i = 0; i < ngroups; i++) {
      if (Rf_isNull(VECTOR_ELT(chunks, i))) {
        DPLYR_MASK_SET_GROUP(i);
        dplyr::stop_mutate_mixed_NULL();
      }
    }
  }

  // there was only NULL results
  if (ngroups > 0 && !seen_vec) {
    chunks = R_NilValue;
  }
  SET_VECTOR_ELT(res, 0, chunks);
  SET_VECTOR_ELT(res, 1, Rf_ScalarLogical(needs_recycle));

  UNPROTECT(2);
  DPLYR_MASK_FINALISE();

  return res;
}
