#include "dplyr.h"

namespace dplyr {

void stop_summarise_unsupported_type(SEXP result) {
  DPLYR_ERROR_INIT(1);
    DPLYR_ERROR_SET(0, "result", result);
  DPLYR_ERROR_THROW("dplyr:::summarise_unsupported_type");
}

void stop_summarise_mixed_null() {
  DPLYR_ERROR_INIT(0);
  DPLYR_ERROR_THROW("dplyr:::summarise_mixed_null");
}

void stop_summarise_incompatible_size(
  int index_expression,
  int index_group,
  int actual_size
) {
  DPLYR_ERROR_INIT(3);
  DPLYR_ERROR_SET(0, "index_expression", Rf_ScalarInteger(index_expression + 1));
  DPLYR_ERROR_SET(1, "index_group", Rf_ScalarInteger(index_group + 1));
  DPLYR_ERROR_SET(2, "actual_size", Rf_ScalarInteger(actual_size));
  DPLYR_ERROR_THROW("dplyr:::summarise_incompatible_size");
}

void stop_reframe_incompatible_size(
  int index_expression,
  int index_group,
  int actual_size,
  int expected_size
) {
  DPLYR_ERROR_INIT(4);
  DPLYR_ERROR_SET(0, "index_expression", Rf_ScalarInteger(index_expression + 1));
  DPLYR_ERROR_SET(1, "index_group", Rf_ScalarInteger(index_group + 1));
  DPLYR_ERROR_SET(2, "actual_size", Rf_ScalarInteger(actual_size));
  DPLYR_ERROR_SET(3, "expected_size", Rf_ScalarInteger(expected_size));
  DPLYR_ERROR_THROW("dplyr:::reframe_incompatible_size");
}

}


SEXP dplyr_mask_eval_all_summarise(SEXP quo, SEXP env_private) {
  DPLYR_MASK_INIT();

  R_xlen_t n_null = 0;
  SEXP chunks = PROTECT(Rf_allocVector(VECSXP, ngroups));
  for (R_xlen_t i = 0; i < ngroups; i++) {
    DPLYR_MASK_ITERATION_INIT();
    DPLYR_MASK_SET_GROUP(i);

    SEXP result_i = PROTECT(DPLYR_MASK_EVAL(quo));
    SET_VECTOR_ELT(chunks, i, result_i);

    if (result_i == R_NilValue) {
      n_null++;
    } else if (!vctrs::obj_is_vector(result_i)) {
      dplyr::stop_summarise_unsupported_type(result_i);
    }

    UNPROTECT(1);
    DPLYR_MASK_ITERATION_FINALISE();
  }
  DPLYR_MASK_FINALISE();
  UNPROTECT(1);

  if (n_null == ngroups) {
    return R_NilValue;
  } else if (n_null != 0) {
    const SEXP* v_chunks = VECTOR_PTR_RO(chunks);

    for (R_xlen_t i = 0; i < ngroups; i++) {
      if (v_chunks[i] == R_NilValue) {
        // Find out the first time the group was `NULL`
        // so that the error will be associated with this group
        DPLYR_MASK_SET_GROUP(i);
        dplyr::stop_summarise_mixed_null();
      }
    }
  }

  return chunks;
}

SEXP dplyr_summarise_check_all_size_one(
  SEXP result_per_group_per_expression,
  SEXP s_n_groups
) {
  if (TYPEOF(result_per_group_per_expression) != VECSXP) {
    Rf_errorcall(R_NilValue, "Internal error: `result_per_group_per_expression` must be a list.");
  }

  const R_xlen_t n_groups = (R_xlen_t) INTEGER_ELT(s_n_groups, 0);

  const R_xlen_t n_expressions = Rf_xlength(result_per_group_per_expression);
  const SEXP* v_result_per_group_per_expression = VECTOR_PTR_RO(result_per_group_per_expression);

  for (R_xlen_t i = 0; i < n_expressions; ++i) {
    // This expression's results across all groups
    SEXP result_per_group = v_result_per_group_per_expression[i];
    const SEXP* v_result_per_group = VECTOR_PTR_RO(result_per_group);

    for (R_xlen_t j = 0; j < n_groups; ++j) {
      // This group's result for this expression
      SEXP result = v_result_per_group[j];
      const R_xlen_t result_size = vctrs::short_vec_size(result);

      if (result_size != 1) {
        dplyr::stop_summarise_incompatible_size(i, j, result_size);
      }
    }
  }

  return R_NilValue;
}

SEXP dplyr_reframe_recycle_horizontally_in_place(
  SEXP result_per_group_per_expression,
  SEXP result_per_expression,
  SEXP s_n_groups
) {
  // - `result_per_group_per_expression` will be modified in place by recycling
  //   each result to its common size for that group across expressions as
  //   necessary.
  // - `result_per_expression` will be modified in place if any pieces of
  //   `result_per_group_per_expression` that originally created the result
  //   element were recycled, because the result won't be the right size
  //   anymore.
  // - Returns an integer vector of the common size of each group, which we
  //   use to `vec_rep_each()` the group keys later on.

  if (TYPEOF(result_per_group_per_expression) != VECSXP) {
    Rf_errorcall(R_NilValue, "Internal error: `result_per_group_per_expression` must be a list.");
  }
  if (TYPEOF(result_per_expression) != VECSXP) {
    Rf_errorcall(R_NilValue, "Internal error: `result_per_expression` must be a list.");
  }

  const R_xlen_t n_groups = (R_xlen_t) INTEGER_ELT(s_n_groups, 0);

  SEXP group_sizes = PROTECT(Rf_allocVector(INTSXP, n_groups));
  int* v_group_sizes = INTEGER(group_sizes);

  // Initialize `group_sizes` with 1. Recyclable to any other size, and needed as the
  // default when there are no expressions to evaluate, or all expressions
  // evaluate to data frames with zero columns.
  for (R_xlen_t i = 0; i < n_groups; ++i) {
    v_group_sizes[i] = 1;
  }

  const R_xlen_t n_expressions = Rf_xlength(result_per_group_per_expression);
  const SEXP* v_result_per_group_per_expression = VECTOR_PTR_RO(result_per_group_per_expression);

  // Find common size of each group
  for (R_xlen_t i = 0; i < n_expressions; ++i) {
    SEXP result_per_group = v_result_per_group_per_expression[i];
    const SEXP* v_result_per_group = VECTOR_PTR_RO(result_per_group);

    for (R_xlen_t j = 0; j < n_groups; ++j) {
      SEXP result = v_result_per_group[j];

      const R_xlen_t out_size = v_group_sizes[j];
      const R_xlen_t result_size = vctrs::short_vec_size(result);

      if (out_size == result_size) {
        // v_group_sizes[j] is correct
      } else if (out_size == 1) {
        v_group_sizes[j] = result_size;
      } else if (result_size == 1) {
        // v_group_sizes[j] is correct
      } else {
        dplyr::stop_reframe_incompatible_size(i, j, result_size, out_size);
      }
    }
  }

  // Actually recycle each result to its group's common size
  for (R_xlen_t i = 0; i < n_expressions; ++i) {
    SEXP result_per_group = v_result_per_group_per_expression[i];
    const SEXP* v_result_per_group = VECTOR_PTR_RO(result_per_group);

    bool reset_result_for_this_expression = false;

    for (R_xlen_t j = 0; j < n_groups; ++j) {
      SEXP result = v_result_per_group[j];

      const R_xlen_t out_size = v_group_sizes[j];
      const R_xlen_t result_size = vctrs::short_vec_size(result);

      if (out_size != result_size) {
        // Recycle and modify in place!
        result = vctrs::short_vec_recycle(result, out_size);
        SET_VECTOR_ELT(result_per_group, j, result);
        reset_result_for_this_expression = true;
      }
    }

    if (reset_result_for_this_expression) {
      // `result_per_expression[[i]]` was created from
      // `result_per_group_per_expression`, but an individual `result` has been
      // recycled so now `result_per_expression[[i]]` is out of date. It will be
      // regenerated on the R side from the new
      // `result_per_group_per_expression` values.
      SET_VECTOR_ELT(result_per_expression, i, R_NilValue);
    }
  }

  UNPROTECT(1);
  return group_sizes;
}

SEXP dplyr_extract_chunks(SEXP df_list, SEXP df_ptype) {
  R_xlen_t n_columns = XLENGTH(df_ptype);
  R_xlen_t n_rows = XLENGTH(df_list);

  const SEXP* p_df_list = VECTOR_PTR_RO(df_list);

  SEXP out = PROTECT(Rf_allocVector(VECSXP, n_columns));
  for (R_xlen_t i = 0; i < n_columns; i++) {
    SEXP out_i = PROTECT(Rf_allocVector(VECSXP, n_rows));
    for (R_xlen_t j = 0; j < n_rows; j++) {
      SET_VECTOR_ELT(out_i, j, VECTOR_ELT(p_df_list[j], i));
    }
    SET_VECTOR_ELT(out, i, out_i);
    UNPROTECT(1);
  }
  Rf_namesgets(out, Rf_getAttrib(df_ptype, R_NamesSymbol));
  UNPROTECT(1);
  return out;
}
