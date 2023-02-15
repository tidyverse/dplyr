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

void stop_summarise_incompatible_size(int index_group, int index_expression, int expected_size, int size) {
  DPLYR_ERROR_INIT(4);
    DPLYR_ERROR_SET(0, "group", Rf_ScalarInteger(index_group + 1));
    DPLYR_ERROR_SET(1, "index", Rf_ScalarInteger(index_expression + 1));
    DPLYR_ERROR_SET(2, "expected_size", Rf_ScalarInteger(expected_size));
    DPLYR_ERROR_SET(3, "size", Rf_ScalarInteger(size));
  DPLYR_ERROR_THROW("dplyr:::summarise_incompatible_size");
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

SEXP dplyr_summarise_recycle_chunks_in_place(SEXP list_of_chunks, SEXP list_of_result) {
  // - `list_of_chunks` will be modified in place by recycling each chunk
  //   to its common size as necessary.
  // - `list_of_result` will be modified in place if any chunks that originally
  //   created the result element were recycled, because the result won't be
  //   the right size anymore.
  // - Returns an integer vector of the common sizes.

  if (TYPEOF(list_of_chunks) != VECSXP) {
    Rf_errorcall(R_NilValue, "Internal error: `list_of_chunks` must be a list.");
  }
  if (TYPEOF(list_of_result) != VECSXP) {
    Rf_errorcall(R_NilValue, "Internal error: `list_of_result` must be a list.");
  }

  const R_xlen_t n_list_of_chunks = Rf_xlength(list_of_chunks);
  const SEXP* v_list_of_chunks = VECTOR_PTR_RO(list_of_chunks);

  if (n_list_of_chunks == 0) {
    // At least one set of chunks is required to proceed
    return dplyr::vectors::empty_int_vector;
  }

  SEXP first_chunks = v_list_of_chunks[0];
  const SEXP* v_first_chunks = VECTOR_PTR_RO(first_chunks);
  const R_xlen_t n_chunks = Rf_xlength(first_chunks);

  SEXP sizes = PROTECT(Rf_allocVector(INTSXP, n_chunks));
  int* v_sizes = INTEGER(sizes);

  // Initialize `sizes` with first set of chunks
  for (R_xlen_t i = 0; i < n_chunks; ++i) {
    v_sizes[i] = vctrs::short_vec_size(v_first_chunks[i]);
  }

  bool any_need_recycling = false;

  // Find common size across sets of chunks
  for (R_xlen_t i = 1; i < n_list_of_chunks; ++i) {
    SEXP chunks = v_list_of_chunks[i];
    const SEXP* v_chunks = VECTOR_PTR_RO(chunks);

    for (R_xlen_t j = 0; j < n_chunks; ++j) {
      SEXP chunk = v_chunks[j];

      const R_xlen_t out_size = v_sizes[j];
      const R_xlen_t elt_size = vctrs::short_vec_size(chunk);

      if (out_size == elt_size) {
        // v_sizes[j] is correct
      } else if (out_size == 1) {
        v_sizes[j] = elt_size;
        any_need_recycling = true;
      } else if (elt_size == 1) {
        // v_sizes[j] is correct
        any_need_recycling = true;
      } else {
        dplyr::stop_summarise_incompatible_size(j, i, out_size, elt_size);
      }
    }
  }

  if (!any_need_recycling) {
    UNPROTECT(1);
    return sizes;
  }

  // Actually recycle across chunks
  for (R_xlen_t i = 0; i < n_list_of_chunks; ++i) {
    SEXP chunks = v_list_of_chunks[i];
    const SEXP* v_chunks = VECTOR_PTR_RO(chunks);

    bool reset_result = false;

    for (R_xlen_t j = 0; j < n_chunks; ++j) {
      SEXP chunk = v_chunks[j];

      const R_xlen_t out_size = v_sizes[j];
      const R_xlen_t elt_size = vctrs::short_vec_size(chunk);

      if (out_size != elt_size) {
        // Recycle and modify `chunks` in place!
        chunk = vctrs::short_vec_recycle(chunk, out_size);
        SET_VECTOR_ELT(chunks, j, chunk);
        reset_result = true;
      }
    }

    if (reset_result) {
      // `list_of_result[[i]]` was created from `list_of_chunks[[i]]`,
      // but the chunks have been recycled so now the result is out of date.
      // It will be regenerated on the R side from the new chunks.
      SET_VECTOR_ELT(list_of_result, i, R_NilValue);
    }
  }

  UNPROTECT(1);
  return sizes;
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


