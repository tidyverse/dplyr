#include "dplyr.h"

namespace dplyr {

void stop_summarise_unsupported_type(SEXP result) {
  DPLYR_ERROR_INIT(1);
    DPLYR_ERROR_SET(0, "result", result);
  DPLYR_ERROR_MESG_INIT(0);
  DPLYR_ERROR_THROW("dplyr:::summarise_unsupported_type");
}

void stop_summarise_mixed_null() {
  DPLYR_ERROR_INIT(0);
  DPLYR_ERROR_MESG_INIT(0);
  DPLYR_ERROR_THROW("dplyr:::summarise_mixed_null");
}

void stop_summarise_incompatible_size(int index_group, int index_expression, int expected_size, int size) {
  DPLYR_ERROR_INIT(4);
    DPLYR_ERROR_SET(0, "group", Rf_ScalarInteger(index_group + 1));
    DPLYR_ERROR_SET(1, "index", Rf_ScalarInteger(index_expression + 1));
    DPLYR_ERROR_SET(2, "expected_size", Rf_ScalarInteger(expected_size));
    DPLYR_ERROR_SET(3, "size", Rf_ScalarInteger(size));
  DPLYR_ERROR_MESG_INIT(0);
  DPLYR_ERROR_THROW("dplyr:::summarise_incompatible_size");
}

}


SEXP dplyr_mask_eval_all_summarise(SEXP quo, SEXP env_private) {
  DPLYR_MASK_INIT();

  R_xlen_t n_null = 0;
  SEXP chunks = PROTECT(Rf_allocVector(VECSXP, ngroups));
  for (R_xlen_t i = 0; i < ngroups; i++) {
    DPLYR_MASK_SET_GROUP(i);

    SEXP result_i = PROTECT(DPLYR_MASK_EVAL(quo));
    SET_VECTOR_ELT(chunks, i, result_i);

    if (result_i == R_NilValue) {
      n_null++;
    } else if (!vctrs::vec_is_vector(result_i)) {
      dplyr::stop_summarise_unsupported_type(result_i);
    }

    UNPROTECT(1);
  }
  DPLYR_MASK_FINALISE();
  UNPROTECT(1);

  if (n_null == ngroups) {
    return R_NilValue;
  } else if (n_null != 0) {
    dplyr::stop_summarise_mixed_null();
  }

  return chunks;
}

bool is_useful_chunk(SEXP ptype) {
  return !Rf_inherits(ptype, "data.frame") || XLENGTH(ptype) > 0;
}

SEXP dplyr_summarise_recycle_chunks(SEXP chunks, SEXP rows, SEXP ptypes) {
  R_len_t n_chunks = LENGTH(chunks);
  R_len_t n_groups = LENGTH(rows);

  SEXP res = PROTECT(Rf_allocVector(VECSXP, 2));
  Rf_namesgets(res, dplyr::vectors::names_summarise_recycle_chunks);
  SET_VECTOR_ELT(res, 0, chunks);

  SEXP useful = PROTECT(Rf_allocVector(LGLSXP, n_chunks));
  int* p_useful = LOGICAL(useful);
  int n_useful = 0;
  const SEXP* p_ptypes = VECTOR_PTR_RO(ptypes);
  for (R_len_t j = 0; j < n_chunks; j++) {
    n_useful += p_useful[j] = is_useful_chunk(p_ptypes[j]);
  }

  // early exit if there are no useful chunks, this includes
  // when there are no chunks at all
  if (n_useful == 0) {
    SET_VECTOR_ELT(res, 1, Rf_ScalarInteger(1));
    UNPROTECT(2);
    return res;
  }

  bool all_one = true;
  int k = 1;
  SEXP sizes = PROTECT(Rf_allocVector(INTSXP, n_groups));
  int* p_sizes = INTEGER(sizes);
  const SEXP* p_chunks = VECTOR_PTR_RO(chunks);
  for (R_xlen_t i = 0; i < n_groups; i++, ++p_sizes) {
    R_len_t n_i = 1;

    R_len_t j = 0;
    for (; j < n_chunks; j++) {
      // skip useless chunks before looking for chunk size
      for (; j < n_chunks && !p_useful[j]; j++);
      if (j == n_chunks) break;

      R_len_t n_i_j = vctrs::short_vec_size(VECTOR_ELT(p_chunks[j], i));

      if (n_i != n_i_j) {
        if (n_i == 1) {
          n_i = n_i_j;
        } else if (n_i_j != 1) {
          dplyr::stop_summarise_incompatible_size(i, j, n_i, n_i_j);
        }
      }
    }

    k = k + n_i;
    *p_sizes = n_i;
    if (n_i != 1) {
      all_one = false;
    }
  }

  if (all_one) {
    SET_VECTOR_ELT(res, 1, Rf_ScalarInteger(1));
  } else {
    // perform recycling
    for (int j = 0; j < n_chunks; j++){
      // skip useless chunks before recycling
      for (; j < n_chunks && !p_useful[j]; j++);
      if (j == n_chunks) break;

      SEXP chunks_j = p_chunks[j];
      int* p_sizes = INTEGER(sizes);
      for (int i = 0; i < n_groups; i++, ++p_sizes) {
        SET_VECTOR_ELT(chunks_j, i,
          vctrs::short_vec_recycle(VECTOR_ELT(chunks_j, i), *p_sizes)
        );
      }
    }
    SET_VECTOR_ELT(res, 0, chunks);
    SET_VECTOR_ELT(res, 1, sizes);
  }

  UNPROTECT(3);
  return res;
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
  UNPROTECT(1);
  return out;
}


