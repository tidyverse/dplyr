#include "dplyr.h"

namespace dplyr {

void stop_summarise_unsupported_type(SEXP result) {
  SEXP sym_stop_summarise_unsupported_type = Rf_install("stop_summarise_unsupported_type");
  SEXP call = PROTECT(Rf_lang2(sym_stop_summarise_unsupported_type, result));
  Rf_eval(call, dplyr::envs::ns_dplyr);

  // for rchk
  UNPROTECT(1);
}

void stop_summarise_incompatible_size(int index_group, int index_expression, int expected_size, int size) {
  SEXP s_size = PROTECT(Rf_ScalarInteger(size));
  SEXP s_expected_size = PROTECT(Rf_ScalarInteger(expected_size));
  SEXP s_index_group = PROTECT(Rf_ScalarInteger(index_group + 1));
  SEXP s_index_expression = PROTECT(Rf_ScalarInteger(index_expression + 1));
  SEXP sym_stop_summarise_incompatible_size = Rf_install("stop_summarise_incompatible_size");
  SEXP call = PROTECT(Rf_lang5(sym_stop_summarise_incompatible_size, s_index_group, s_index_expression, s_expected_size, s_size));
  Rf_eval(call, dplyr::envs::ns_dplyr);

  // for rchk
  UNPROTECT(5);
}

}


SEXP dplyr_mask_eval_all_summarise(SEXP quo, SEXP env_private) {
  DPLYR_MASK_INIT();

  SEXP chunks = PROTECT(Rf_allocVector(VECSXP, ngroups));
  for (R_xlen_t i = 0; i < ngroups; i++) {
    DPLYR_MASK_SET_GROUP(i);

    SEXP result_i = PROTECT(DPLYR_MASK_EVAL(quo));
    SET_VECTOR_ELT(chunks, i, result_i);

    if (!vctrs::vec_is_vector(result_i)) {
      dplyr::stop_summarise_unsupported_type(result_i);
    }

    UNPROTECT(1);
  }
  DPLYR_MASK_FINALISE();

  UNPROTECT(1);
  return chunks;
}

bool is_useful_chunk(SEXP ptype) {
  return !Rf_inherits(ptype, "data.frame") || XLENGTH(ptype) > 0;
}

SEXP dplyr_summarise_recycle_chunks(SEXP rows, SEXP results) {
  R_len_t n_chunks = LENGTH(results);
  R_len_t n_groups = LENGTH(rows);

  SEXP res = PROTECT(Rf_allocVector(VECSXP, 4));
  Rf_namesgets(res, dplyr::vectors::names_summarise_recycle_chunks);

  bool all_one = true;

  SEXP out_sizes = PROTECT(Rf_allocVector(INTSXP, n_groups));
  int* p_sizes = INTEGER(out_sizes);
  for (R_xlen_t i = 0; i < n_groups; i++, ++p_sizes) {
    R_len_t n_i = 1;

    R_len_t j = 0;
    for (; j < n_chunks; j++) {
      SEXP results_j = VECTOR_ELT(results, j);

      SEXP chunks_j = VECTOR_ELT(results_j, 0);
      SEXP sizes_j  = VECTOR_ELT(results_j, 3);

      // assume that the chunk size is 1
      R_len_t n_i_j = 1;

      if (sizes_j == R_NilValue) {
        // no size information, so get the size from chunks if the chunks come from
        // standard evaluation, otherwise it means 1
        if (chunks_j != R_NilValue && !Rf_inherits(chunks_j, "dplyr_lazy_vec_chop")) {
          n_i_j = vctrs::short_vec_size(VECTOR_ELT(chunks_j, i));
        }
      } else if (XLENGTH(sizes_j) == n_groups) {
        // get from the sizes[[j]] if correct size, otherwise, leave 1
        n_i_j = INTEGER(sizes_j)[i];
      }

      if (n_i != n_i_j) {
        if (n_i == 1) {
          n_i = n_i_j;
        } else if (n_i_j != 1) {
          dplyr::stop_summarise_incompatible_size(i, j, n_i, n_i_j);
        }
      }
    }

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
      SEXP results_j = VECTOR_ELT(results, j);

      SEXP chunks_j = VECTOR_ELT(results_j, 0);
      SEXP x_j      = VECTOR_ELT(results_j, 2);
      SEXP sizes_j  = VECTOR_ELT(results_j, 3);

      if (x_j != R_NilValue) {
        // we already have a result
        if (sizes_j == R_NilValue) {
          // this means chunks are size 1 and we need to recycle them
          SET_VECTOR_ELT(results_j, 2, vctrs::vec_rep_each(x_j, out_sizes));
        }

        // otherwise there are two possibilities:
        // - the sizes do match and so there is nothing to do
        // - they don't and this would have been stopped earlier by stop_summarise_incompatible_size()
      } else {
        // there were no results, so recycle the chunks
        if (chunks_j != R_NilValue) {
          SEXP new_chunks_j = PROTECT(Rf_allocVector(VECSXP, n_groups));
          int* p_sizes = INTEGER(out_sizes);
          for (int i = 0; i < n_groups; i++, ++p_sizes) {
            SET_VECTOR_ELT(new_chunks_j, i,
              vctrs::short_vec_recycle(VECTOR_ELT(chunks_j, i), *p_sizes)
            );
          }
          SET_VECTOR_ELT(results_j, 0, new_chunks_j);
          UNPROTECT(1);
        }
      }
    }
    SET_VECTOR_ELT(res, 1, out_sizes);
  }
  SET_VECTOR_ELT(res, 0, results);

  UNPROTECT(2);
  return res;
}
