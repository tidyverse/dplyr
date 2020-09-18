#include "dplyr.h"

SEXP new_environment(int size, SEXP parent)  {
  SEXP call = PROTECT(Rf_lang4(Rf_install("new.env"), Rf_ScalarLogical(TRUE), parent, Rf_ScalarInteger(size)));
  SEXP res = Rf_eval(call, R_BaseEnv);
  UNPROTECT(1);
  return res;
}

void dplyr_lazy_vec_chop_grouped(SEXP chops_env, SEXP indices_env, SEXP data, bool rowwise) {
  SEXP names = PROTECT(Rf_getAttrib(data, R_NamesSymbol));
  SEXP groups_df = PROTECT(Rf_getAttrib(data, dplyr::symbols::groups));
  SEXP indices = VECTOR_ELT(groups_df, XLENGTH(groups_df) - 1);
  Rf_defineVar(dplyr::symbols::dot_indices, indices, indices_env);
  R_xlen_t n = XLENGTH(data);

  for (R_xlen_t i = 0; i < n; i++) {
    SEXP prom = PROTECT(Rf_allocSExp(PROMSXP));
    SET_PRENV(prom, R_EmptyEnv);
    SEXP column = VECTOR_ELT(data, i);
    if (rowwise && vctrs::vec_is_list(column)) {
      SET_PRCODE(prom, column);
    } else {
      SET_PRCODE(prom, Rf_lang3(dplyr::functions::vec_chop, column, indices));
    }
    SET_PRVALUE(prom, R_UnboundValue);

    Rf_defineVar(Rf_installChar(STRING_ELT(names, i)), prom, chops_env);
    UNPROTECT(1);
  }

  UNPROTECT(2);
}

void dplyr_lazy_vec_chop_ungrouped(SEXP chops_env, SEXP indices_env, SEXP data) {
  SEXP names = PROTECT(Rf_getAttrib(data, R_NamesSymbol));
  R_xlen_t n = XLENGTH(data);

  // list(1:nrow(data))
  SEXP call = PROTECT(Rf_lang3(dplyr::symbols::colon, Rf_ScalarInteger(1), Rf_ScalarInteger(vctrs::short_vec_size(data))));
  SEXP list_indices = PROTECT(Rf_allocVector(VECSXP, 1));
  SET_VECTOR_ELT(list_indices, 0, Rf_eval(call, R_BaseEnv));

  Rf_defineVar(dplyr::symbols::dot_indices, list_indices, indices_env);

  for (R_xlen_t i = 0; i < n; i++) {
    SEXP prom = PROTECT(Rf_allocSExp(PROMSXP));
    SET_PRENV(prom, R_EmptyEnv);
    SET_PRCODE(prom, Rf_lang2(dplyr::functions::list, VECTOR_ELT(data, i)));
    SET_PRVALUE(prom, R_UnboundValue);

    Rf_defineVar(Rf_installChar(STRING_ELT(names, i)), prom, chops_env);
    UNPROTECT(1);
  }

  UNPROTECT(3);
}

SEXP dplyr_lazy_vec_chop(SEXP data, SEXP caller_env) {
  SEXP indices_env = PROTECT(new_environment(1, caller_env));
  SEXP chops_env = PROTECT(new_environment(XLENGTH(data), indices_env));
  if (Rf_inherits(data, "grouped_df")) {
    dplyr_lazy_vec_chop_grouped(chops_env, indices_env, data, false);
  } else if (Rf_inherits(data, "rowwise_df")) {
    dplyr_lazy_vec_chop_grouped(chops_env, indices_env, data, true);
  } else {
    dplyr_lazy_vec_chop_ungrouped(chops_env, indices_env, data);
  }
  UNPROTECT(2);
  return chops_env;
}

SEXP dplyr_data_masks_setup(SEXP chops_env, SEXP data) {
  SEXP names = PROTECT(Rf_getAttrib(data, R_NamesSymbol));

  R_xlen_t n_groups = 1;
  if (Rf_inherits(data, "grouped_df")) {
    SEXP groups_df = PROTECT(Rf_getAttrib(data, dplyr::symbols::groups));
    SEXP indices = VECTOR_ELT(groups_df, XLENGTH(groups_df) - 1);

    n_groups = XLENGTH(indices);
    UNPROTECT(1);
  } else if (Rf_inherits(data, "rowwise_df")) {
    n_groups = vctrs::short_vec_size(data);
  }
  R_xlen_t n_columns = XLENGTH(data);

  // create masks
  R_xlen_t mask_size = n_columns + 20;
  SEXP masks = PROTECT(Rf_allocVector(VECSXP, n_groups));
  SEXP list_indices = Rf_findVarInFrame(ENCLOS(chops_env), dplyr::symbols::dot_indices);
  for (R_xlen_t i = 0; i < n_groups; i++) {
    SEXP mask_metadata_env = PROTECT(new_environment(2, R_EmptyEnv));
    Rf_defineVar(dplyr::symbols::dot_indices, VECTOR_ELT(list_indices, i), mask_metadata_env);
    Rf_defineVar(dplyr::symbols::current_group, Rf_ScalarInteger(i+1), mask_metadata_env);

    SET_VECTOR_ELT(masks, i, new_environment(mask_size, mask_metadata_env));
    UNPROTECT(1);
  }

  for (R_xlen_t i = 0; i < n_columns; i++) {
    SEXP name = Rf_installChar(STRING_ELT(names, i));

    for (R_xlen_t j = 0; j < n_groups; j++) {
      // promise of the slice for column {name} and group {j}
      SEXP prom = PROTECT(Rf_allocSExp(PROMSXP));
      SET_PRENV(prom, chops_env);
      SET_PRCODE(prom, Rf_lang3(dplyr::functions::dot_subset2, name, Rf_ScalarInteger(j + 1)));
      SET_PRVALUE(prom, R_UnboundValue);

      Rf_defineVar(name, prom, VECTOR_ELT(masks, j));
      UNPROTECT(1);
    }
  }

  UNPROTECT(2);
  return masks;
}

SEXP env_resolved(SEXP env, SEXP names) {
  R_xlen_t n = XLENGTH(names);
  SEXP res = PROTECT(Rf_allocVector(LGLSXP, n));

  int* p_res = LOGICAL(res);
  for(R_xlen_t i = 0; i < n; i++) {
    SEXP prom = Rf_findVarInFrame(env, Rf_installChar(STRING_ELT(names, i)));
    p_res[i] = PRVALUE(prom) != R_UnboundValue;
  }

  Rf_namesgets(res, names);
  UNPROTECT(1);
  return res;
}

namespace funs {

SEXP eval_hybrid(SEXP quo, SEXP chops) {
  SEXP call = PROTECT(Rf_lang3(dplyr::functions::eval_hybrid, quo, chops));
  SEXP res = PROTECT(Rf_eval(call, R_BaseEnv));
  UNPROTECT(2);

  return res;
}

}

SEXP dplyr_eval_tidy_all(SEXP quosures, SEXP chops, SEXP masks, SEXP caller_env, SEXP auto_names, SEXP private_env) {
  R_xlen_t n_expr = XLENGTH(quosures);
  SEXP names = PROTECT(Rf_getAttrib(quosures, R_NamesSymbol));
  if (names == R_NilValue) {
    UNPROTECT(1);

    names = PROTECT(Rf_allocVector(STRSXP, n_expr));
  }

  R_xlen_t n_masks = XLENGTH(masks);

  // initialize all results
  SEXP res = PROTECT(Rf_allocVector(VECSXP, n_masks));
  for (R_xlen_t i = 0; i < n_masks; i++) {
    SEXP res_i = PROTECT(Rf_allocVector(VECSXP, n_expr));
    Rf_namesgets(res_i, names);
    SET_VECTOR_ELT(res, i, res_i);
    UNPROTECT(1);
  }

  SEXP index_expression = Rf_findVarInFrame(private_env, dplyr::symbols::current_expression);
  int *p_index_expression = INTEGER(index_expression);

  SEXP index_group = Rf_findVarInFrame(private_env, dplyr::symbols::current_group);
  int* p_index_group = INTEGER(index_group);

  // eval all the things
  for (R_xlen_t i_expr = 0; i_expr < n_expr; i_expr++) {
    *p_index_expression = i_expr + 1;

    SEXP quo = VECTOR_ELT(quosures, i_expr);

    SEXP name = STRING_ELT(names, i_expr);
    SEXP auto_name = STRING_ELT(auto_names, i_expr);

    *p_index_group = -1;
    SEXP hybrid_result = PROTECT(funs::eval_hybrid(quo, chops));
    if (hybrid_result != R_NilValue) {

      if (TYPEOF(hybrid_result) != VECSXP || XLENGTH(hybrid_result) != n_masks) {
        Rf_error("Malformed hybrid result, not a list");
      }

      SEXP ptype = Rf_getAttrib(hybrid_result, dplyr::symbols::ptype);
      if (ptype == R_NilValue) {
        Rf_error("Malformed hybrid result, needs ptype");
      }

      if (XLENGTH(name) == 0) {
        // if @ptype is a data frame, then auto splice as we go
        // this assumes all results exactly match the ptype
        if (Rf_inherits(ptype, "data.frame")) {
          R_xlen_t n_results = XLENGTH(ptype);

          SEXP result_names = Rf_getAttrib(ptype, R_NamesSymbol);
          SEXP result_symbols = Rf_allocVector(VECSXP, n_results);

          // only install once
          for (R_xlen_t i_result = 0; i_result < n_results; i_result++) {
            SET_VECTOR_ELT(result_symbols, i_result, Rf_installChar(STRING_ELT(result_names, i_result)));
          }

          for (R_xlen_t i_group = 0; i_group < n_masks; i_group++) {
            SEXP res_i = VECTOR_ELT(hybrid_result, i_group);
            SET_VECTOR_ELT(VECTOR_ELT(res, i_group), i_expr, res_i);
            SEXP mask = VECTOR_ELT(masks, i_group);

            for (R_xlen_t i_result = 0; i_result < n_results; i_result++) {
              Rf_defineVar(
                VECTOR_ELT(result_symbols, i_result),
                VECTOR_ELT(hybrid_result, i_result),
                mask
              );
            }
          }

        } else {
          // unnamed, but not a data frame, so use the deduced name
          SEXP s_auto_name = Rf_installChar(auto_name);

          for (R_xlen_t i_group = 0; i_group < n_masks; i_group++) {
            SEXP hybrid_res_i = VECTOR_ELT(hybrid_result, i_group);

            SEXP res_i = VECTOR_ELT(res, i_group);
            SET_VECTOR_ELT(res_i, i_expr, hybrid_res_i);

            SEXP names_res_i = Rf_getAttrib(res_i, R_NamesSymbol);
            SET_STRING_ELT(names_res_i, i_expr, auto_name);

            Rf_defineVar(s_auto_name, hybrid_res_i, VECTOR_ELT(masks, i_group));
          }
        }

      } else {
        SEXP s_name = Rf_installChar(name);

        // we have a proper name, so no auto splice or auto name use
        for (R_xlen_t i_group = 0; i_group < n_masks; i_group++) {
          SEXP res_i = VECTOR_ELT(hybrid_result, i_group);
          SET_VECTOR_ELT(VECTOR_ELT(res, i_group), i_expr, res_i);
          Rf_defineVar(s_name, res_i, VECTOR_ELT(masks, i_group));
        }
      }

    } else {
      for (R_xlen_t i_group = 0; i_group < n_masks; i_group++) {
        *p_index_group = i_group + 1;
        SEXP mask = VECTOR_ELT(masks, i_group);

        SEXP result = PROTECT(rlang::eval_tidy(quo, mask, caller_env));

        SET_VECTOR_ELT(VECTOR_ELT(res, i_group), i_expr, result);

        if (XLENGTH(name) == 0) {
          if (Rf_inherits(result, "data.frame")) {
            R_xlen_t n_columns = XLENGTH(result);
            SEXP names_columns = PROTECT(Rf_getAttrib(result, R_NamesSymbol));
            for (R_xlen_t i_column = 0; i_column < n_columns; i_column++) {
              SEXP name_i = Rf_installChar(STRING_ELT(names_columns, i_column));
              Rf_defineVar(name_i, VECTOR_ELT(result, i_column), mask);
            }
            UNPROTECT(1);
          } else {
            SEXP s_auto_name = Rf_installChar(auto_name);

            // this uses an auto name instead of ""
            SEXP names_res_i = Rf_getAttrib(VECTOR_ELT(res, i_group), R_NamesSymbol);
            SET_STRING_ELT(names_res_i, i_expr, auto_name);

            Rf_defineVar(s_auto_name, result, mask);
          }
        } else {
          Rf_defineVar(Rf_installChar(name), result, mask);
        }

        UNPROTECT(1);
      }
    }
    UNPROTECT(1);
  }

  UNPROTECT(2);
  return res;
}
