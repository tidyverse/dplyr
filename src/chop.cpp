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
  Rf_defineVar(dplyr::symbols::dot_indices, indices_env, list_indices);

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
  R_xlen_t n_columns = XLENGTH(names);

  // create masks
  R_xlen_t mask_size = XLENGTH(data) + 20;
  SEXP masks = PROTECT(Rf_allocVector(VECSXP, n_groups));
  SEXP list_indices = Rf_findVarInFrame(ENCLOS(chops_env), dplyr::symbols::dot_indices);
  for (R_xlen_t i = 0; i < n_groups; i++) {
    SEXP mask_metadata_env = PROTECT(new_environment(2, R_EmptyEnv));
    Rf_defineVar(dplyr::symbols::dot_indices, VECTOR_ELT(list_indices, i), mask_metadata_env);
    Rf_defineVar(dplyr::symbols::current_group, Rf_ScalarInteger(i+1), mask_metadata_env);

    SET_VECTOR_ELT(masks, i, new_environment(mask_size, mask_metadata_env));
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

      // indices of this group
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

