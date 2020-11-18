#include "dplyr.h"

SEXP new_environment(int size, SEXP parent)  {
  SEXP call = PROTECT(Rf_lang4(Rf_install("new.env"), Rf_ScalarLogical(TRUE), parent, Rf_ScalarInteger(size)));
  SEXP res = Rf_eval(call, R_BaseEnv);
  UNPROTECT(1);
  return res;
}

void dplyr_lazy_vec_chop_grouped(SEXP chops_env, SEXP rows, SEXP data, bool rowwise) {
  SEXP names = PROTECT(Rf_getAttrib(data, R_NamesSymbol));
  R_xlen_t n = XLENGTH(data);

  for (R_xlen_t i = 0; i < n; i++) {
    SEXP prom = PROTECT(Rf_allocSExp(PROMSXP));
    SET_PRENV(prom, R_EmptyEnv);
    SEXP column = VECTOR_ELT(data, i);
    if (rowwise && vctrs::vec_is_list(column)) {
      SET_PRCODE(prom, column);
    } else {
      SET_PRCODE(prom, Rf_lang3(dplyr::functions::vec_chop, column, rows));
    }
    SET_PRVALUE(prom, R_UnboundValue);

    Rf_defineVar(Rf_installChar(STRING_ELT(names, i)), prom, chops_env);
    UNPROTECT(1);
  }

  UNPROTECT(1);
}

void dplyr_lazy_vec_chop_ungrouped(SEXP chops_env, SEXP data) {
  SEXP names = PROTECT(Rf_getAttrib(data, R_NamesSymbol));
  R_xlen_t n = XLENGTH(data);

  for (R_xlen_t i = 0; i < n; i++) {
    SEXP prom = PROTECT(Rf_allocSExp(PROMSXP));
    SET_PRENV(prom, R_EmptyEnv);
    SET_PRCODE(prom, Rf_lang2(dplyr::functions::list, VECTOR_ELT(data, i)));
    SET_PRVALUE(prom, R_UnboundValue);

    Rf_defineVar(Rf_installChar(STRING_ELT(names, i)), prom, chops_env);
    UNPROTECT(1);
  }

  UNPROTECT(1);
}

SEXP dplyr_lazy_vec_chop(SEXP data, SEXP rows) {
  // a first environment to hide `.indices`
  // this is for example used by funs::
  SEXP indices_env = PROTECT(new_environment(1, R_EmptyEnv));
  Rf_defineVar(dplyr::symbols::dot_indices, rows, indices_env);

  // then an environment to hold the chops of the columns
  SEXP chops_env = PROTECT(new_environment(XLENGTH(data), indices_env));
  if (Rf_inherits(data, "grouped_df")) {
    dplyr_lazy_vec_chop_grouped(chops_env, rows, data, false);
  } else if (Rf_inherits(data, "rowwise_df")) {
    dplyr_lazy_vec_chop_grouped(chops_env, rows, data, true);
  } else {
    dplyr_lazy_vec_chop_ungrouped(chops_env, data);
  }
  UNPROTECT(2);
  return chops_env;
}

SEXP dplyr_data_masks_setup(SEXP chops_env, SEXP data, SEXP rows) {
  SEXP names = PROTECT(Rf_getAttrib(data, R_NamesSymbol));

  R_xlen_t n_groups = XLENGTH(rows);
  R_xlen_t n_columns = XLENGTH(data);

  // create masks
  R_xlen_t mask_size = XLENGTH(data) + 20;
  SEXP masks = PROTECT(Rf_allocVector(VECSXP, n_groups));
  for (R_xlen_t i = 0; i < n_groups; i++) {
    SET_VECTOR_ELT(masks, i, new_environment(mask_size, R_EmptyEnv));
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

  SEXP new_data_mask_call = PROTECT(Rf_lang2(Rf_install("new_data_mask"), R_NilValue));
  SEXP as_data_pronoun_call = PROTECT(Rf_lang2(Rf_install("as_data_pronoun"), R_NilValue));

  for (R_xlen_t i = 0; i < n_groups; i++) {
    SETCAR(CDR(new_data_mask_call), VECTOR_ELT(masks, i));
    SEXP mask = PROTECT(Rf_eval(new_data_mask_call, dplyr::envs::ns_rlang));

    SETCAR(CDR(as_data_pronoun_call), mask);
    SEXP pronoun = PROTECT(Rf_eval(as_data_pronoun_call, dplyr::envs::ns_rlang));
    Rf_defineVar(Rf_install(".data"), pronoun, mask);

    SET_VECTOR_ELT(masks, i, mask);
    UNPROTECT(2);
  }

  UNPROTECT(4);
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

