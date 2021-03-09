#include "dplyr.h"

SEXP new_environment(int size, SEXP parent)  {
  SEXP call = PROTECT(Rf_lang4(dplyr::symbols::new_env, Rf_ScalarLogical(TRUE), parent, Rf_ScalarInteger(size)));
  SEXP res = Rf_eval(call, R_BaseEnv);
  UNPROTECT(1);
  return res;
}

void dplyr_lazy_vec_chop_grouped(SEXP chops_env, SEXP rows, SEXP data, bool rowwise) {
  SEXP names = PROTECT(Rf_getAttrib(data, R_NamesSymbol));
  R_xlen_t n = XLENGTH(data);

  const SEXP* p_data = VECTOR_PTR_RO(data);
  const SEXP* p_names = STRING_PTR_RO(names);
  for (R_xlen_t i = 0; i < n; i++) {
    SEXP prom = PROTECT(Rf_allocSExp(PROMSXP));
    SET_PRENV(prom, R_EmptyEnv);
    SEXP column = p_data[i];
    if (rowwise && vctrs::vec_is_list(column) && Rf_length(column) > 0) {
      SET_PRCODE(prom, column);
    } else {
      SET_PRCODE(prom, Rf_lang3(dplyr::functions::vec_chop, column, rows));
    }
    SET_PRVALUE(prom, R_UnboundValue);

    Rf_defineVar(rlang::str_as_symbol(p_names[i]), prom, chops_env);
    UNPROTECT(1);
  }

  UNPROTECT(1);
}

void dplyr_lazy_vec_chop_ungrouped(SEXP chops_env, SEXP data) {
  SEXP names = PROTECT(Rf_getAttrib(data, R_NamesSymbol));
  R_xlen_t n = XLENGTH(data);

  const SEXP* p_data = VECTOR_PTR_RO(data);
  const SEXP* p_names = STRING_PTR_RO(names);
  for (R_xlen_t i = 0; i < n; i++) {
    SEXP prom = PROTECT(Rf_allocSExp(PROMSXP));
    SET_PRENV(prom, R_EmptyEnv);
    SET_PRCODE(prom, Rf_lang2(dplyr::functions::list, p_data[i]));
    SET_PRVALUE(prom, R_UnboundValue);

    SEXP symb = rlang::str_as_symbol(p_names[i]);
    Rf_defineVar(symb, prom, chops_env);
    UNPROTECT(1);
  }

  UNPROTECT(1);
}

SEXP dplyr_lazy_vec_chop(SEXP data, SEXP rows) {
  // a first environment to hide `.indices` and `.current_group`
  // this is for example used by funs::
  SEXP indices_env = PROTECT(new_environment(2, R_EmptyEnv));
  Rf_defineVar(dplyr::symbols::dot_indices, rows, indices_env);
  Rf_defineVar(dplyr::symbols::dot_current_group, Rf_ScalarInteger(0), indices_env);

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

void add_mask_binding(SEXP name, SEXP env_bindings, SEXP env_chops) {
  SEXP body = PROTECT(Rf_lang3(dplyr::functions::dot_subset2, name, dplyr::symbols::dot_current_group));
  SEXP fun  = PROTECT(Rf_lang3(dplyr::functions::function, R_NilValue, body));
  SEXP binding = PROTECT(Rf_eval(fun, env_chops));
  R_MakeActiveBinding(name, binding, env_bindings);

  UNPROTECT(3);
}

SEXP dplyr_data_masks_setup(SEXP env_chops, SEXP data, SEXP rows) {
  SEXP names = PROTECT(Rf_getAttrib(data, R_NamesSymbol));
  const SEXP* p_names = STRING_PTR_RO(names);
  R_xlen_t n_columns = XLENGTH(data);

  // create dynamic mask with one active binding per column
  R_xlen_t mask_size = XLENGTH(data) + 20;
  SEXP env_bindings = PROTECT(new_environment(mask_size, R_EmptyEnv));
  for (R_xlen_t i = 0; i < n_columns; i++) {
    SEXP name = PROTECT(rlang::str_as_symbol(p_names[i]));
    add_mask_binding(name, env_bindings, env_chops);
    UNPROTECT(1);
  }
  SEXP mask = PROTECT(rlang::new_data_mask(env_bindings, R_NilValue));
  SEXP pronoun = PROTECT(rlang::as_data_pronoun(env_bindings));
  Rf_defineVar(dplyr::symbols::dot_data, pronoun, mask);

  UNPROTECT(4);
  return mask;
}

SEXP env_resolved(SEXP env, SEXP names) {
  R_xlen_t n = XLENGTH(names);
  SEXP res = PROTECT(Rf_allocVector(LGLSXP, n));

  int* p_res = LOGICAL(res);
  const SEXP* p_names = STRING_PTR_RO(names);

  for(R_xlen_t i = 0; i < n; i++) {
    SEXP name = PROTECT(rlang::str_as_symbol(p_names[i]));
    SEXP prom = PROTECT(Rf_findVarInFrame(env, name));
    SEXP val = TYPEOF(prom) == PROMSXP ? PRVALUE(prom) : prom;
    p_res[i] = val != R_UnboundValue;
    UNPROTECT(2);
  }

  Rf_namesgets(res, names);
  UNPROTECT(1);
  return res;
}

