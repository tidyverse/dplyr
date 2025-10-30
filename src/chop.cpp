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

    if (rowwise && vctrs::obj_is_list(column)) {
      if (Rf_length(column) == 0) {
        SEXP ptype = PROTECT(Rf_getAttrib(column, Rf_install("ptype")));
        column = PROTECT(Rf_allocVector(VECSXP, 1));
        if (ptype != R_NilValue) {
          SET_VECTOR_ELT(column, 0, ptype);
        } else {
          // i.e. `vec_ptype_finalise(unspecified())` (#6369)
          SET_VECTOR_ELT(column, 0, Rf_allocVector(LGLSXP, 0));
        }
        SET_PRCODE(prom, column);
        UNPROTECT(2);
      } else {
        SET_PRCODE(prom, column);
      }
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

SEXP dplyr_lazy_vec_chop(SEXP data,
                         SEXP rows,
                         SEXP env_current_group_info,
                         SEXP ffi_grouped,
                         SEXP ffi_rowwise) {
  bool grouped = static_cast<bool>(LOGICAL_ELT(ffi_grouped, 0));
  bool rowwise = static_cast<bool>(LOGICAL_ELT(ffi_rowwise, 0));

  // An environment to hold the chops of the columns.
  // Parent environment contains information about current group id
  // and current group size, for use in mask binding evaluation.
  SEXP env_chops = PROTECT(new_environment(XLENGTH(data), env_current_group_info));

  if (grouped) {
    dplyr_lazy_vec_chop_grouped(env_chops, rows, data, false);
  } else if (rowwise) {
    dplyr_lazy_vec_chop_grouped(env_chops, rows, data, true);
  } else {
    dplyr_lazy_vec_chop_ungrouped(env_chops, data);
  }

  UNPROTECT(1);
  return env_chops;
}

void add_mask_binding(SEXP name, SEXP env_mask_bindings, SEXP env_chops) {
  SEXP body = PROTECT(Rf_lang3(dplyr::functions::dot_subset2, name, dplyr::symbols::current_group_id));
  SEXP fun  = PROTECT(Rf_lang3(dplyr::functions::function, R_NilValue, body));
  SEXP binding = PROTECT(Rf_eval(fun, env_chops));
  R_MakeActiveBinding(name, binding, env_mask_bindings);

  UNPROTECT(3);
}

SEXP dplyr_make_mask_bindings(SEXP env_chops, SEXP data) {
  R_xlen_t n_columns = XLENGTH(data);

  SEXP names = PROTECT(Rf_getAttrib(data, R_NamesSymbol));
  const SEXP* p_names = STRING_PTR_RO(names);

  // Create environment with one active binding per column.
  // Leave some extra room for new columns added by `dplyr_mask_binding_add()`.
  R_xlen_t size = n_columns + 20;
  SEXP env_mask_bindings = PROTECT(new_environment(size, R_EmptyEnv));

  for (R_xlen_t i = 0; i < n_columns; i++) {
    SEXP name = PROTECT(rlang::str_as_symbol(p_names[i]));
    add_mask_binding(name, env_mask_bindings, env_chops);
    UNPROTECT(1);
  }

  UNPROTECT(2);
  return env_mask_bindings;
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

