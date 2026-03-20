#include "dplyr.h"
#include "utils.h"

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

  SEXP eval_env = R_EmptyEnv;

  for (R_xlen_t i = 0; i < n; i++) {
    SEXP column = p_data[i];
    SEXP sym = PROTECT(rlang::str_as_symbol(p_names[i]));

    if (rowwise && vctrs::obj_is_list(column)) {
      // Special handling of list-columns in rowwise mode
      if (Rf_xlength(column) == 0) {
        // List-col is empty, bind a length 1 list-col with either its `ptype`
        // attribute (for list-of) or a fallback to `logical()` (i.e.
        // `vec_ptype_finalise(unspecified())` as the sole element (#6369)).
        SEXP ptype = Rf_getAttrib(column, Rf_install("ptype"));
        if (ptype == R_NilValue) {
          ptype = Rf_allocVector(LGLSXP, 0);
        }
        PROTECT(ptype);

        column = PROTECT(Rf_allocVector(VECSXP, 1));
        SET_VECTOR_ELT(column, 0, ptype);

        env_bind_delayed(chops_env, sym, column, eval_env);
        UNPROTECT(2);
      } else {
        // List-col has length, bind it directly
        env_bind_delayed(chops_env, sym, column, eval_env);
      }
    } else {
      // Standard grouped case is to bind a delayed call to `vec_chop(column, rows)`
      SEXP expr = PROTECT(Rf_lang3(dplyr::functions::vec_chop, column, rows));
      env_bind_delayed(chops_env, sym, expr, eval_env);
      UNPROTECT(1);
    }

    UNPROTECT(1);
  }

  UNPROTECT(1);
}

void dplyr_lazy_vec_chop_ungrouped(SEXP chops_env, SEXP data) {
  SEXP names = PROTECT(Rf_getAttrib(data, R_NamesSymbol));
  R_xlen_t n = XLENGTH(data);

  const SEXP* p_data = VECTOR_PTR_RO(data);
  const SEXP* p_names = STRING_PTR_RO(names);

  SEXP eval_env = R_EmptyEnv;

  // Bind a delayed call to `list(column)`, i.e. 1 group containing the whole column
  for (R_xlen_t i = 0; i < n; i++) {
    SEXP column = p_data[i];
    SEXP sym = PROTECT(rlang::str_as_symbol(p_names[i]));
    SEXP expr = PROTECT(Rf_lang2(dplyr::functions::list, column));
    env_bind_delayed(chops_env, sym, expr, eval_env);
    UNPROTECT(2);
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

static inline
bool env_binding_is_delayed(SEXP env, SEXP sym) {
#if (R_VERSION >= R_Version(4, 6, 0))
  R_BindingType_t type = R_GetBindingType(sym, env);
  if (type == R_BindingTypeUnbound) {
    Rf_errorcall(R_NilValue, "Internal error: Unbound chops binding.");
  }
  return type == R_BindingTypeDelayed;
#else
  SEXP value = Rf_findVarInFrame3(env, sym, FALSE);
  if (value == R_UnboundValue) {
    Rf_errorcall(R_NilValue, "Internal error: Unbound chops binding.");
  }
  return TYPEOF(value) == PROMSXP && PRVALUE(value) == R_UnboundValue;
#endif
}

// Detect "used" chops bindings
//
// We first require that the binding exists, otherwise that would
// be an internal error on our part.
//
// Then we check to see if the binding is delayed. If it is, then it is
// considered "unused".
//
// Used bindings include:
// - Forced chop promises (i.e. if the user referenced an original column in an expression)
// - Newly installed columns from a previous expression
SEXP ffi_env_bindings_are_used(SEXP chops_env, SEXP names) {
  R_xlen_t n = XLENGTH(names);
  SEXP res = PROTECT(Rf_allocVector(LGLSXP, n));

  int* p_res = LOGICAL(res);
  const SEXP* p_names = STRING_PTR_RO(names);

  for (R_xlen_t i = 0; i < n; i++) {
    SEXP sym = PROTECT(rlang::str_as_symbol(p_names[i]));
    p_res[i] = !env_binding_is_delayed(chops_env, sym);
    UNPROTECT(1);
  }

  Rf_namesgets(res, names);
  UNPROTECT(1);
  return res;
}
