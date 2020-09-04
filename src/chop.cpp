#include "dplyr.h"

SEXP new_environment(int size)  {
  SEXP call = PROTECT(Rf_lang4(Rf_install("new.env"), Rf_ScalarLogical(TRUE), R_EmptyEnv, Rf_ScalarInteger(size)));
  SEXP res = Rf_eval(call, R_BaseEnv);
  UNPROTECT(1);
  return res;
}

void dplyr_lazy_vec_chop_grouped(SEXP e, SEXP data, bool rowwise) {
  SEXP names = PROTECT(Rf_getAttrib(data, R_NamesSymbol));
  SEXP groups_df = PROTECT(Rf_getAttrib(data, dplyr::symbols::groups));
  SEXP indices = VECTOR_ELT(groups_df, XLENGTH(groups_df) - 1);
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

    Rf_defineVar(Rf_installChar(STRING_ELT(names, i)), prom, e);
    UNPROTECT(1);
  }

  UNPROTECT(2);
}

void dplyr_lazy_vec_chop_ungrouped(SEXP e, SEXP data) {
  SEXP names = PROTECT(Rf_getAttrib(data, R_NamesSymbol));
  R_xlen_t n = XLENGTH(data);

  for (R_xlen_t i = 0; i < n; i++) {
    SEXP prom = PROTECT(Rf_allocSExp(PROMSXP));
    SET_PRENV(prom, R_EmptyEnv);
    SET_PRCODE(prom, Rf_lang2(dplyr::functions::list, VECTOR_ELT(data, i)));
    SET_PRVALUE(prom, R_UnboundValue);

    Rf_defineVar(Rf_installChar(STRING_ELT(names, i)), prom, e);
    UNPROTECT(1);
  }

  UNPROTECT(1);
}

SEXP dplyr_lazy_vec_chop(SEXP data) {
  SEXP e = PROTECT(new_environment(XLENGTH(data)));
  if (Rf_inherits(data, "grouped_df")) {
    dplyr_lazy_vec_chop_grouped(e, data, false);
  } else if (Rf_inherits(data, "rowwise_df")) {
    dplyr_lazy_vec_chop_grouped(e, data, true);
  } else {
    dplyr_lazy_vec_chop_ungrouped(e, data);
  }
  UNPROTECT(1);
  return e;
}

SEXP dplyr_data_masks_setup(SEXP chops, SEXP data) {
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
  for (R_xlen_t i = 0; i < n_groups; i++) {
    SET_VECTOR_ELT(masks, i, new_environment(mask_size));
  }

  for (R_xlen_t i = 0; i < n_columns; i++) {
    SEXP name = Rf_installChar(STRING_ELT(names, i));

    for (R_xlen_t j = 0; j < n_groups; j++) {
      SEXP prom = PROTECT(Rf_allocSExp(PROMSXP));
      SET_PRENV(prom, chops);
      SET_PRCODE(prom, Rf_lang3(dplyr::functions::dot_subset2, name, Rf_ScalarInteger(j + 1)));
      SET_PRVALUE(prom, R_UnboundValue);

      Rf_defineVar(name, prom, VECTOR_ELT(masks, j));
      UNPROTECT(1);
    }
  }

  UNPROTECT(2);
  return masks;
}
