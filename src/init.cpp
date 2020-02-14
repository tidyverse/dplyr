#include "dplyr.h"

namespace dplyr {

SEXP envs::ns_dplyr = NULL;

SEXP get_classes_vctrs_list_of() {
  SEXP klasses = Rf_allocVector(STRSXP, 3);
  R_PreserveObject(klasses);
  SET_STRING_ELT(klasses, 0, Rf_mkChar("vctrs_list_of"));
  SET_STRING_ELT(klasses, 1, Rf_mkChar("vctrs_vctr"));
  SET_STRING_ELT(klasses, 2, Rf_mkChar("list"));
  return klasses;
}

SEXP get_classes_tbl_df() {
  SEXP klasses = Rf_allocVector(STRSXP, 3);
  R_PreserveObject(klasses);
  SET_STRING_ELT(klasses, 0, Rf_mkChar("tbl_df"));
  SET_STRING_ELT(klasses, 1, Rf_mkChar("tbl"));
  SET_STRING_ELT(klasses, 2, Rf_mkChar("data.frame"));
  return klasses;
}

SEXP get_empty_int_vector() {
  SEXP x = Rf_allocVector(INTSXP, 0);
  R_PreserveObject(x);
  return x;
}

SEXP symbols::ptype = Rf_install("ptype");
SEXP symbols::levels = Rf_install("levels");
SEXP symbols::groups = Rf_install("groups");
SEXP symbols::vars = Rf_install("vars");
SEXP symbols::current_group = Rf_install("current_group");
SEXP symbols::current_expression = Rf_install("current_expression");
SEXP symbols::rows = Rf_install("rows");
SEXP symbols::mask = Rf_install("mask");
SEXP symbols::caller = Rf_install("caller");
SEXP symbols::resolved = Rf_install("resolved");
SEXP symbols::bindings = Rf_install("bindings");
SEXP symbols::which_used = Rf_install("which_used");
SEXP symbols::dot_drop = Rf_install(".drop");

SEXP vectors::classes_vctrs_list_of = get_classes_vctrs_list_of();
SEXP vectors::classes_tbl_df = get_classes_tbl_df();
SEXP vectors::empty_int_vector = get_empty_int_vector();

} // dplyr

SEXP dplyr_init_library(SEXP ns) {
  dplyr::envs::ns_dplyr = ns;
  return R_NilValue;
}

static const R_CallMethodDef CallEntries[] = {
  {"dplyr_init_library", (DL_FUNC)& dplyr_init_library, 1},

  {"dplyr_expand_groups", (DL_FUNC)& dplyr_expand_groups, 3},
  {"dplyr_between", (DL_FUNC)& dplyr_between, 3},
  {"dplyr_cumall", (DL_FUNC)& dplyr_cumall, 1},
  {"dplyr_cumany", (DL_FUNC)& dplyr_cumany, 1},
  {"dplyr_cummean", (DL_FUNC)& dplyr_cummean, 1},
  {"dplyr_validate_grouped_df", (DL_FUNC)& dplyr_validate_grouped_df, 2},

  {"dplyr_mask_eval_all", (DL_FUNC)& dplyr_mask_eval_all, 2},
  {"dplyr_mask_eval_all_summarise", (DL_FUNC)& dplyr_mask_eval_all_summarise, 2},
  {"dplyr_mask_eval_all_mutate", (DL_FUNC)& dplyr_mask_eval_all_mutate, 2},
  {"dplyr_mask_eval_all_filter", (DL_FUNC)& dplyr_mask_eval_all_filter, 4},

  {"dplyr_vec_sizes", (DL_FUNC)& dplyr_vec_sizes, 1},
  {"dplyr_validate_summarise_sizes", (DL_FUNC)& dplyr_validate_summarise_sizes, 2},
  {"dplyr_group_indices", (DL_FUNC)& dplyr_group_indices, 2},
  {"dplyr_group_keys", (DL_FUNC)& dplyr_group_keys, 1},
  {"dplyr_vec_sprinkle", (DL_FUNC)& dplyr_vec_sprinkle, 4},

  {NULL, NULL, 0}
};

extern "C" void R_init_dplyr(DllInfo* dll) {
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
