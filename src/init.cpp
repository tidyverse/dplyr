#include "dplyr.h"

namespace dplyr {

SEXP envs::ns_dplyr = NULL;
SEXP envs::ns_vctrs = NULL;
SEXP envs::ns_rlang = NULL;

SEXP get_classes_vctrs_list_of() {
  SEXP klasses = Rf_allocVector(STRSXP, 3);
  R_PreserveObject(klasses);
  SET_STRING_ELT(klasses, 0, Rf_mkChar("vctrs_list_of"));
  SET_STRING_ELT(klasses, 1, Rf_mkChar("vctrs_vctr"));
  SET_STRING_ELT(klasses, 2, Rf_mkChar("list"));
  return klasses;
}

SEXP get_empty_int_vector() {
  SEXP x = Rf_allocVector(INTSXP, 0);
  R_PreserveObject(x);
  return x;
}

SEXP get_names_expanded() {
  SEXP names = Rf_allocVector(STRSXP, 2);
  R_PreserveObject(names);
  SET_STRING_ELT(names, 0, Rf_mkChar("indices"));
  SET_STRING_ELT(names, 1, Rf_mkChar("rows"));
  return names;
}

SEXP get_names_summarise_recycle_chunks(){
  SEXP names = Rf_allocVector(STRSXP, 3);
  R_PreserveObject(names);

  SET_STRING_ELT(names, 0, Rf_mkChar("chunks"));
  SET_STRING_ELT(names, 1, Rf_mkChar("sizes"));
  SET_STRING_ELT(names, 2, Rf_mkChar("results"));
  return names;
}

SEXP symbols::ptype = Rf_install("ptype");
SEXP symbols::levels = Rf_install("levels");
SEXP symbols::groups = Rf_install("groups");
SEXP symbols::current_group_id = Rf_install("dplyr:::current_group_id");
SEXP symbols::current_group_size = Rf_install("dplyr:::current_group_size");
SEXP symbols::current_expression = Rf_install("current_expression");
SEXP symbols::rows = Rf_install("rows");
SEXP symbols::caller = Rf_install("caller");
SEXP symbols::current_data = Rf_install("current_data");
SEXP symbols::dot_drop = Rf_install(".drop");
SEXP symbols::dplyr_internal_error = Rf_install("dplyr_internal_error");
SEXP symbols::dplyr_internal_signal = Rf_install("dplyr_internal_signal");
SEXP symbols::chops = Rf_install("chops");
SEXP symbols::obj_is_list = Rf_install("obj_is_list");
SEXP symbols::new_env = Rf_install("new.env");
SEXP symbols::dot_data = Rf_install(".data");
SEXP symbols::used = Rf_install("used");
SEXP symbols::across = Rf_install("across");
SEXP symbols::env_current_group_info = Rf_install("env_current_group_info");
SEXP symbols::env_mask_bindings = Rf_install("env_mask_bindings");

SEXP vectors::classes_vctrs_list_of = get_classes_vctrs_list_of();
SEXP vectors::empty_int_vector = get_empty_int_vector();

SEXP vectors::names_expanded = get_names_expanded();
SEXP vectors::names_summarise_recycle_chunks = get_names_summarise_recycle_chunks();

SEXP functions::vec_chop = NULL;
SEXP functions::dot_subset2 = NULL;
SEXP functions::list = NULL;
SEXP functions::function = NULL;

} // dplyr

SEXP dplyr_init_library(SEXP ns_dplyr, SEXP ns_vctrs, SEXP ns_rlang) {
  dplyr::envs::ns_dplyr = ns_dplyr;
  dplyr::envs::ns_vctrs = ns_vctrs;
  dplyr::envs::ns_rlang = ns_rlang;
  dplyr::functions::vec_chop = PROTECT(Rf_findVarInFrame(ns_vctrs, Rf_install("vec_chop")));
  dplyr::functions::dot_subset2 = PROTECT(Rf_findVarInFrame(R_BaseEnv, Rf_install(".subset2")));
  dplyr::functions::list = PROTECT(Rf_findVarInFrame(R_BaseEnv, Rf_install("list")));
  dplyr::functions::function = PROTECT(Rf_eval(Rf_install("function"), R_BaseEnv));

  R_PreserveObject(dplyr::functions::vec_chop);
  R_PreserveObject(dplyr::functions::dot_subset2);
  R_PreserveObject(dplyr::functions::list);
  R_PreserveObject(dplyr::functions::function);

  UNPROTECT(4);

  return R_NilValue;
}

static const R_CallMethodDef CallEntries[] = {
  {"dplyr_init_library", (DL_FUNC)& dplyr_init_library, 3},

  {"ffi_dplyr_reconstruct", (DL_FUNC)& ffi_dplyr_reconstruct, 2},
  {"ffi_test_dplyr_attributes", (DL_FUNC)& ffi_test_dplyr_attributes, 1},
  {"ffi_test_dplyr_set_attributes", (DL_FUNC)& ffi_test_dplyr_set_attributes, 2},

  {"dplyr_expand_groups", (DL_FUNC)& dplyr_expand_groups, 3},
  {"dplyr_cumall", (DL_FUNC)& dplyr_cumall, 1},
  {"dplyr_cumany", (DL_FUNC)& dplyr_cumany, 1},
  {"dplyr_cummean", (DL_FUNC)& dplyr_cummean, 1},
  {"dplyr_validate_grouped_df", (DL_FUNC)& dplyr_validate_grouped_df, 2},
  {"dplyr_validate_rowwise_df", (DL_FUNC)& dplyr_validate_rowwise_df, 1},

  {"dplyr_mask_eval_all", (DL_FUNC)& dplyr_mask_eval_all, 2},
  {"dplyr_mask_eval_all_summarise", (DL_FUNC)& dplyr_mask_eval_all_summarise, 2},
  {"dplyr_mask_eval_all_mutate", (DL_FUNC)& dplyr_mask_eval_all_mutate, 2},
  {"dplyr_mask_eval_all_filter", (DL_FUNC)& dplyr_mask_eval_all_filter, 4},

  {"dplyr_summarise_recycle_chunks_in_place", (DL_FUNC)& dplyr_summarise_recycle_chunks_in_place, 2},

  {"dplyr_group_indices", (DL_FUNC)& dplyr_group_indices, 2},
  {"dplyr_group_keys", (DL_FUNC)& dplyr_group_keys, 1},

  {"dplyr_mask_binding_remove", (DL_FUNC)& dplyr_mask_binding_remove, 2},
  {"dplyr_mask_binding_add", (DL_FUNC)& dplyr_mask_binding_add, 4},

  {"dplyr_lazy_vec_chop_impl", (DL_FUNC)& dplyr_lazy_vec_chop, 5},
  {"dplyr_make_mask_bindings", (DL_FUNC)& dplyr_make_mask_bindings, 2},
  {"env_resolved", (DL_FUNC)& env_resolved, 2},

  {"dplyr_extract_chunks", (DL_FUNC)& dplyr_extract_chunks, 2},

  {NULL, NULL, 0}
};

extern "C" void R_init_dplyr(DllInfo* dll) {
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
