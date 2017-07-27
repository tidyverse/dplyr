#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <stdbool.h>
#include <R_ext/Rdynload.h>

#include <tools/rlang-export.h>

/* FIXME:
  Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP _dplyr_anti_join_impl(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _dplyr_arrange_impl(SEXP, SEXP);
extern SEXP _dplyr_as_regular_df(SEXP);
extern SEXP _dplyr_assert_all_white_list(SEXP);
extern SEXP _dplyr_between(SEXP, SEXP, SEXP);
extern SEXP _dplyr_bind_rows_(SEXP, SEXP);
extern SEXP _dplyr_cbind_all(SEXP);
extern SEXP _dplyr_combine_all(SEXP);
extern SEXP _dplyr_combine_vars(SEXP, SEXP);
extern SEXP _dplyr_compatible_data_frame(SEXP, SEXP, SEXP, SEXP);
extern SEXP _dplyr_compatible_data_frame_nonames(SEXP, SEXP, SEXP);
extern SEXP _dplyr_cumall(SEXP);
extern SEXP _dplyr_cumany(SEXP);
extern SEXP _dplyr_cummean(SEXP);
extern SEXP _dplyr_dfloc(SEXP);
extern SEXP _dplyr_distinct_impl(SEXP, SEXP, SEXP);
extern SEXP _dplyr_equal_data_frame(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _dplyr_filter_impl(SEXP, SEXP);
extern SEXP _dplyr_flatten_bindable(SEXP x);
extern SEXP _dplyr_full_join_impl(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _dplyr_gp(SEXP);
extern SEXP _dplyr_group_size_grouped_cpp(SEXP);
extern SEXP _dplyr_grouped_df_impl(SEXP, SEXP, SEXP);
extern SEXP _dplyr_grouped_indices_grouped_df_impl(SEXP);
extern SEXP _dplyr_init_logging(SEXP);
extern SEXP _dplyr_inner_join_impl(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _dplyr_intersect_data_frame(SEXP, SEXP);
extern SEXP _dplyr_left_join_impl(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _dplyr_loc(SEXP);
extern SEXP _dplyr_mutate_impl(SEXP, SEXP);
extern SEXP _dplyr_n_distinct_multi(SEXP, SEXP);
extern SEXP _dplyr_plfloc(SEXP);
extern SEXP _dplyr_RcppExport_registerCCallable();
extern SEXP _dplyr_right_join_impl(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _dplyr_select_impl(SEXP, SEXP);
extern SEXP _dplyr_semi_join_impl(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _dplyr_setdiff_data_frame(SEXP, SEXP);
extern SEXP _dplyr_shallow_copy(SEXP);
extern SEXP _dplyr_slice_impl(SEXP, SEXP);
extern SEXP _dplyr_strings_addresses(SEXP);
extern SEXP _dplyr_summarise_impl(SEXP, SEXP);
extern SEXP _dplyr_test_comparisons();
extern SEXP _dplyr_test_matches();
extern SEXP _dplyr_test_length_wrap();
extern SEXP _dplyr_test_grouped_df(SEXP);
extern SEXP _dplyr_ungroup_grouped_df(SEXP);
extern SEXP _dplyr_union_data_frame(SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
  {"_dplyr_anti_join_impl",                  (DL_FUNC) &_dplyr_anti_join_impl,                  5},
  {"_dplyr_arrange_impl",                    (DL_FUNC) &_dplyr_arrange_impl,                    2},
  {"_dplyr_as_regular_df",                   (DL_FUNC) &_dplyr_as_regular_df,                   1},
  {"_dplyr_assert_all_white_list",           (DL_FUNC) &_dplyr_assert_all_white_list,           1},
  {"_dplyr_between",                         (DL_FUNC) &_dplyr_between,                         3},
  {"_dplyr_bind_rows_",                      (DL_FUNC) &_dplyr_bind_rows_,                      2},
  {"_dplyr_cbind_all",                       (DL_FUNC) &_dplyr_cbind_all,                       1},
  {"_dplyr_combine_all",                     (DL_FUNC) &_dplyr_combine_all,                     1},
  {"_dplyr_combine_vars",                    (DL_FUNC) &_dplyr_combine_vars,                    2},
  {"_dplyr_compatible_data_frame",           (DL_FUNC) &_dplyr_compatible_data_frame,           4},
  {"_dplyr_compatible_data_frame_nonames",   (DL_FUNC) &_dplyr_compatible_data_frame_nonames,   3},
  {"_dplyr_cumall",                          (DL_FUNC) &_dplyr_cumall,                          1},
  {"_dplyr_cumany",                          (DL_FUNC) &_dplyr_cumany,                          1},
  {"_dplyr_cummean",                         (DL_FUNC) &_dplyr_cummean,                         1},
  {"_dplyr_dfloc",                           (DL_FUNC) &_dplyr_dfloc,                           1},
  {"_dplyr_distinct_impl",                   (DL_FUNC) &_dplyr_distinct_impl,                   3},
  {"_dplyr_equal_data_frame",                (DL_FUNC) &_dplyr_equal_data_frame,                5},
  {"_dplyr_filter_impl",                     (DL_FUNC) &_dplyr_filter_impl,                     2},
  {"_dplyr_flatten_bindable",                (DL_FUNC) &_dplyr_flatten_bindable,                1},
  {"_dplyr_full_join_impl",                  (DL_FUNC) &_dplyr_full_join_impl,                  7},
  {"_dplyr_gp",                              (DL_FUNC) &_dplyr_gp,                              1},
  {"_dplyr_group_size_grouped_cpp",          (DL_FUNC) &_dplyr_group_size_grouped_cpp,          1},
  {"_dplyr_grouped_df_impl",                 (DL_FUNC) &_dplyr_grouped_df_impl,                 3},
  {"_dplyr_grouped_indices_grouped_df_impl", (DL_FUNC) &_dplyr_grouped_indices_grouped_df_impl, 1},
  {"_dplyr_init_logging",                    (DL_FUNC) &_dplyr_init_logging,                    1},
  {"_dplyr_inner_join_impl",                 (DL_FUNC) &_dplyr_inner_join_impl,                 7},
  {"_dplyr_intersect_data_frame",            (DL_FUNC) &_dplyr_intersect_data_frame,            2},
  {"_dplyr_left_join_impl",                  (DL_FUNC) &_dplyr_left_join_impl,                  7},
  {"_dplyr_loc",                             (DL_FUNC) &_dplyr_loc,                             1},
  {"_dplyr_mutate_impl",                     (DL_FUNC) &_dplyr_mutate_impl,                     2},
  {"_dplyr_n_distinct_multi",                (DL_FUNC) &_dplyr_n_distinct_multi,                2},
  {"_dplyr_plfloc",                          (DL_FUNC) &_dplyr_plfloc,                          1},
  {"_dplyr_RcppExport_registerCCallable",    (DL_FUNC) &_dplyr_RcppExport_registerCCallable,    0},
  {"_dplyr_right_join_impl",                 (DL_FUNC) &_dplyr_right_join_impl,                 7},
  {"_dplyr_select_impl",                     (DL_FUNC) &_dplyr_select_impl,                     2},
  {"_dplyr_semi_join_impl",                  (DL_FUNC) &_dplyr_semi_join_impl,                  5},
  {"_dplyr_setdiff_data_frame",              (DL_FUNC) &_dplyr_setdiff_data_frame,              2},
  {"_dplyr_shallow_copy",                    (DL_FUNC) &_dplyr_shallow_copy,                    1},
  {"_dplyr_slice_impl",                      (DL_FUNC) &_dplyr_slice_impl,                      2},
  {"_dplyr_strings_addresses",               (DL_FUNC) &_dplyr_strings_addresses,               1},
  {"_dplyr_summarise_impl",                  (DL_FUNC) &_dplyr_summarise_impl,                  2},
  {"_dplyr_test_comparisons",                (DL_FUNC) &_dplyr_test_comparisons,                0},
  {"_dplyr_test_matches",                    (DL_FUNC) &_dplyr_test_matches,                    0},
  {"_dplyr_test_length_wrap",                (DL_FUNC) &_dplyr_test_length_wrap,                0},
  {"_dplyr_test_grouped_df",                 (DL_FUNC) &_dplyr_test_grouped_df,                 1},
  {"_dplyr_ungroup_grouped_df",              (DL_FUNC) &_dplyr_ungroup_grouped_df,              1},
  {"_dplyr_union_data_frame",                (DL_FUNC) &_dplyr_union_data_frame,                2},
  {NULL, NULL, 0}
};

/* Raw function pointers */
extern bool dplyr_is_bind_spliceable(SEXP x);

void R_init_dplyr(DllInfo *dll)
{
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
  rlang_register_pointer("dplyr", "is_bind_spliceable", (DL_FUNC) &dplyr_is_bind_spliceable);
}
