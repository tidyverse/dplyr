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
extern SEXP dplyr_anti_join_impl(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP dplyr_arrange_impl(SEXP, SEXP);
extern SEXP dplyr_as_regular_df(SEXP);
extern SEXP dplyr_assert_all_white_list(SEXP);
extern SEXP dplyr_between(SEXP, SEXP, SEXP);
extern SEXP dplyr_bind_rows_(SEXP, SEXP);
extern SEXP dplyr_cbind_all(SEXP);
extern SEXP dplyr_combine_all(SEXP);
extern SEXP dplyr_combine_vars(SEXP, SEXP);
extern SEXP dplyr_compatible_data_frame(SEXP, SEXP, SEXP, SEXP);
extern SEXP dplyr_compatible_data_frame_nonames(SEXP, SEXP, SEXP);
extern SEXP dplyr_cumall(SEXP);
extern SEXP dplyr_cumany(SEXP);
extern SEXP dplyr_cummean(SEXP);
extern SEXP dplyr_dfloc(SEXP);
extern SEXP dplyr_distinct_impl(SEXP, SEXP, SEXP);
extern SEXP dplyr_equal_data_frame(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP dplyr_filter_impl(SEXP, SEXP);
extern SEXP dplyr_flatten_bindable(SEXP x);
extern SEXP dplyr_full_join_impl(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP dplyr_gp(SEXP);
extern SEXP dplyr_group_size_grouped_cpp(SEXP);
extern SEXP dplyr_grouped_df_impl(SEXP, SEXP, SEXP);
extern SEXP dplyr_grouped_indices_grouped_df_impl(SEXP);
extern SEXP dplyr_init_logging(SEXP);
extern SEXP dplyr_inner_join_impl(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP dplyr_intersect_data_frame(SEXP, SEXP);
extern SEXP dplyr_left_join_impl(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP dplyr_loc(SEXP);
extern SEXP dplyr_mutate_impl(SEXP, SEXP);
extern SEXP dplyr_n_distinct_multi(SEXP, SEXP);
extern SEXP dplyr_plfloc(SEXP);
extern SEXP dplyr_RcppExport_registerCCallable();
extern SEXP dplyr_right_join_impl(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP dplyr_select_impl(SEXP, SEXP);
extern SEXP dplyr_semi_join_impl(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP dplyr_setdiff_data_frame(SEXP, SEXP);
extern SEXP dplyr_shallow_copy(SEXP);
extern SEXP dplyr_slice_impl(SEXP, SEXP);
extern SEXP dplyr_strings_addresses(SEXP);
extern SEXP dplyr_summarise_impl(SEXP, SEXP);
extern SEXP dplyr_test_comparisons();
extern SEXP dplyr_test_matches();
extern SEXP dplyr_test_length_wrap();
extern SEXP dplyr_test_grouped_df(SEXP);
extern SEXP dplyr_ungroup_grouped_df(SEXP);
extern SEXP dplyr_union_data_frame(SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
  {"dplyr_anti_join_impl",                  (DL_FUNC) &dplyr_anti_join_impl,                  5},
  {"dplyr_arrange_impl",                    (DL_FUNC) &dplyr_arrange_impl,                    2},
  {"dplyr_as_regular_df",                   (DL_FUNC) &dplyr_as_regular_df,                   1},
  {"dplyr_assert_all_white_list",           (DL_FUNC) &dplyr_assert_all_white_list,           1},
  {"dplyr_between",                         (DL_FUNC) &dplyr_between,                         3},
  {"dplyr_bind_rows_",                      (DL_FUNC) &dplyr_bind_rows_,                      2},
  {"dplyr_cbind_all",                       (DL_FUNC) &dplyr_cbind_all,                       1},
  {"dplyr_combine_all",                     (DL_FUNC) &dplyr_combine_all,                     1},
  {"dplyr_combine_vars",                    (DL_FUNC) &dplyr_combine_vars,                    2},
  {"dplyr_compatible_data_frame",           (DL_FUNC) &dplyr_compatible_data_frame,           4},
  {"dplyr_compatible_data_frame_nonames",   (DL_FUNC) &dplyr_compatible_data_frame_nonames,   3},
  {"dplyr_cumall",                          (DL_FUNC) &dplyr_cumall,                          1},
  {"dplyr_cumany",                          (DL_FUNC) &dplyr_cumany,                          1},
  {"dplyr_cummean",                         (DL_FUNC) &dplyr_cummean,                         1},
  {"dplyr_dfloc",                           (DL_FUNC) &dplyr_dfloc,                           1},
  {"dplyr_distinct_impl",                   (DL_FUNC) &dplyr_distinct_impl,                   3},
  {"dplyr_equal_data_frame",                (DL_FUNC) &dplyr_equal_data_frame,                5},
  {"dplyr_filter_impl",                     (DL_FUNC) &dplyr_filter_impl,                     2},
  {"dplyr_flatten_bindable",                (DL_FUNC) &dplyr_flatten_bindable,                1},
  {"dplyr_full_join_impl",                  (DL_FUNC) &dplyr_full_join_impl,                  7},
  {"dplyr_gp",                              (DL_FUNC) &dplyr_gp,                              1},
  {"dplyr_group_size_grouped_cpp",          (DL_FUNC) &dplyr_group_size_grouped_cpp,          1},
  {"dplyr_grouped_df_impl",                 (DL_FUNC) &dplyr_grouped_df_impl,                 3},
  {"dplyr_grouped_indices_grouped_df_impl", (DL_FUNC) &dplyr_grouped_indices_grouped_df_impl, 1},
  {"dplyr_init_logging",                    (DL_FUNC) &dplyr_init_logging,                    1},
  {"dplyr_inner_join_impl",                 (DL_FUNC) &dplyr_inner_join_impl,                 7},
  {"dplyr_intersect_data_frame",            (DL_FUNC) &dplyr_intersect_data_frame,            2},
  {"dplyr_left_join_impl",                  (DL_FUNC) &dplyr_left_join_impl,                  7},
  {"dplyr_loc",                             (DL_FUNC) &dplyr_loc,                             1},
  {"dplyr_mutate_impl",                     (DL_FUNC) &dplyr_mutate_impl,                     2},
  {"dplyr_n_distinct_multi",                (DL_FUNC) &dplyr_n_distinct_multi,                2},
  {"dplyr_plfloc",                          (DL_FUNC) &dplyr_plfloc,                          1},
  {"dplyr_RcppExport_registerCCallable",    (DL_FUNC) &dplyr_RcppExport_registerCCallable,    0},
  {"dplyr_right_join_impl",                 (DL_FUNC) &dplyr_right_join_impl,                 7},
  {"dplyr_select_impl",                     (DL_FUNC) &dplyr_select_impl,                     2},
  {"dplyr_semi_join_impl",                  (DL_FUNC) &dplyr_semi_join_impl,                  5},
  {"dplyr_setdiff_data_frame",              (DL_FUNC) &dplyr_setdiff_data_frame,              2},
  {"dplyr_shallow_copy",                    (DL_FUNC) &dplyr_shallow_copy,                    1},
  {"dplyr_slice_impl",                      (DL_FUNC) &dplyr_slice_impl,                      2},
  {"dplyr_strings_addresses",               (DL_FUNC) &dplyr_strings_addresses,               1},
  {"dplyr_summarise_impl",                  (DL_FUNC) &dplyr_summarise_impl,                  2},
  {"dplyr_test_comparisons",                (DL_FUNC) &dplyr_test_comparisons,                0},
  {"dplyr_test_matches",                    (DL_FUNC) &dplyr_test_matches,                    0},
  {"dplyr_test_length_wrap",                (DL_FUNC) &dplyr_test_length_wrap,                0},
  {"dplyr_test_grouped_df",                 (DL_FUNC) &dplyr_test_grouped_df,                 1},
  {"dplyr_ungroup_grouped_df",              (DL_FUNC) &dplyr_ungroup_grouped_df,              1},
  {"dplyr_union_data_frame",                (DL_FUNC) &dplyr_union_data_frame,                2},
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
