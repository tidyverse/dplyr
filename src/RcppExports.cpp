// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include "../inst/include/dplyr.h"
#include "../inst/include/dplyr_types.h"
#include <Rcpp.h>

using namespace Rcpp;

// loc
Rcpp::CharacterVector loc(SEXP data);
RcppExport SEXP _dplyr_loc(SEXP dataSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< SEXP >::type data(dataSEXP);
    rcpp_result_gen = Rcpp::wrap(loc(data));
    return rcpp_result_gen;
END_RCPP
}
// dfloc
Rcpp::CharacterVector dfloc(Rcpp::List df);
RcppExport SEXP _dplyr_dfloc(SEXP dfSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< Rcpp::List >::type df(dfSEXP);
    rcpp_result_gen = Rcpp::wrap(dfloc(df));
    return rcpp_result_gen;
END_RCPP
}
// plfloc
Rcpp::CharacterVector plfloc(Rcpp::Pairlist data);
RcppExport SEXP _dplyr_plfloc(SEXP dataSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< Rcpp::Pairlist >::type data(dataSEXP);
    rcpp_result_gen = Rcpp::wrap(plfloc(data));
    return rcpp_result_gen;
END_RCPP
}
// strings_addresses
Rcpp::CharacterVector strings_addresses(Rcpp::CharacterVector s);
RcppExport SEXP _dplyr_strings_addresses(SEXP sSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< Rcpp::CharacterVector >::type s(sSEXP);
    rcpp_result_gen = Rcpp::wrap(strings_addresses(s));
    return rcpp_result_gen;
END_RCPP
}
// init_logging
void init_logging(const std::string& log_level);
RcppExport SEXP _dplyr_init_logging(SEXP log_levelSEXP) {
BEGIN_RCPP
    Rcpp::traits::input_parameter< const std::string& >::type log_level(log_levelSEXP);
    init_logging(log_level);
    return R_NilValue;
END_RCPP
}
// is_maybe_shared
bool is_maybe_shared(SEXP env, SEXP name);
RcppExport SEXP _dplyr_is_maybe_shared(SEXP envSEXP, SEXP nameSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< SEXP >::type env(envSEXP);
    Rcpp::traits::input_parameter< SEXP >::type name(nameSEXP);
    rcpp_result_gen = Rcpp::wrap(is_maybe_shared(env, name));
    return rcpp_result_gen;
END_RCPP
}
// maybe_shared_columns
Rcpp::LogicalVector maybe_shared_columns(SEXP df);
RcppExport SEXP _dplyr_maybe_shared_columns(SEXP dfSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< SEXP >::type df(dfSEXP);
    rcpp_result_gen = Rcpp::wrap(maybe_shared_columns(df));
    return rcpp_result_gen;
END_RCPP
}
// between
Rcpp::LogicalVector between(Rcpp::NumericVector x, double left, double right);
RcppExport SEXP _dplyr_between(SEXP xSEXP, SEXP leftSEXP, SEXP rightSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< double >::type left(leftSEXP);
    Rcpp::traits::input_parameter< double >::type right(rightSEXP);
    rcpp_result_gen = Rcpp::wrap(between(x, left, right));
    return rcpp_result_gen;
END_RCPP
}
// flatten_bindable
SEXP flatten_bindable(SEXP x);
RcppExport SEXP _dplyr_flatten_bindable(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(flatten_bindable(x));
    return rcpp_result_gen;
END_RCPP
}
// bind_rows_check
void bind_rows_check(Rcpp::List dots);
RcppExport SEXP _dplyr_bind_rows_check(SEXP dotsSEXP) {
BEGIN_RCPP
    Rcpp::traits::input_parameter< Rcpp::List >::type dots(dotsSEXP);
    bind_rows_check(dots);
    return R_NilValue;
END_RCPP
}
// cbind_all
SEXP cbind_all(Rcpp::List dots);
RcppExport SEXP _dplyr_cbind_all(SEXP dotsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< Rcpp::List >::type dots(dotsSEXP);
    rcpp_result_gen = Rcpp::wrap(cbind_all(dots));
    return rcpp_result_gen;
END_RCPP
}
// combine_all
SEXP combine_all(Rcpp::List data);
RcppExport SEXP _dplyr_combine_all(SEXP dataSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< Rcpp::List >::type data(dataSEXP);
    rcpp_result_gen = Rcpp::wrap(combine_all(data));
    return rcpp_result_gen;
END_RCPP
}
// expand_groups
Rcpp::List expand_groups(Rcpp::DataFrame old_groups, Rcpp::List positions, int nr);
RcppExport SEXP _dplyr_expand_groups(SEXP old_groupsSEXP, SEXP positionsSEXP, SEXP nrSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< Rcpp::DataFrame >::type old_groups(old_groupsSEXP);
    Rcpp::traits::input_parameter< Rcpp::List >::type positions(positionsSEXP);
    Rcpp::traits::input_parameter< int >::type nr(nrSEXP);
    rcpp_result_gen = Rcpp::wrap(expand_groups(old_groups, positions, nr));
    return rcpp_result_gen;
END_RCPP
}
// filter_update_rows
SEXP filter_update_rows(int n_rows, SEXP group_indices, SEXP keep, SEXP new_rows_sizes);
RcppExport SEXP _dplyr_filter_update_rows(SEXP n_rowsSEXP, SEXP group_indicesSEXP, SEXP keepSEXP, SEXP new_rows_sizesSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< int >::type n_rows(n_rowsSEXP);
    Rcpp::traits::input_parameter< SEXP >::type group_indices(group_indicesSEXP);
    Rcpp::traits::input_parameter< SEXP >::type keep(keepSEXP);
    Rcpp::traits::input_parameter< SEXP >::type new_rows_sizes(new_rows_sizesSEXP);
    rcpp_result_gen = Rcpp::wrap(filter_update_rows(n_rows, group_indices, keep, new_rows_sizes));
    return rcpp_result_gen;
END_RCPP
}
// group_data_grouped_df
Rcpp::DataFrame group_data_grouped_df(Rcpp::DataFrame data);
RcppExport SEXP _dplyr_group_data_grouped_df(SEXP dataSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< Rcpp::DataFrame >::type data(dataSEXP);
    rcpp_result_gen = Rcpp::wrap(group_data_grouped_df(data));
    return rcpp_result_gen;
END_RCPP
}
// ungroup_grouped_df
Rcpp::DataFrame ungroup_grouped_df(Rcpp::DataFrame df);
RcppExport SEXP _dplyr_ungroup_grouped_df(SEXP dfSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< Rcpp::DataFrame >::type df(dfSEXP);
    rcpp_result_gen = Rcpp::wrap(ungroup_grouped_df(df));
    return rcpp_result_gen;
END_RCPP
}
// grouped_indices_grouped_df_impl
Rcpp::IntegerVector grouped_indices_grouped_df_impl(const dplyr::GroupedDataFrame& gdf);
RcppExport SEXP _dplyr_grouped_indices_grouped_df_impl(SEXP gdfSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< const dplyr::GroupedDataFrame& >::type gdf(gdfSEXP);
    rcpp_result_gen = Rcpp::wrap(grouped_indices_grouped_df_impl(gdf));
    return rcpp_result_gen;
END_RCPP
}
// select_impl
Rcpp::DataFrame select_impl(Rcpp::DataFrame df, Rcpp::CharacterVector vars);
RcppExport SEXP _dplyr_select_impl(SEXP dfSEXP, SEXP varsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< Rcpp::DataFrame >::type df(dfSEXP);
    Rcpp::traits::input_parameter< Rcpp::CharacterVector >::type vars(varsSEXP);
    rcpp_result_gen = Rcpp::wrap(select_impl(df, vars));
    return rcpp_result_gen;
END_RCPP
}
// test_comparisons
Rcpp::LogicalVector test_comparisons();
RcppExport SEXP _dplyr_test_comparisons() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    rcpp_result_gen = Rcpp::wrap(test_comparisons());
    return rcpp_result_gen;
END_RCPP
}
// test_length_wrap
Rcpp::LogicalVector test_length_wrap();
RcppExport SEXP _dplyr_test_length_wrap() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    rcpp_result_gen = Rcpp::wrap(test_length_wrap());
    return rcpp_result_gen;
END_RCPP
}
// check_valid_names
void check_valid_names(const Rcpp::CharacterVector& names, bool warn_only);
RcppExport SEXP _dplyr_check_valid_names(SEXP namesSEXP, SEXP warn_onlySEXP) {
BEGIN_RCPP
    Rcpp::traits::input_parameter< const Rcpp::CharacterVector& >::type names(namesSEXP);
    Rcpp::traits::input_parameter< bool >::type warn_only(warn_onlySEXP);
    check_valid_names(names, warn_only);
    return R_NilValue;
END_RCPP
}
// assert_all_allow_list
void assert_all_allow_list(const Rcpp::DataFrame& data);
RcppExport SEXP _dplyr_assert_all_allow_list(SEXP dataSEXP) {
BEGIN_RCPP
    Rcpp::traits::input_parameter< const Rcpp::DataFrame& >::type data(dataSEXP);
    assert_all_allow_list(data);
    return R_NilValue;
END_RCPP
}
// is_data_pronoun
bool is_data_pronoun(SEXP expr);
RcppExport SEXP _dplyr_is_data_pronoun(SEXP exprSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< SEXP >::type expr(exprSEXP);
    rcpp_result_gen = Rcpp::wrap(is_data_pronoun(expr));
    return rcpp_result_gen;
END_RCPP
}
// is_variable_reference
bool is_variable_reference(SEXP expr);
RcppExport SEXP _dplyr_is_variable_reference(SEXP exprSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< SEXP >::type expr(exprSEXP);
    rcpp_result_gen = Rcpp::wrap(is_variable_reference(expr));
    return rcpp_result_gen;
END_RCPP
}
// quo_is_variable_reference
bool quo_is_variable_reference(SEXP quo);
RcppExport SEXP _dplyr_quo_is_variable_reference(SEXP quoSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< SEXP >::type quo(quoSEXP);
    rcpp_result_gen = Rcpp::wrap(quo_is_variable_reference(quo));
    return rcpp_result_gen;
END_RCPP
}
// quo_is_data_pronoun
bool quo_is_data_pronoun(SEXP quo);
RcppExport SEXP _dplyr_quo_is_data_pronoun(SEXP quoSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< SEXP >::type quo(quoSEXP);
    rcpp_result_gen = Rcpp::wrap(quo_is_data_pronoun(quo));
    return rcpp_result_gen;
END_RCPP
}
// cumall
Rcpp::LogicalVector cumall(Rcpp::LogicalVector x);
RcppExport SEXP _dplyr_cumall(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< Rcpp::LogicalVector >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(cumall(x));
    return rcpp_result_gen;
END_RCPP
}
// cumany
Rcpp::LogicalVector cumany(Rcpp::LogicalVector x);
RcppExport SEXP _dplyr_cumany(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< Rcpp::LogicalVector >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(cumany(x));
    return rcpp_result_gen;
END_RCPP
}
// cummean
Rcpp::NumericVector cummean(Rcpp::NumericVector x);
RcppExport SEXP _dplyr_cummean(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(cummean(x));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_dplyr_loc", (DL_FUNC) &_dplyr_loc, 1},
    {"_dplyr_dfloc", (DL_FUNC) &_dplyr_dfloc, 1},
    {"_dplyr_plfloc", (DL_FUNC) &_dplyr_plfloc, 1},
    {"_dplyr_strings_addresses", (DL_FUNC) &_dplyr_strings_addresses, 1},
    {"_dplyr_init_logging", (DL_FUNC) &_dplyr_init_logging, 1},
    {"_dplyr_is_maybe_shared", (DL_FUNC) &_dplyr_is_maybe_shared, 2},
    {"_dplyr_maybe_shared_columns", (DL_FUNC) &_dplyr_maybe_shared_columns, 1},
    {"_dplyr_between", (DL_FUNC) &_dplyr_between, 3},
    {"_dplyr_flatten_bindable", (DL_FUNC) &_dplyr_flatten_bindable, 1},
    {"_dplyr_bind_rows_check", (DL_FUNC) &_dplyr_bind_rows_check, 1},
    {"_dplyr_cbind_all", (DL_FUNC) &_dplyr_cbind_all, 1},
    {"_dplyr_combine_all", (DL_FUNC) &_dplyr_combine_all, 1},
    {"_dplyr_expand_groups", (DL_FUNC) &_dplyr_expand_groups, 3},
    {"_dplyr_filter_update_rows", (DL_FUNC) &_dplyr_filter_update_rows, 4},
    {"_dplyr_group_data_grouped_df", (DL_FUNC) &_dplyr_group_data_grouped_df, 1},
    {"_dplyr_ungroup_grouped_df", (DL_FUNC) &_dplyr_ungroup_grouped_df, 1},
    {"_dplyr_grouped_indices_grouped_df_impl", (DL_FUNC) &_dplyr_grouped_indices_grouped_df_impl, 1},
    {"_dplyr_select_impl", (DL_FUNC) &_dplyr_select_impl, 2},
    {"_dplyr_test_comparisons", (DL_FUNC) &_dplyr_test_comparisons, 0},
    {"_dplyr_test_length_wrap", (DL_FUNC) &_dplyr_test_length_wrap, 0},
    {"_dplyr_check_valid_names", (DL_FUNC) &_dplyr_check_valid_names, 2},
    {"_dplyr_assert_all_allow_list", (DL_FUNC) &_dplyr_assert_all_allow_list, 1},
    {"_dplyr_is_data_pronoun", (DL_FUNC) &_dplyr_is_data_pronoun, 1},
    {"_dplyr_is_variable_reference", (DL_FUNC) &_dplyr_is_variable_reference, 1},
    {"_dplyr_quo_is_variable_reference", (DL_FUNC) &_dplyr_quo_is_variable_reference, 1},
    {"_dplyr_quo_is_data_pronoun", (DL_FUNC) &_dplyr_quo_is_data_pronoun, 1},
    {"_dplyr_cumall", (DL_FUNC) &_dplyr_cumall, 1},
    {"_dplyr_cumany", (DL_FUNC) &_dplyr_cumany, 1},
    {"_dplyr_cummean", (DL_FUNC) &_dplyr_cummean, 1},
    {NULL, NULL, 0}
};

RcppExport void R_init_dplyr(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
