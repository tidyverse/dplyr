#include "pch.h"
#include <dplyr/main.h>

#include <tools/train.h>
#include <tools/bad.h>
#include <tools/match.h>
#include <tools/utils.h>
#include <dplyr/symbols.h>

#include <dplyr/data/GroupedDataFrame.h>
#include <dplyr/data/NaturalDataFrame.h>

namespace dplyr {

// call the R function dplyr::make_grouped_df_groups_attribute()
SEXP make_grouped_df_groups_attribute(SEXP data, SEXP vars, bool drop) {
  Rcpp::Environment ns_dplyr = Rcpp::Environment::namespace_env("dplyr");
  Rcpp::Function R_dplyr__make_grouped_df_groups_attribute = ns_dplyr["make_grouped_df_groups_attribute"];
  return R_dplyr__make_grouped_df_groups_attribute(data, vars, drop);
}

SEXP check_grouped(Rcpp::RObject data) {
  // compat with old style grouped data frames
  SEXP vars = Rf_getAttrib(data, symbols::vars);

  if (!Rf_isNull(vars)) {
    Rf_warningcall(R_NilValue, "Detecting old grouped_df format, replacing `vars` attribute by `groups`");

    // only make the groups attribute if it does not yet exist
    if (Rf_isNull(Rf_getAttrib(data, symbols::groups))) {
      // using drop = true here because this is likely to play better with
      // older representations
      Rcpp::DataFrame groups = make_grouped_df_groups_attribute(data, vars, true);
      Rf_setAttrib(data, symbols::groups, groups);
    }

    // but always clean the pre 0.8.0 attributes
    Rf_setAttrib(data, symbols::vars, R_NilValue);
    Rf_setAttrib(data, symbols::indices, R_NilValue);
    Rf_setAttrib(data, symbols::labels, R_NilValue);

  }

  // get the groups attribute and check for consistency
  SEXP groups = Rf_getAttrib(data, symbols::groups);

  // groups must be a data frame
  if (!Rcpp::is<Rcpp::DataFrame>(groups)) {
    bad_arg(".data", "is a corrupt grouped_df, the `\"groups\"` attribute must be a data frame");
  }

  int nc = Rf_length(groups);

  // it must have at least 1 column
  if (nc < 1) {
    bad_arg(".data", "is a corrupt grouped_df, the `\"groups\"` attribute must have at least one column");
  }

  // the last column must be a list and called `.rows`
  SEXP names = Rf_getAttrib(groups, R_NamesSymbol);
  SEXP last = VECTOR_ELT(groups, nc - 1);
  static Rcpp::String rows(".rows");
  if (TYPEOF(last) != VECSXP || STRING_ELT(names, nc - 1) != rows.get_sexp()) {
    bad_arg(".data", "is a corrupt grouped_df, the `\"groups\"` attribute must have a list column named `.rows` as last column");
  }

  return data ;
}

GroupedDataFrame::GroupedDataFrame(Rcpp::DataFrame x):
  data_(check_grouped(x)),
  symbols(group_vars()),
  groups(Rf_getAttrib(data_, symbols::groups)),
  nvars_(symbols.size())
{}

GroupedDataFrame::GroupedDataFrame(Rcpp::DataFrame x, const GroupedDataFrame& model):
  data_(x),
  symbols(model.get_vars()),
  groups(make_grouped_df_groups_attribute(data_, model.get_vars().get_vector(), model.drops())),
  nvars_(symbols.size())
{
  set_groups(data_, groups);
}

SymbolVector GroupedDataFrame::group_vars() const {
  SEXP groups = Rf_getAttrib(data_, dplyr::symbols::groups);

  int n = Rf_length(groups) - 1;
  Rcpp::Shelter<SEXP> shelter;
  SEXP vars_attr = shelter(Rf_getAttrib(groups, R_NamesSymbol));
  SEXP vars = shelter(Rf_allocVector(STRSXP, n));
  for (int i = 0; i < n; i++) {
    SET_STRING_ELT(vars, i, STRING_ELT(vars_attr, i));
  }
  return SymbolVector(vars);
}

}

// [[Rcpp::export(rng = false)]]
Rcpp::DataFrame group_data_grouped_df(Rcpp::DataFrame data) {
  return dplyr::GroupedDataFrame(data).group_data();
}

// [[Rcpp::export(rng = false)]]
Rcpp::DataFrame ungroup_grouped_df(Rcpp::DataFrame df) {
  Rcpp::DataFrame copy(shallow_copy(df));
  dplyr::GroupedDataFrame::strip_groups(copy);
  dplyr::set_class(copy, dplyr::NaturalDataFrame::classes());
  return copy;
}

// [[Rcpp::export(rng = false)]]
Rcpp::IntegerVector grouped_indices_grouped_df_impl(const dplyr::GroupedDataFrame& gdf) {
  int n = gdf.nrows();
  Rcpp::IntegerVector res(Rcpp::no_init(n));
  int ngroups = gdf.ngroups();
  dplyr::GroupedDataFrameIndexIterator it = gdf.group_begin();
  for (int i = 0; i < ngroups; i++, ++it) {
    const GroupedSlicingIndex& index = *it;
    int n_index = index.size();
    for (int j = 0; j < n_index; j++) {
      res[ index[j] ] = i + 1;
    }
  }
  return res;
}
