#include "pch.h"
#include <dplyr/main.h>

#include <tools/utils.h>
#include <dplyr/data/GroupedDataFrame.h>

namespace dplyr {

SEXP select_not_grouped(const Rcpp::DataFrame& df, const SymbolVector& keep, const SymbolVector& new_names) {
  Rcpp::Shield<SEXP> positions(r_match(keep.get_vector(), Rf_getAttrib(df, symbols::names)));
  int* p_positions = INTEGER(positions);

  int n = keep.size();
  Rcpp::List res(n);
  for (int i = 0; i < n; i++) {
    int pos = p_positions[i];
    if (pos < 1 || pos > df.size()) {
      std::stringstream s;
      if (pos == NA_INTEGER) {
        s << "NA";
      } else {
        s << pos;
      }
      Rcpp::stop("invalid column index : %d for variable: '%s' = '%s'",
                 s.str(), new_names[i].get_utf8_cstring(), keep[i].get_utf8_cstring());
    }
    res[i] = df[ pos - 1 ];
  }
  copy_most_attributes(res, df);
  Rf_namesgets(res, new_names.get_vector());

  return res;
}

Rcpp::DataFrame select_grouped(const GroupedDataFrame& gdf, const SymbolVector& keep, const SymbolVector& new_names) {
  // start by selecting the columns without taking care of the grouping structure
  Rcpp::DataFrame copy = select_not_grouped(gdf.data(), keep, new_names);

  // then handle the groups attribute
  // it is almost the same as the groups attribute of the input data frame, but
  // names might change so we need to create a shallow copy and then deal with the names
  Rcpp::DataFrame groups(shallow_copy(Rcpp::List(gdf.group_data())));

  // update the names of the grouping variables in case they are involved in
  // the selection, i.e. select(data, g1 = g2)
  Rcpp::Shield<SEXP> group_names(Rf_duplicate(Rf_getAttrib(groups, dplyr::symbols::names)));

  Rcpp::Shield<SEXP> positions(r_match(group_names, keep.get_vector()));
  int nl = gdf.nvars();

  // maybe rename the variables in the groups metadata
  int* p_positions = INTEGER(positions);
  for (int i = 0; i < nl; i++) {
    int pos = p_positions[i];
    if (pos != NA_INTEGER) {
      SET_STRING_ELT(group_names, i, new_names[pos - 1].get_sexp());
    } else {
      bad_col(STRING_ELT(group_names, i), "not found in groups metadata. Probably a corrupt grouped_df object.");
    }
  }
  Rf_namesgets(groups, group_names);

  // then keep the grouping structure in the groups attribute
  GroupedDataFrame::set_groups(copy, groups) ;
  return copy;
}

}

// [[Rcpp::export(rng = false)]]
Rcpp::DataFrame select_impl(Rcpp::DataFrame df, Rcpp::CharacterVector vars) {
  check_valid_colnames(df);
  dplyr::SymbolVector s_vars(vars);
  dplyr::SymbolVector s_names_vars(Rf_getAttrib(vars, dplyr::symbols::names));
  if (Rcpp::is<dplyr::GroupedDataFrame>(df)) {
    dplyr::GroupedDataFrame gdf(df);
    return dplyr::select_grouped(gdf, s_vars, s_names_vars);
  } else {
    return dplyr::select_not_grouped(df, s_vars, s_names_vars);
  }
}
