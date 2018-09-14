#include "pch.h"
#include <dplyr/main.h>

#include <tools/utils.h>

#include <dplyr/data/GroupedDataFrame.h>

using namespace Rcpp;
using namespace dplyr;

SEXP select_not_grouped(const DataFrame& df, const SymbolVector& keep, const SymbolVector& new_names) {
  IntegerVector positions = keep.match_in_table(df.names());
  int n = keep.size();
  List res(n);
  for (int i = 0; i < n; i++) {
    int pos = positions[i];
    if (pos < 1 || pos > df.size()) {
      std::stringstream s;
      if (pos == NA_INTEGER) {
        s << "NA";
      } else {
        s << pos;
      }
      stop("invalid column index : %d for variable: '%s' = '%s'",
           s.str(), new_names[i].get_utf8_cstring(), keep[i].get_utf8_cstring());
    }
    res[i] = df[ pos - 1 ];
  }
  copy_most_attributes(res, df);
  res.names() = new_names;
  return res;
}

DataFrame select_grouped(GroupedDataFrame gdf, const SymbolVector& keep, const SymbolVector& new_names) {
  // start by selecting the columns without taking care of the grouping structure
  DataFrame copy = select_not_grouped(gdf.data(), keep, new_names);

  // then handle the groups attribute
  // it is almost the same as the groups attribute of the input data frame, but
  // names might change so we need to create a shallow copy and then deal with the names
  DataFrame groups(shallow_copy(List(gdf.group_data())));

  // update the names of the grouping variables in case they are involved in
  // the selection, i.e. select(data, g1 = g2)
  CharacterVector group_names = clone<CharacterVector>(groups.names());
  IntegerVector positions = keep.match(group_names);
  int nl = gdf.nvars();

  // maybe rename the variables in the groups metadata
  for (int i = 0; i < nl; i++) {
    int pos = positions[i];
    if (pos != NA_INTEGER) {
      group_names[i] = new_names[pos - 1].get_string();
    } else {
      bad_col(group_names[i], "not found in groups metadata. Probably a corrupt grouped_df object.");
    }
  }
  groups.names() = group_names;

  // then keep the grouping structure in the groups attribute
  GroupedDataFrame::set_groups(copy, groups) ;
  return copy;
}

// [[Rcpp::export]]
DataFrame select_impl(DataFrame df, CharacterVector vars) {
  check_valid_colnames(df);
  if (is<GroupedDataFrame>(df)) {
    return select_grouped(GroupedDataFrame(df), SymbolVector(vars), SymbolVector(vars.names()));
  } else {
    return select_not_grouped(df, SymbolVector(vars), SymbolVector(vars.names()));
  }
}
