#include "pch.h"
#include <dplyr/main.h>

#include <tools/utils.h>

#include <dplyr/GroupedDataFrame.h>

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
  DataFrame copy = select_not_grouped(gdf.data(), keep, new_names);

  SymbolMap keep_map(keep);

  // handle vars  attribute : make a shallow copy of the list and alter
  //   its names attribute
  SymbolVector vars(get_vars(copy));

  int nv = vars.size();
  for (int i = 0; i < nv; i++) {
    SymbolString s = vars[i];
    SymbolMapIndex j = keep_map.get_index(s);
    if (j.origin != NEW) {
      vars.set(i, new_names[j.pos]);
    }
  }

  set_vars(copy, vars);

  // handle labels attribute
  //   make a shallow copy of the data frame and alter its names attributes
  if (!Rf_isNull(copy.attr("labels"))) {

    DataFrame original_labels(copy.attr("labels"));

    DataFrame labels(shallow_copy(original_labels));
    CharacterVector label_names = clone<CharacterVector>(labels.names());

    IntegerVector positions = keep.match(label_names);
    int nl = label_names.size();
    for (int i = 0; i < nl; i++) {
      int pos = positions[i];
      if (pos != NA_INTEGER) {
        label_names[i] = new_names[pos - 1].get_string();
      }
    }
    labels.names() = label_names;
    set_vars(labels, vars);
    copy.attr("labels") = labels;
  }
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
