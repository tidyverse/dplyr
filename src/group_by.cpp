#include "pch.h"
#include <dplyr/main.h>

#include <tools/match.h>
#include <tools/utils.h>

#include <dplyr/tbl_cpp.h>
#include <dplyr/Groups.h>

using namespace Rcpp;
using namespace dplyr;

// [[Rcpp::export]]
DataFrame grouped_df_impl(DataFrame data, SymbolVector symbols, bool drop) {
  assert_all_white_list(data);
  DataFrame copy(shallow_copy(data));
  set_vars(copy, symbols);
  copy.attr("drop") = drop;
  if (!symbols.size())
    stop("no variables to group by");
  return build_index_cpp(copy);
}

// [[Rcpp::export]]
DataFrame as_regular_df(DataFrame df) {
  DataFrame copy(shallow_copy(df));
  SET_ATTRIB(copy, strip_group_attributes(df));
  SET_OBJECT(copy, OBJECT(df));
  set_class(copy, CharacterVector::create("data.frame"));
  return copy;
}

// [[Rcpp::export]]
DataFrame ungroup_grouped_df(DataFrame df) {
  DataFrame copy(shallow_copy(df));
  SET_ATTRIB(copy, strip_group_attributes(df));
  return copy;
}

// [[Rcpp::export]]
SEXP test_grouped_df(DataFrame data) {
  return GroupedDataFrame(data).data();
}
