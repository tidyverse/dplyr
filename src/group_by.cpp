#include "pch.h"
#include <dplyr/main.h>

#include <tools/match.h>
#include <tools/utils.h>

#include <dplyr/Groups.h>
#include <dplyr/data/tbl_classes.h>

using namespace Rcpp;
using namespace dplyr;

// [[Rcpp::export]]
DataFrame as_regular_df(DataFrame df) {
  DataFrame copy(shallow_copy(df));
  GroupedDataFrame::strip_groups(copy);
  SET_OBJECT(copy, OBJECT(df));
  set_class(copy, CharacterVector::create("data.frame"));
  return copy;
}

// [[Rcpp::export]]
DataFrame ungroup_grouped_df(DataFrame df) {
  DataFrame copy(shallow_copy(df));
  GroupedDataFrame::strip_groups(copy);
  set_class(copy, tbl_classes<NaturalDataFrame>());
  return copy;
}
