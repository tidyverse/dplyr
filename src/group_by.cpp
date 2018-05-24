#include "pch.h"
#include <dplyr/main.h>

#include <tools/match.h>
#include <tools/utils.h>

#include <dplyr/tbl_cpp.h>
#include <dplyr/Groups.h>

using namespace Rcpp;
using namespace dplyr;

// [[Rcpp::export]]
DataFrame as_regular_df(DataFrame df) {
  DataFrame copy(shallow_copy(df));
  copy.attr("groups") = R_NilValue ;
  SET_OBJECT(copy, OBJECT(df));
  set_class(copy, CharacterVector::create("data.frame"));
  return copy;
}

// [[Rcpp::export]]
DataFrame ungroup_grouped_df(DataFrame df) {
  DataFrame copy(shallow_copy(df));
  copy.attr("groups") = R_NilValue ;
  set_class(copy, classes_not_grouped());
  return copy;
}
