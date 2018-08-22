#include "pch.h"
#include <dplyr/main.h>

#include <tools/Quosure.h>

#include <dplyr/allow_list.h>

#include <dplyr/data/GroupedDataFrame.h>

#include <dplyr/visitors/subset/DataFrameSubsetVisitors.h>
#include <dplyr/visitors/order/Order.h>
#include <dplyr/Groups.h>
#include <tools/bad.h>
#include <dplyr/DataMask.h>

using namespace Rcpp;
using namespace dplyr;

#include <tools/debug.h>

template <typename SlicedTibble>
SEXP arrange_template(const SlicedTibble& gdf, const QuosureList& quosures) {
  typedef LazySplitSubsets<NaturalDataFrame> Subsets;
  static SEXP symb_desc = Rf_install("desc");

  const DataFrame& data = gdf.data();
  if (data.size() == 0 || data.nrows() == 0)
    return data;

  int nargs = quosures.size();
  if (nargs == 0)
    return data;

  check_valid_colnames(data);
  assert_all_allow_list(data);
  List variables(nargs);
  LogicalVector ascending(nargs);

  Subsets subsets(NaturalDataFrame(gdf.data()));
  NaturalSlicingIndex indices_all(gdf.nrows());

  for (int i = 0; i < nargs; i++) {
    const NamedQuosure& quosure = quosures[i];
    SEXP env = quosure.env();
    SEXP expr = quosure.expr();
    bool is_desc = TYPEOF(expr) == LANGSXP && symb_desc == CAR(expr);
    expr = is_desc ? CADR(expr) : expr ;

    DataMask<NaturalDataFrame> data_mask(subsets, env);
    Shield<SEXP> v(data_mask.eval(expr, indices_all));
    if (!allow_list(v)) {
      stop("cannot arrange column of class '%s' at position %d", get_single_class(v), i + 1);
    }

    if (Rf_inherits(v, "data.frame")) {
      bad_pos_arg(i + 1, "is of unsupported type data.frame");
    } else if (Rf_isMatrix(v)) {
      bad_pos_arg(i + 1, "is of unsupported type matrix");
    } else {
      if (Rf_length(v) != data.nrows()) {
        stop("incorrect size (%d) at position %d, expecting : %d", Rf_length(v), i + 1, data.nrows());
      }
    }
    variables[i] = v;
    ascending[i] = !is_desc;
  }
  variables.names() = quosures.names();
  OrderVisitors o(variables, ascending, nargs);
  IntegerVector index = o.apply();

  List res = DataFrameSubsetVisitors(data).subset_all(index);

  // let the grouping class organise the rest (the groups attribute etc ...)
  return SlicedTibble(res, gdf).data();

}

// [[Rcpp::export]]
SEXP arrange_impl(DataFrame df, QuosureList quosures) {
  if (is<RowwiseDataFrame>(df)) {
    return arrange_template<RowwiseDataFrame>(RowwiseDataFrame(df), quosures);
  } else if (is<GroupedDataFrame>(df)) {
    return arrange_template<GroupedDataFrame>(GroupedDataFrame(df), quosures);
  } else {
    return arrange_template<NaturalDataFrame>(NaturalDataFrame(df), quosures);
  }
}

