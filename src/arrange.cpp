#include "pch.h"
#include <dplyr/main.h>

#include <tools/Quosure.h>

#include <dplyr/white_list.h>

#include <dplyr/GroupedDataFrame.h>

#include <dplyr/Order.h>

#include <dplyr/Result/CallProxy.h>

#include <dplyr/Groups.h>
#include <dplyr/bad.h>

using namespace Rcpp;
using namespace dplyr;

#include <tools/debug.h>

template <typename SlicedTibble>
SEXP arrange_template(const SlicedTibble& gdf, const QuosureList& quosures) {
  const DataFrame& data = gdf.data();
  if (data.size() == 0 || data.nrows() == 0)
    return data;

  int nargs = quosures.size();
  if (nargs == 0)
    return data;

  check_valid_colnames(data);
  assert_all_white_list(data);
  List variables(nargs);
  LogicalVector ascending(nargs);

  for (int i = 0; i < nargs; i++) {
    const NamedQuosure& quosure = quosures[i];

    Shield<SEXP> call_(quosure.expr());
    SEXP call = call_;
    bool is_desc = TYPEOF(call) == LANGSXP && Rf_install("desc") == CAR(call);

    CallProxy call_proxy(is_desc ? CADR(call) : call, data, quosure.env());

    Shield<SEXP> v(call_proxy.eval());
    if (!white_list(v)) {
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
  DataFrameSubsetVisitors visitors(data, SymbolVector(data.names()));

  // organise the rows
  List res = visitors.subset(index, get_class(data));

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

