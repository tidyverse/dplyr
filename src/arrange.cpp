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

// [[Rcpp::export]]
List arrange_impl(DataFrame data, QuosureList quosures) {
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
      DataFrame df(v);
      int nr = df.nrows();
      if (nr != data.nrows()) {
        stop("data frame column with incompatible number of rows (%d), expecting : %d", nr, data.nrows());
      }
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

  DataFrameSubsetVisitors visitors(data, data.names());
  List res = visitors.subset(index, get_class(data));

  if (is<GroupedDataFrame>(data)) {
    // so that all attributes are recalculated (indices ... )
    // see the lazyness feature in GroupedDataFrame
    // if we don't do that, we get the values of the un-arranged data
    // set for free from subset (#1064)
    res.attr("labels") = R_NilValue;
    copy_vars(res, data);
    return GroupedDataFrame(res).data();
  }
  else {
    SET_ATTRIB(res, strip_group_attributes(res));
    return res;
  }
}
