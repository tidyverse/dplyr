#include "pch.h"
#include <dplyr/main.h>

#include <tools/Quosure.h>
#include <tools/debug.h>
#include <tools/bad.h>

#include <dplyr/allow_list.h>
#include <dplyr/symbols.h>

#include <dplyr/data/GroupedDataFrame.h>
#include <dplyr/data/DataMask.h>

#include <dplyr/visitors/subset/DataFrameSubsetVisitors.h>
#include <dplyr/visitors/order/Order.h>

namespace dplyr {

int64_t comparisons_int64::NA_INT64 = std::numeric_limits<int64_t>::min();

template <typename SlicedTibble>
SEXP arrange_template(const SlicedTibble& gdf, const QuosureList& quosures, SEXP frame) {
  const Rcpp::DataFrame& data = gdf.data();
  if (data.size() == 0 || data.nrows() == 0)
    return data;

  int nargs = quosures.size();
  if (nargs == 0)
    return data;

  check_valid_colnames(data);
  assert_all_allow_list(data);
  Rcpp::List variables(nargs);
  Rcpp::LogicalVector ascending(nargs);

  NaturalDataFrame ndf(data);
  DataMask<NaturalDataFrame> mask(ndf);
  NaturalSlicingIndex indices_all(gdf.nrows());

  for (int i = 0; i < nargs; i++) {
    const NamedQuosure& named_quosure = quosures[i];

    SEXP expr = named_quosure.expr();

    bool is_desc = TYPEOF(expr) == LANGSXP && symbols::desc == CAR(expr);
    expr = is_desc ? CADR(expr) : expr ;

    Rcpp::RObject v(R_NilValue);

    // if expr is a symbol from the data, just use it
    if (TYPEOF(expr) == SYMSXP) {
      const ColumnBinding<NaturalDataFrame>* binding = mask.maybe_get_subset_binding(CHAR(PRINTNAME(expr)));
      if (binding) {
        v = binding->get_data();
      }
    }

    // otherwise need to evaluate in the data mask
    mask.setup();
    if (v.isNULL()) {
      if (is_desc) {
        // we need a new quosure that peels off `desc` from the original
        // quosure, and uses the same environment
        Quosure quo(PROTECT(rlang::quo_set_expr(named_quosure.get(), expr)));
        v = mask.eval(quo, indices_all);
        UNPROTECT(1);
      } else {
        // just use the original quosure
        v = mask.eval(named_quosure.get(), indices_all);
      }
    }

    if (!allow_list(v)) {
      Rcpp::stop("cannot arrange column of class '%s' at position %d", get_single_class(v), i + 1);
    }

    if (Rf_inherits(v, "data.frame")) {
      bad_pos_arg(i + 1, "is of unsupported type data.frame");
    } else if (Rf_isMatrix(v)) {
      bad_pos_arg(i + 1, "is of unsupported type matrix");
    } else {
      if (Rf_length(v) != data.nrows()) {
        Rcpp::stop("incorrect size (%d) at position %d, expecting : %d", Rf_length(v), i + 1, data.nrows());
      }
    }

    variables[i] = v;
    ascending[i] = !is_desc;
  }
  Rf_namesgets(variables, quosures.names());
  OrderVisitors o(variables, ascending, nargs);
  Rcpp::IntegerVector one_based_index = o.apply();

  Rcpp::List res = DataFrameSubsetVisitors(data, frame).subset_all(one_based_index);

  // let the grouping class organise the rest (the groups attribute etc ...)
  return SlicedTibble(res, gdf).data();
}

}

// [[Rcpp::export(rng = false)]]
SEXP arrange_impl(Rcpp::DataFrame df, dplyr::QuosureList quosures, SEXP frame) {
  if (Rcpp::is<dplyr::RowwiseDataFrame>(df)) {
    return dplyr::arrange_template<dplyr::RowwiseDataFrame>(dplyr::RowwiseDataFrame(df), quosures, frame);
  } else if (Rcpp::is<dplyr::GroupedDataFrame>(df)) {
    return dplyr::arrange_template<dplyr::GroupedDataFrame>(dplyr::GroupedDataFrame(df), quosures, frame);
  } else {
    return dplyr::arrange_template<dplyr::NaturalDataFrame>(dplyr::NaturalDataFrame(df), quosures, frame);
  }
}

