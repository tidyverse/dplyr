#include "pch.h"
#include <dplyr/main.h>

#include <tools/hash.h>
#include <tools/Quosure.h>
#include <tools/utils.h>
#include <tools/SymbolString.h>

#include <dplyr/GroupedDataFrame.h>

#include <dplyr/Result/LazyRowwiseSubsets.h>
#include <dplyr/Result/GroupedCallProxy.h>
#include <dplyr/Result/CallProxy.h>

#include <dplyr/bad.h>

using namespace Rcpp;
using namespace dplyr;

inline
SEXP empty_subset(const DataFrame& df, const CharacterVector& classes) {
  DataFrame res = DataFrameSubsetVisitors(df).subset(EmptySubset(), classes);
  strip_index(res);
  return res;
}

inline
void check_result_length(const LogicalVector& test, int n) {
  if (test.size() != n) {
    stop("Result must have length %d, not %d", n, test.size());
  }
}
inline
SEXP check_result_lgl_type(SEXP tmp) {
  if (TYPEOF(tmp) != LGLSXP) {
    bad_pos_arg(2, "filter condition does not evaluate to a logical vector");
  }
  return tmp;
}

template <typename SlicedTibble, typename Subsets>
DataFrame filter_grouped(const SlicedTibble& gdf, const NamedQuosure& quo) {
  typedef GroupedCallProxy<SlicedTibble, Subsets> Proxy;
  const DataFrame& data = gdf.data();

  LogicalVector test(data.nrows(), TRUE);
  LogicalVector g_test;
  Proxy call_proxy(quo.expr(), gdf, quo.env());

  int ngroups = gdf.ngroups();
  typename SlicedTibble::group_iterator git = gdf.group_begin();
  for (int i = 0; i < ngroups; i++, ++git) {
    const SlicingIndex& indices = *git;
    int chunk_size = indices.size();

    g_test = check_result_lgl_type(call_proxy.get(indices));
    if (g_test.size() == 1) {
      int val = g_test[0] == TRUE;
      for (int j = 0; j < chunk_size; j++) {
        test[indices[j]] = val;
      }
    } else {
      check_result_length(g_test, chunk_size);
      for (int j = 0; j < chunk_size; j++) {
        if (g_test[j] != TRUE) test[ indices[j] ] = FALSE;
      }
    }
  }

  // Subset the grouped data frame
  DataFrame res = subset(data, test, data.names(), classes_grouped<SlicedTibble>());
  copy_vars(res, data);
  strip_index(res);
  return SlicedTibble(res).data();
}

DataFrame filter_ungrouped(DataFrame df, const NamedQuosure& quo) {
  CallProxy proxy(quo.expr(), df, quo.env());
  LogicalVector test = check_result_lgl_type(proxy.eval());

  if (test.size() == 1) {
    if (test[0] == TRUE) {
      return df;
    } else {
      return empty_subset(df, classes_not_grouped());
    }
  } else {
    check_result_length(test, df.nrows());
    return subset(df, test, classes_not_grouped());
  }
}

// [[Rcpp::export]]
SEXP filter_impl(DataFrame df, NamedQuosure quo) {
  if (df.nrows() == 0 || Rf_isNull(df)) {
    return df;
  }
  check_valid_colnames(df);
  assert_all_white_list(df);

  if (is<GroupedDataFrame>(df)) {
    return filter_grouped<GroupedDataFrame, LazyGroupedSubsets>(GroupedDataFrame(df), quo);
  } else if (is<RowwiseDataFrame>(df)) {
    return filter_grouped<RowwiseDataFrame, LazyRowwiseSubsets>(RowwiseDataFrame(df), quo);
  } else {
    return filter_ungrouped(df, quo);
  }
}
