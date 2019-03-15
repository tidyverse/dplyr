#include "pch.h"
#include <dplyr/main.h>

#include <dplyr/visitor_set/VisitorSetIndexSet.h>

#include <dplyr/visitors/vector/MultipleVectorVisitors.h>
#include <dplyr/hybrid/scalar_result/n_distinct.h>

#include <dplyr/visitors/subset/DataFrameSelect.h>
#include <dplyr/visitors/subset/DataFrameSubsetVisitors.h>

using namespace Rcpp;
using namespace dplyr;

SEXP select_not_grouped(const DataFrame& df, const SymbolVector& keep, const SymbolVector& new_names);

// [[Rcpp::export(rng = false)]]
SEXP distinct_impl(DataFrame df, const IntegerVector& vars, const IntegerVector& keep, SEXP frame) {
  if (df.size() == 0) {
    DataFrame res = DataFrame::create();
    copy_most_attributes(res, df);
    set_rownames(res, df.nrows() == 0 ? 0 : 1);
    return res ;
  }

  // No vars means ungrouped data with keep_all = TRUE.
  if (vars.size() == 0)
    return df;

  check_valid_colnames(df, true);
  DataFrameVisitors visitors(df, vars);

  int n = df.nrows();

  // allocate a big enough vector
  IntegerVector indices(n);
  VisitorSetIndexSet<DataFrameVisitors> set(visitors);

  int k = 0;
  for (int i = 0; i < n; i++) {
    if (set.insert(i).second) {
      indices[k++] = i + 1;
    }
  }

  // but then pretend it is smaller in case it is used in R subscripting
  SETLENGTH(indices, k);

  SEXP res = DataFrameSubsetVisitors(DataFrameSelect(df, keep), frame).subset_all(indices);

  // restore original length for GC bookkeeping
  SETLENGTH(indices, n);

  return res;
}

// [[Rcpp::export(rng = false)]]
int n_distinct_multi(List variables, bool na_rm = false) {
  if (variables.length() == 0) {
    stop("Need at least one column for `n_distinct()`");
  }

  int n = variables.size();

  // get the number of rows of an hypothetical data frame
  // that would contain variables, taking into account potential
  // recycling of length 1 variables
  int length = get_size(variables[0]);

  for (int i = 1; i < n; i++) {
    int l = get_size(variables[i]);
    if (length == l) continue;

    if (length == 1 && l > 1) {
      length = l;
    }
  }

  MultipleVectorVisitors visitors(variables, length, 1);

  typedef VisitorHash<MultipleVectorVisitors> Hash;
  typedef VisitorEqualPredicate<MultipleVectorVisitors> Pred;
  typedef dplyr_hash_set<int, Hash, Pred > Set;

  Set set(n, Hash(visitors), Pred(visitors));
  for (int i = 0; i < length; i++) {
    if (!na_rm || !visitors.is_na(i)) set.insert(i);
  }
  return set.size();
}
