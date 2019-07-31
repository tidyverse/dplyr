#include "pch.h"
#include <dplyr/main.h>

#include <dplyr/visitor_set/VisitorSetIndexSet.h>

#include <dplyr/visitors/vector/MultipleVectorVisitors.h>
#include <dplyr/hybrid/scalar_result/n_distinct.h>

// [[Rcpp::export(rng = false)]]
int n_distinct_multi(Rcpp::List variables, bool na_rm = false) {
  if (variables.length() == 0) {
    Rcpp::stop("Need at least one column for `n_distinct()`");
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

  dplyr::MultipleVectorVisitors visitors(variables, length, 1);

  typedef dplyr::VisitorHash<dplyr::MultipleVectorVisitors> Hash;
  typedef dplyr::VisitorEqualPredicate<dplyr::MultipleVectorVisitors> Pred;
  typedef dplyr_hash_set<int, Hash, Pred > Set;

  Set set(n, Hash(visitors), Pred(visitors));
  for (int i = 0; i < length; i++) {
    if (!na_rm || !visitors.is_na(i)) set.insert(i);
  }
  return set.size();
}
