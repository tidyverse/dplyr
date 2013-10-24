#define COMPILING_DPLYR
#include <Rcpp.h>
using namespace Rcpp;

// here is the code that will no longer be needed once
// we power tbl_df with the internal code

// [[Rcpp::export]]
std::vector<std::vector<int> > split_indices(IntegerVector group, int groups) {
  std::vector<std::vector<int> > ids(groups);

  int n = group.size();
  for (int i = 0; i < n; ++i) {
    ids[group[i] - 1].push_back(i + 1);
  }

  return ids;
}
