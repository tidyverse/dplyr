#ifndef dplyr_tools_GroupFilterIndices_H
#define dplyr_tools_GroupFilterIndices_H

#include <tools/SlicingIndex.h>

namespace dplyr{

class GroupFilterIndices {
public:
  int ngroups;

  std::vector<const SlicingIndex*> old_indices;
  Rcpp::List tests;
  Rcpp::List new_indices;
  Rcpp::IntegerVector group_sizes;
  int biggest_group_size ;

private:

  int k;

public:

  GroupFilterIndices(int ngroups_) :
    ngroups(ngroups_),

    old_indices(ngroups),
    tests(ngroups),
    new_indices(ngroups),
    group_sizes(ngroups),
    biggest_group_size(0),
    k(0)
  {}

  void empty_group(int i){
    group_sizes[i] = 0;
    new_indices[i] = Rcpp::IntegerVector::create();
  }

  void add_group(int i, const SlicingIndex* old_idx, int n) {
    old_indices[i] = old_idx;
    group_sizes[i] = n;
    new_indices[i] = Rcpp::seq(k, k+n-1) ;
    if (biggest_group_size < n) biggest_group_size = n;
    k += n ;
  }

  void add_group_lgl(int i, const SlicingIndex* old_idx, int n, Rcpp::LogicalVector g_test){
    add_group(i, old_idx, n) ;
    tests[i] = g_test;
  }

  inline int size() const {
    return k;
  }

  inline bool is_full(int i) const {
    return group_sizes[i] == old_indices[i]->size();
  }

};

}
#endif
