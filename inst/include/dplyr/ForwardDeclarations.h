#ifndef dplyr_dplyr_ForwardDeclarations_H
#define dplyr_dplyr_ForwardDeclarations_H

namespace dplyr {
  class LazySubsets;
  Symbol extract_column(SEXP, const Environment&);
  Symbol get_column(SEXP, const Environment&, const LazySubsets&);
  class Result;
  class ResultSet;
  class Reducer_Proxy;
  class DataFrameVisitors;
  class DataFrameJoinVisitors;
  std::string get_single_class(SEXP x);

  void strip_index(DataFrame x);
  template <typename Index>
  DataFrame subset(DataFrame df, const Index& indices, CharacterVector classes);
  void check_attribute_compatibility(SEXP left, SEXP right);
  bool same_levels(SEXP left, SEXP right);
}

typedef dplyr::Result* (*HybridHandler)(SEXP, const dplyr::LazySubsets&, int);

#endif // #ifndef dplyr_dplyr_ForwardDeclarations_H
