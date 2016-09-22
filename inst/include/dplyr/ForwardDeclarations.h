#ifndef dplyr_dplyr_ForwardDeclarations_H
#define dplyr_dplyr_ForwardDeclarations_H

namespace dplyr {
  class LazySubsets;
  class Result;
  class ResultSet;
  class Reducer_Proxy;
  class DataFrameVisitors;
  class DataFrameJoinVisitors;
  std::string get_single_class(SEXP x);

  void strip_index(DataFrame x);
  void check_attribute_compatibility(SEXP left, SEXP right);
  bool same_levels(SEXP left, SEXP right);
}

typedef dplyr::Result* (*HybridHandler)(SEXP, const dplyr::LazySubsets&, int);

#endif // #ifndef dplyr_dplyr_ForwardDeclarations_H
