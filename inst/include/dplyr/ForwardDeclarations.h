#ifndef dplyr_dplyr_ForwardDeclarations_H
#define dplyr_dplyr_ForwardDeclarations_H

namespace dplyr {
  std::string get_single_class(SEXP x);
  void strip_index(DataFrame x);
  void check_attribute_compatibility(SEXP left, SEXP right);
  bool same_levels(SEXP left, SEXP right);
}

#endif // #ifndef dplyr_dplyr_ForwardDeclarations_H
