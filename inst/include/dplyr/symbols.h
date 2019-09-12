#ifndef DPLYR_SYMBOLS_H
#define DPLYR_SYMBOLS_H

namespace dplyr {

struct symbols {
  static SEXP levels;
  static SEXP ptype;
};

struct vectors {
  static SEXP classes_vctrs_list_of;
  static SEXP empty_int_vector;
};

} // namespace dplyr


#endif
