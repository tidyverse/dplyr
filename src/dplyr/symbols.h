#ifndef DPLYR_SYMBOLS_H
#define DPLYR_SYMBOLS_H

namespace dplyr {

struct symbols {
  static SEXP groups;
  static SEXP levels;
  static SEXP ptype;
  static SEXP vars;
};

struct vectors {
  static SEXP classes_vctrs_list_of;
  static SEXP classes_tbl_df;
  static SEXP empty_int_vector;
};

} // namespace dplyr


#endif
