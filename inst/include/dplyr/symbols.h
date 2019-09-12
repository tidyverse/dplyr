#ifndef DPLYR_SYMBOLS_H
#define DPLYR_SYMBOLS_H

namespace dplyr {

struct symbols {
  static SEXP levels;
  static SEXP ptype;
  static SEXP names;
  static SEXP formula;
  static SEXP envir;
  static SEXP getNamespace;
  static SEXP dot_dot_group_size;
  static SEXP dot_dot_group_number;
};

struct fns {
  static SEXP quote;
  static SEXP rm;
};

struct strings {
  static SEXP POSIXct;
  static SEXP POSIXt;
  static SEXP Date;
  static SEXP factor;
  static SEXP ordered;
};

struct vectors {
  static SEXP factor;
  static SEXP ordered;
  static SEXP unbound_sentinel;
  static SEXP classes_vctrs_list_of;
  static SEXP empty_int_vector;

};

struct envs {
  static SEXP ns_dplyr;
  static SEXP ns_rlang;
  static SEXP ns_stats;

  static SEXP context_env;
};

} // namespace dplyr


#endif
