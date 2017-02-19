#ifndef dplyr_check_length_one_H
#define dplyr_check_length_one_H

namespace dplyr {

  void check_length_one(SEXP x) {
    if (Rf_length(x) != 1) {
      stop("expecting result of length one, got length %d", Rf_length(x));
    }
  }

}
#endif
