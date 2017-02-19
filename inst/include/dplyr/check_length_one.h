#ifndef dplyr_check_length_one_H
#define dplyr_check_length_one_H

#include <dplyr/check_length.h>

namespace dplyr {

  void check_length_one(SEXP x) {
    check_length(Rf_length(x), 1, "a summary value");
  }

}
#endif
