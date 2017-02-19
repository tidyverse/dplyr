#ifndef dplyr_check_length_H
#define dplyr_check_length_H

namespace dplyr {

  inline void check_length(const int actual, const int expected, const char* comment) {
    if (expected == 1) {
      if (actual != expected) {
        stop(
          "incompatible size (%d), expecting one (%s)",
          actual, comment
        );
      }
    }
    else {
      if (actual != expected) {
        stop(
          "incompatible size (%d), expecting %d (%s) or one",
          actual, expected, comment
        );
      }
    }
  }

}
#endif
