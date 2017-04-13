#ifndef DPLYR_WORKAROUND_XLEN_H
#define DPLYR_WORKAROUND_XLEN__H

namespace Rcpp {

template <>
inline SEXP wrap(const R_xlen_t& x) {
  if (x < -2147483647 || x > 2147483647) {
    return Rf_ScalarReal(static_cast<double>(x));
  }
  else {
    return Rf_ScalarInteger(static_cast<int>(x));
  }
}

}

#endif
