#ifndef dplyr_tools_is_lubridate_unsupported_h
#define dplyr_tools_is_lubridate_unsupported_h

#include <dplyr/symbols.h>

namespace dplyr {

inline bool is_lubridate_unsupported(SEXP x) {
  if (!Rf_inherits(x, "Period") && !Rf_inherits(x, "Interval")) return false ;
  SEXP cl = Rf_getAttrib(x, R_ClassSymbol) ;
  if (Rf_isNull(cl)) return false ;
  SEXP pkg = Rf_getAttrib(cl, symbols().package) ;
  if (Rf_isNull(pkg)) return false ;
  return STRING_ELT(pkg, 0) == Rf_mkChar("lubridate");
}

}

#endif
