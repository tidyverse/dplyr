#ifndef dplyr_tools_match_h
#define dplyr_tools_match_h


namespace dplyr {

  inline IntegerVector r_match(SEXP x, SEXP y, SEXP incomparables = R_NilValue) {
    static Function match("match", R_BaseEnv);
    if (R_VERSION == R_Version(3, 3, 0)) {
      if (Rf_isNull(incomparables)) {
        return match(x, y, NA_INTEGER, LogicalVector());
      }
      else {
        return match(x, y, NA_INTEGER, incomparables);
      }
    }
    else {
      return match(x, y, NA_INTEGER, incomparables);
    }
  }

}

#endif
