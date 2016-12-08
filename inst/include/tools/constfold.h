#ifndef dplyr_tools_constfold_h
#define dplyr_tools_constfold_h


namespace dplyr {

  inline SEXP r_constfold(SEXP x) {
    static Function constfold("constfold", "dplyr");
    SEXP ret = constfold(x);
    if (Rf_isNull(ret))
      return x;

    return ret;
  }

}

#endif
