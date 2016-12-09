#ifndef dplyr_tools_constfold_h
#define dplyr_tools_constfold_h


namespace dplyr {

  inline SEXP r_constfold(SEXP x) {
    if (!Rf_isLanguage(x)) {
      return x;
    }

    static Function force("force", R_BaseEnv);
    try {
      return force(x);
    }
    catch (...) {
      return x;
    }
  }

}

#endif
