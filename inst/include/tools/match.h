#ifndef dplyr_tools_match_h
#define dplyr_tools_match_h


namespace dplyr {

  inline IntegerVector r_match(SEXP x, SEXP y) {
    static Function match("match", R_BaseEnv);
    return match(x, y, NA_INTEGER, CharacterVector());
  }

  inline SEXP r_match_call(SEXP definition, SEXP call) {
    static Function match_call("match.call", R_BaseEnv);
    return match_call(definition, Rf_lang2(R_QuoteSymbol, call));
  }

}

#endif
