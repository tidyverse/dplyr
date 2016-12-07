#ifndef dplyr_tools_match_h
#define dplyr_tools_match_h


namespace dplyr {

  class RMatch {
  public:
    RMatch() : match_fun("match", R_BaseEnv) {}
    IntegerVector operator()(SEXP x, SEXP y) {
      return match_fun(x, y, NA_INTEGER, CharacterVector());
    }

  private:
    Function match_fun;
  };

  inline IntegerVector r_match(SEXP x, SEXP y) {
    static RMatch m;
    return m(x, y);
  }

  class RMatchCall {
  public:
    RMatchCall() : match_call_fun("match.call", R_BaseEnv) {}
    //RMatchCall() : match_call_fun("match_call", Environment::namespace_env("dplyr")) {}
    SEXP operator()(SEXP definition, SEXP call) {
      return match_call_fun(definition, call);
    }

    Function match_call_fun;
  };

  inline SEXP r_match_call(SEXP definition, SEXP call) {
    static RMatchCall m;
    return m(definition, Rf_lang2(R_QuoteSymbol, call));
  }

}

#endif
