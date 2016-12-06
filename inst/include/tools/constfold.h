#ifndef dplyr_tools_constfold_h
#define dplyr_tools_constfold_h


namespace dplyr {

  class RConstFold {
  public:
    RConstFold() : constfold_fun("constfold", Environment::namespace_env("dplyr")) {}
    SEXP operator()(SEXP x) {
      return constfold_fun(x);
    }

  private:
    Function constfold_fun;
  };

  inline SEXP r_constfold(SEXP x) {
    static RConstFold constfold;
    return constfold(x);
  }

}

#endif
