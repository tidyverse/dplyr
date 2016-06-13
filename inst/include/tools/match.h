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

inline IntegerVector r_match( SEXP x, SEXP y ) {
  static RMatch m;
  return m(x, y);
}

}

#endif
