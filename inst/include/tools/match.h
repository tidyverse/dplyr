#ifndef dplyr_tools_match_h
#define dplyr_tools_match_h


namespace dplyr {

inline IntegerVector r_match( SEXP x, SEXP y ) {
  return Language( "match", x, y, NA_INTEGER, CharacterVector() ).fast_eval(R_BaseEnv) ;
}

};

#endif
