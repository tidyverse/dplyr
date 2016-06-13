#include <Rcpp.h>
using namespace Rcpp;

const char* address(SEXP x){
  static char buffer[20] ;
  snprintf( buffer, 20, "%p", reinterpret_cast<void*>(x) ) ;
  return (const char*)buffer ;
}

// [[Rcpp::export]]
CharacterVector loc(RObject data) {
  CharacterVector out(1);
  out[0] = address(data);
  return out;
}

// [[Rcpp::export]]
CharacterVector dfloc(List df){
  int n = df.size() ;
  CharacterVector pointers(n);
  for( int i=0; i<n; i++) {
    pointers[i] = address(df[i]) ;
  }
  pointers.names() = df.names() ;
  return pointers ;
}

// [[Rcpp::export]]
CharacterVector plfloc(Pairlist data){
  int n = data.size() ;
  CharacterVector pointers(n), names(n) ;
  SEXP p = data ;
  int i=0 ;
  while( ! Rf_isNull(p) ){
    pointers[i] = address(CAR(p)) ;
    names[i] = PRINTNAME(TAG(p)) ;
    p = CDR(p) ;
    i++ ;
  }
  pointers.names() = names ;
  return pointers;
}
