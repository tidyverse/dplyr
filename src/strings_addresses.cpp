#include <Rcpp.h>
using namespace Rcpp ;

// [[Rcpp::export]]
CharacterVector strings_addresses(CharacterVector s){
    static char buffer[20] ;
    int n = s.size() ;
    
    CharacterVector res(n) ;
    for( int i=0; i<n; i++){
        SEXP x = s[i] ;
        snprintf( buffer, 20, "%p", reinterpret_cast<void*>(x) ) ;
        res[i] = buffer ;
    }
    res.names() = s ;
    
    return res ;
}

