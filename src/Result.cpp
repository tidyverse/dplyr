#include <Rcpp.h>
#include <dplyr.h>

using namespace Rcpp ;

namespace dplyr {

    Subset* subset( SEXP x ) { 
        switch( TYPEOF(x) ){
            case INTSXP: return new SubsetTemplate<INTSXP>(x) ;
            case REALSXP: return new SubsetTemplate<REALSXP>(x) ;
            case STRSXP: return new SubsetTemplate<STRSXP>(x) ;
        }
        return 0 ;
    }
    
}      
