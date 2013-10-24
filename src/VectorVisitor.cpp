#define COMPILING_DPLYR
#include <Rcpp.h>
#include <dplyr.h>

using namespace Rcpp ;
                
namespace dplyr {

    OrderVisitor* order_visitor( SEXP vec, bool ascending ){
        if( ascending )
            switch( TYPEOF(vec) ){
                case INTSXP:  return new OrderVectorVisitorImpl<INTSXP , true>( vec ) ;
                case REALSXP: return new OrderVectorVisitorImpl<REALSXP, true>( vec ) ;
                case LGLSXP:  return new OrderVectorVisitorImpl<LGLSXP , true>( vec ) ;
                case STRSXP:  return new OrderVectorVisitorImpl<STRSXP , true>( vec ) ;
                default: break ;
            }
        else 
            switch( TYPEOF(vec) ){
                case INTSXP:  return new OrderVectorVisitorImpl<INTSXP , false>( vec ) ;
                case REALSXP: return new OrderVectorVisitorImpl<REALSXP, false>( vec ) ;
                case LGLSXP:  return new OrderVectorVisitorImpl<LGLSXP , false>( vec ) ;
                case STRSXP:  return new OrderVectorVisitorImpl<STRSXP , false>( vec ) ;
                default: break ;
            }
        
        // should not happen
        return 0 ;
    }
    
}
