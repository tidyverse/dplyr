#include <Rcpp.h>
#include <dplyr.h>

using namespace Rcpp ;

namespace dplyr {

    namespace internal {
        
        template <>
        int sum<INTSXP,true>( int* ptr, const Index_0_based& indices) {
            long double res = 0 ;
            int n = indices.size() ;
            for( int i=0; i<n; i++){
                int value = ptr[indices[i]] ;
                if( ! Rcpp::traits::is_na<INTSXP>( value ) ) res += value ;    
            }
            if(res > INT_MAX || res <= INT_MIN){
                return IntegerVector::get_na() ;   
            }
            return (int)res ;    
        }
    
        template <>
        int sum<INTSXP, false>( int* ptr, const Index_0_based& indices ){
            int res = 0 ;
            int n = indices.size() ;
            for( int i=0; i<n; i++){
                int value = ptr[indices[i]] ;
                if( Rcpp::traits::is_na<INTSXP>( value ) ){
                    return NA_INTEGER ;    
                }
                res += value ;    
            }
            return res ;
        }
        
        template <>
        double sum<REALSXP, false>( double* ptr, const Index_0_based& indices ){
            long double res = 0.0 ;
            int n = indices.size() ;
            for( int i=0; i<n; i++){
                // we don't test for NA here because += NA will give NA
                // this is faster in the most common case where there are no NA
                // if there are NA, we could return quicker as in the version for
                // INTSXP above, but we would penalize the most common case
                res += ptr[ indices[i] ] ;    
            }
            return (double)res ;
        }
        
    } 
    
    Subset* subset( SEXP x ) { 
        switch( TYPEOF(x) ){
            case INTSXP: return new SubsetTemplate<INTSXP>(x) ;
            case REALSXP: return new SubsetTemplate<REALSXP>(x) ;
            case STRSXP: return new SubsetTemplate<STRSXP>(x) ;
        }
        return 0 ;
    }
    
    
}      
