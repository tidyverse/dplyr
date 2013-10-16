// Copyright (C) 2013    Romain Francois
// Copyright (C) 2013    Rice University
//
// This file is part of dplyr.
//
// dplyr is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// dplyr is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with dplyr.  If not, see <http://www.gnu.org/licenses/>.

#include <Rcpp.h>
#include <dplyr.h>

using namespace Rcpp ;

namespace dplyr {

    namespace internal {
        
        template <>
        double mean<INTSXP,false>( int* ptr, const Index_1_based& indices ){
            long double res = 0.0 ;
            int n = indices.size() ;
            for( int i=0; i<n; i++){
                int value = ptr[ indices[i] ] ;
                // need to handle missing value specifically
                if( value == NA_INTEGER ){
                    return NA_REAL ;
                }   
                res += value ;
            }
            res /= n ;
            
            if(R_FINITE((double)res)) {
                long double t = 0.0 ;
                for (int i = 0; i<n; i++) {
                    t += ptr[indices[i]] - res;
                }
                res += t/n;
	        }
	        return (double)res ;
        }
        
        template <>
        double mean<REALSXP,false>( double* ptr, const Index_1_based& indices ){
            long double res = 0.0 ;
            int n = indices.size() ;
            for( int i=0; i<n; i++){
                res += ptr[ indices[i] ] ;
            }
            res /= n ;
            
            if(R_FINITE((double)res)) {
                long double t = 0.0 ;
                for (int i = 0; i<n; i++) {
                    t += ptr[indices[i]] - res;
                }
                res += t/n;
	        }
	        return (double)res ;
        }
        
        template <>
        int sum<INTSXP,true>( int* ptr, const Index_1_based& indices) {
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
        int sum<INTSXP, false>( int* ptr, const Index_1_based& indices ){
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
        double sum<REALSXP, false>( double* ptr, const Index_1_based& indices ){
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
