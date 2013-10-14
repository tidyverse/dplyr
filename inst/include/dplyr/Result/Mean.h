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

#ifndef dplyr_Result_Mean_H
#define dplyr_Result_Mean_H

namespace dplyr {
namespace internal {
    
    // version for NA_RM == true
    template <int RTYPE, bool NA_RM> 
    double mean(typename Rcpp::traits::storage_type<RTYPE>::type* ptr,  const std::vector<int>& indices){
        typedef typename Rcpp::traits::storage_type<RTYPE>::type STORAGE ; 
        long double res = 0.0 ;
        int n = indices.size() ;
        int m = 0;
        for( int i=0; i<n; i++){
            STORAGE value = ptr[ indices[i] ] ;
            if( ! Rcpp::traits::is_na<RTYPE>( value ) ){
                res += value ;
                m++ ;
            }    
        }
        if( m == 0 ) return R_NaN ;
        res /= m  ;
        
        if(R_FINITE(res)) {
            long double t = 0.0 ;
            for (int i = 0; i<n; i++) {
                STORAGE value = ptr[indices[i]] ;
                if( ! Rcpp::traits::is_na<RTYPE>( value ) ){
                    t += value - res;
                }
            }
            res += t/m;
	    }
	        
        return (double)res ;
    }
    
    // special cases for NA_RM == false
    template <>
    double mean<INTSXP,false>( int* ptr, const std::vector<int>& indices ) ;
    
    template <>
    double mean<REALSXP,false>( double* ptr, const std::vector<int>& indices ) ;
    
    
} // namespace internal
           
    template <int RTYPE, bool NA_RM>
    class Mean : public Processor< REALSXP, Mean<RTYPE,NA_RM> > {
    public:
        typedef typename Rcpp::traits::storage_type<RTYPE>::type STORAGE ; 
        
        Mean(SEXP x) : data_ptr( Rcpp::internal::r_vector_start<RTYPE>(x) ) {}
        ~Mean(){}
        
        inline double process_chunk( const std::vector<int>& indices ){
            return internal::mean<RTYPE,NA_RM>(data_ptr, indices) ;    
        }
        
    private:
        STORAGE* data_ptr ;
        
    } ;

}

#endif
