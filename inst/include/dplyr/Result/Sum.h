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

#ifndef dplyr_Result_Sum_H
#define dplyr_Result_Sum_H

namespace dplyr {
      
namespace internal {
    
    // this one is actually only used for RTYPE = REALSXP and NA_RM = true
    template <int RTYPE, bool NA_RM> 
    typename Rcpp::traits::storage_type<RTYPE>::type
    sum(typename Rcpp::traits::storage_type<RTYPE>::type* ptr,  const Index_0_based& indices) {
        long double res = 0 ;
        int n = indices.size() ;
        for( int i=0; i<n; i++){
            double value = ptr[indices[i]] ;
            if( ! Rcpp::traits::is_na<RTYPE>( value ) ) res += value ;    
        }
        return (double)res ;
    }
    
    template <>
    int sum<INTSXP,true>( int* ptr, const Index_0_based& indices) ;
    
    template <>
    int sum<INTSXP, false>( int* ptr, const Index_0_based& indices ) ;
    
    template <>
    double sum<REALSXP, false>( double* ptr, const Index_0_based& indices ) ;
    
    
} // namespace internal
           
    template <int RTYPE, bool NA_RM>
    class Sum : public Processor< RTYPE, Sum<RTYPE,NA_RM> > {
    public:
        typedef typename Rcpp::traits::storage_type<RTYPE>::type STORAGE ;
        
        Sum(SEXP x) : data_ptr( Rcpp::internal::r_vector_start<RTYPE>(x) ) {}
        ~Sum(){}
        
        inline STORAGE process_chunk( const Index_0_based& indices ){
            return internal::sum<RTYPE,NA_RM>(data_ptr, indices) ;    
        }
        
        STORAGE* data_ptr ;
    } ;

}

#endif
