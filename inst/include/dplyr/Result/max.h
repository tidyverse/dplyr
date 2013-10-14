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

#ifndef dplyr_Result_Max_H
#define dplyr_Result_Max_H

namespace dplyr {
          
    template <int RTYPE, bool NA_RM>
    class Max : public Processor<RTYPE, Max<RTYPE,NA_RM> > {
    public:
        typedef typename Rcpp::traits::storage_type<RTYPE>::type STORAGE ;
        
        Max(SEXP x) : data_ptr( Rcpp::internal::r_vector_start<RTYPE>(x) ) {}
        ~Max(){}
        
        STORAGE process_chunk( const std::vector<int>& indices ){
            int n = indices.size() ;
        
            // find the first non NA value
            STORAGE res = data_ptr[ indices[0] ] ;
            int i=1 ;
            while( i<n && Rcpp::Vector<RTYPE>::is_na(res) ){
                res = data_ptr[ indices[i++] ] ;    
            }
            
            // we enter this loop if we did not scan the full vector
            if( i < n ) for( ; i<n; i++){
                STORAGE current = data_ptr[indices[i]] ;
                if( !Rcpp::Vector<RTYPE>::is_na(current) && internal::is_smaller<RTYPE>( res, current ) ) res = current ;
            }
            return res ;
        }
         
    private:
        STORAGE* data_ptr ; 
    } ;
     
    // quit early version for NA_RM = false
    template <int RTYPE>
    class Max<RTYPE,false> : public Processor<RTYPE, Max<RTYPE,false> > {
    public:
        typedef typename Rcpp::traits::storage_type<RTYPE>::type STORAGE ;
        
        Max(SEXP x) : data_ptr( Rcpp::internal::r_vector_start<RTYPE>(x) ) {}
        ~Max(){}
        
        STORAGE process_chunk( const std::vector<int>& indices ){
            int n = indices.size() ;
        
            // find the first non NA value
            STORAGE res = data_ptr[ indices[0] ] ;
            if( Rcpp::Vector<RTYPE>::is_na(res) ) return res;
            
            for( int i=1; i<n; i++){
                STORAGE current = data_ptr[indices[i]] ;
                if( Rcpp::Vector<RTYPE>::is_na(current) ) return current ;
                if( internal::is_smaller<RTYPE>( res, current ) ) res = current ;
            }
            return res ;
        }
         
    private:
        STORAGE* data_ptr ;
    } ;
     
    
}

#endif
