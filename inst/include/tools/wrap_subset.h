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

#ifndef dplyr_wrap_subset_H
#define dplyr_wrap_subset_H

namespace dplyr {
    
    template <int RTYPE, typename Container>
    SEXP wrap_subset( SEXP input, const Container& indices ){
        int n = indices.size() ;
        Rcpp::Vector<RTYPE> res = Rcpp::no_init(n) ;
        typedef typename Rcpp::traits::storage_type<RTYPE>::type STORAGE ;
        STORAGE* ptr = Rcpp::internal::r_vector_start<RTYPE>( input ) ;
        for( int i=0; i<n; i++)
            res[i] = ptr[ indices[i] ] ;    
        return res ;
    }
    
    template <int RTYPE, typename Container>
    SEXP wrap_subset_1_based( SEXP input, const Container& indices ){
        int n = indices.size() ;
        Rcpp::Vector<RTYPE> res = Rcpp::no_init(n) ;
        typedef typename Rcpp::traits::storage_type<RTYPE>::type STORAGE ;
        STORAGE* ptr = Rcpp::internal::r_vector_start<RTYPE>( input ) ;
        for( int i=0; i<n; i++)
            res[i] = ptr[ indices[i]-1 ] ;    
        return res ;
    }
    
}


#endif
