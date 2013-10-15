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

#ifndef dplyr_OrderVectorVisitor_Impl_H
#define dplyr_OrderVectorVisitor_Impl_H

namespace dplyr {

// version used for ascending = true
template <int RTYPE, bool ascending>
class OrderVectorVisitorImpl : public OrderVisitor, public comparisons<RTYPE> {
    typedef comparisons<RTYPE> compare ;
public:
    typedef Rcpp::Vector<RTYPE> VECTOR ;
    
    /** 
     * The type of data : int, double, SEXP, Rcomplex
     */
    typedef typename Rcpp::traits::storage_type<RTYPE>::type STORAGE ;
    
    OrderVectorVisitorImpl( const VECTOR& vec_ ) : vec(vec_) {}
        
    inline bool equal(int i, int j){ 
        return compare::is_equal( vec[i], vec[j] ) ;
    }
    
    inline bool before(int i, int j){ 
        return compare::is_less( vec[i], vec[j] ) ;
    }
    
    SEXP get(){ return vec ; }
    
private: 
    VECTOR vec ;    
} ;
  
// version used for ascending = false
template <int RTYPE>
class OrderVectorVisitorImpl<RTYPE,false> : public OrderVisitor, public comparisons<RTYPE> {
    typedef comparisons<RTYPE> compare ;
public:
    typedef Rcpp::Vector<RTYPE> VECTOR ;
    
    /** 
     * The type of data : int, double, SEXP, Rcomplex
     */
    typedef typename Rcpp::traits::storage_type<RTYPE>::type STORAGE ;
    
    OrderVectorVisitorImpl( const VECTOR& vec_ ) : vec(vec_) {}
        
    inline bool equal(int i, int j){ 
        return compare::is_equal( vec[i], vec[j] ) ;
    }
    
    inline bool before(int i, int j){ 
        return compare::is_greater( vec[i], vec[j] ) ;
    }
    
    SEXP get(){ return vec ; }
    
private: 
    VECTOR vec ;    
} ;

}

#endif
