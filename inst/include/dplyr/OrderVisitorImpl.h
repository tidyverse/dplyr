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

class LazySubsetResult {
public:
    LazySubsetResult( OrderVisitor* v_, const Rcpp::IntegerVector& indices_ ):
        v(v_), indices(indices_){}
    
    friend bool operator==( const LazySubsetResult& lhs, const LazySubsetResult& rhs) ;
    
private:
    OrderVisitor* v ;
    const Rcpp::IntegerVector& indices ;
} ;
         
template <int RTYPE, typename STORAGE>
inline bool equal_or_both_na( STORAGE lhs, STORAGE rhs ){
    return lhs == rhs ;
}
template <>
inline bool equal_or_both_na<REALSXP,double>( double lhs, double rhs ){
    // special version for REALSXP
    return ( Rcpp::traits::is_na<REALSXP>( lhs ) && Rcpp::traits::is_na<REALSXP>( rhs ) ) || ( lhs == rhs );  
}


template <int RTYPE>
bool subset_equal( 
    Rcpp::Vector<RTYPE> lhs_data, const Rcpp::IntegerVector& lhs_ind, 
    Rcpp::Vector<RTYPE> rhs_data, const Rcpp::IntegerVector& rhs_ind )
{
    int n = lhs_data.size() ;
    for( int i=0; i<n; i++){
        if( ! equal_or_both_na<RTYPE, typename Rcpp::traits::storage_type<RTYPE>::type>( lhs_data[ lhs_ind[i] ], lhs_data[ lhs_ind[i] ] ) )
            return false ;
    }
    return true ;
}

inline bool operator==( const LazySubsetResult& lhs, const LazySubsetResult& rhs){
    int RTYPE = TYPEOF(lhs.v->get()) ;
    switch( RTYPE ){
    case INTSXP:  return subset_equal<INTSXP>( lhs.v->get(), lhs.indices, rhs.v->get(), rhs.indices ) ; 
    case REALSXP: return subset_equal<REALSXP>( lhs.v->get(), lhs.indices, rhs.v->get(), rhs.indices ) ;
    case STRSXP:  return subset_equal<STRSXP>( lhs.v->get(), lhs.indices, rhs.v->get(), rhs.indices ) ;
    case LGLSXP:  return subset_equal<LGLSXP>( lhs.v->get(), lhs.indices, rhs.v->get(), rhs.indices ) ;
    default:
        break ;
    }
    return false ;
}
inline bool operator!=( const LazySubsetResult& lhs, const LazySubsetResult& rhs){
    return ! ( lhs == rhs ) ;
}


}

#endif
