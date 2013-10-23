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

#ifndef dplyr_VectorVisitor_Impl_H
#define dplyr_VectorVisitor_Impl_H

namespace dplyr {

template <typename Container>
inline int output_size( const Container& container){
    return container.size() ;
}

template <>
inline int output_size<LogicalVector>( const LogicalVector& container){
    return std::count( container.begin(), container.end(), 1 ) ;
}

/** 
 * Implementations 
 */
template <int RTYPE>
class VectorVisitorImpl : public VectorVisitor, public comparisons<RTYPE> {
    typedef comparisons<RTYPE> compare ;
public:
    typedef Rcpp::Vector<RTYPE> VECTOR ;
    
    /** 
     * The type of data : int, double, SEXP, Rcomplex
     */
    typedef typename Rcpp::traits::storage_type<RTYPE>::type STORAGE ;
    
    /**
     * Hasher for that type of data
     */
    typedef boost::hash<STORAGE> hasher ;
    
    VectorVisitorImpl( const VECTOR& vec_ ) : vec(vec_) {}
        
    /** 
     * implementations
     */
    size_t hash(int i){ 
        return hash_fun( vec[i] ) ;
    } 
    inline bool equal(int i, int j){ 
        return compare::is_equal( vec[i], vec[j] ) ;
    }
    
    inline bool less(int i, int j){ 
        return compare::is_less( vec[i], vec[j] ) ;
    }
    
    inline bool greater(int i, int j){ 
        return compare::is_greater( vec[i], vec[j] ) ;
    }
    
    inline SEXP subset( const Rcpp::IntegerVector& index){
        return subset_int_index( index) ;    
    }
    inline SEXP subset( const std::vector<int>& index){
        return subset_int_index( index) ;    
    }
    
    inline SEXP subset( const ChunkIndexMap& map ){
        int n = output_size(map) ;
        VECTOR out = Rcpp::no_init(n) ;
        ChunkIndexMap::const_iterator it = map.begin(); 
        for( int i=0; i<n; i++, ++it)
            out[i] = vec[ it->first ] ;
        set_factor( out ) ;
        return out ;
    }
    
    inline SEXP subset( const Rcpp::LogicalVector& index ){
        int n = output_size(index) ;
        VECTOR out = Rcpp::no_init(n) ;
        for( int i=0, k=0; k<n; k++, i++ ) {
            while( ! index[i] ) i++; 
            out[k] = vec[i] ;
        }
        set_factor( out ) ;
        return out ;
    }
    
    
private: 
    VECTOR vec ;
    hasher hash_fun ;
    
    template <typename Container>
    inline SEXP subset_int_index( const Container& index ) {
        int n = output_size(index) ;
        VECTOR out = Rcpp::no_init(n) ;
        // TODO: find a way to mark that we don't need the NA handling
        for( int i=0; i<n; i++) 
            out[i] = (index[i] < 0) ? VECTOR::get_na() : vec[ index[i] ] ;
        set_factor( out ) ;
        return out ;
    }
    
    void set_factor( VECTOR& out ){
        if( RTYPE == INTSXP && Rf_inherits(vec, "factor" ) ){
            out.attr( "levels" ) = vec.attr("levels") ;
            out.attr( "class"  ) = "factor" ;
        }
    }
    
    
} ;
    
}

#endif
