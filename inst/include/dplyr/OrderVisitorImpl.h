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
        return compare::equal_or_both_na( vec[i], vec[j] ) ;
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
        return compare::equal_or_both_na(vec[i], vec[j]) ;
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
