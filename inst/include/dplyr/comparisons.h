#ifndef dplyr_comparison_H
#define dplyr_comparison_H

namespace dplyr {

template <int RTYPE>
struct comparisons {
    typedef typename Rcpp::traits::storage_type<RTYPE>::type STORAGE ;
    
    inline bool is_less(STORAGE lhs, STORAGE rhs){
        if (is_na(lhs)) return false;
        if (is_na(rhs)) return true;
        
        return lhs < rhs ;   
    }
    
    inline bool is_greater(STORAGE lhs, STORAGE rhs){
        return lhs > rhs ;    
    }
    
    inline bool is_equal(STORAGE lhs, STORAGE rhs ){
        return lhs == rhs ;    
    }
    
    inline bool equal_or_both_na( STORAGE lhs, STORAGE rhs ){
        return lhs == rhs ;    
    }
    
    inline bool is_na(STORAGE x){ 
        return Rcpp::traits::is_na<RTYPE>(x); 
    }
    
} ;  

template <>
struct comparisons<STRSXP> {
    inline bool is_less( SEXP lhs, SEXP rhs ){
        // we need this because CHAR(NA_STRING) gives "NA"
        if (is_na(lhs)) return false;
        if (is_na(rhs)) return true;
        return strcmp( CHAR(lhs), CHAR(rhs) ) < 0 ;   
    }
    
    inline bool is_greater( SEXP lhs, SEXP rhs){
        if (is_na(lhs)) return false;
        if (is_na(rhs)) return true;
        return strcmp( CHAR(lhs), CHAR(rhs) ) > 0;    
    }
    
    inline bool is_equal(SEXP lhs, SEXP rhs ){
        return lhs == rhs ;    
    }
    
    inline bool equal_or_both_na( SEXP lhs, SEXP rhs ){
        return lhs == rhs ;    
    }
    
    inline bool is_na(SEXP x){ 
        return Rcpp::traits::is_na<STRSXP>(x); 
    }
    
} ;

// taking advantage of the particularity of NA_REAL
template <>
struct comparisons<REALSXP> {
    inline bool is_less(double lhs, double rhs){
        return !( rhs <= lhs );    
    }
    
    inline bool is_greater(double lhs, double rhs){
        return !( rhs >= lhs ) ;    
    }
    
    inline bool is_equal(double lhs, double rhs ){
        return lhs == rhs ;    
    }
    
    inline bool equal_or_both_na( double lhs, double rhs ){
        return lhs == rhs || ( is_na(lhs) && is_na(rhs) );    
    }
    
    inline bool is_na(double x){ 
        return Rcpp::traits::is_na<REALSXP>(x); 
    }
 
} ;


}

#endif
