#include <dplyr.h>
using namespace dplyr ;
using namespace Rcpp ;

namespace dplyr{

    template <int RTYPE>
    inline size_t hash_int_double( JoinVisitorImpl<RTYPE,REALSXP>& joiner, int i ){
        if( i>=0 ){
            int val = joiner.left[i] ;
            if( val == NA_INTEGER ) return joiner.RHS_hash_fun( NA_REAL );
            return joiner.RHS_hash_fun( (double)val );
        }
        return joiner.RHS_hash_fun( joiner.right[-i-1] ) ; 
    }
    
    
    template <>
    inline size_t JoinVisitorImpl<INTSXP,REALSXP>::hash(int i){
        return  hash_int_double<INTSXP>( *this, i );  
    }
    template <>
    inline size_t JoinVisitorImpl<LGLSXP,REALSXP>::hash(int i){
        return  hash_int_double<LGLSXP>( *this, i );  
    }
    
    template <int RTYPE>
    inline SEXP subset_join_int_double( JoinVisitorImpl<RTYPE,REALSXP>& joiner, const std::vector<int>& indices ){
        int n = indices.size() ;
        NumericVector res = no_init(n) ;
        for( int i=0; i<n; i++) {
            int index = indices[i] ;
            if( index >= 0 ){  
                res[i] = Rcpp::internal::r_coerce<INTSXP,REALSXP>( joiner.left[index] ) ;
            } else {
                res[i] = joiner.right[-index-1] ;    
            }
        }
        return res ;    
    }
    
    template <>
    inline SEXP JoinVisitorImpl<INTSXP,REALSXP>::subset( const std::vector<int>& indices ){
        return subset_join_int_double<INTSXP>( *this, indices ) ;
    }
    template <>
    inline SEXP JoinVisitorImpl<LGLSXP,REALSXP>::subset( const std::vector<int>& indices ){
        return subset_join_int_double<LGLSXP>( *this, indices ) ;
    }
    
    template <>
    inline SEXP JoinVisitorImpl<INTSXP,REALSXP>::subset( const VisitorSetIndexSet<DataFrameJoinVisitors>& set ){
        int n = set.size() ;
        NumericVector res = no_init(n) ;
        VisitorSetIndexSet<DataFrameJoinVisitors>::const_iterator it=set.begin() ;
        for( int i=0; i<n; i++, ++it) {
            int index = *it ;
            if( index >= 0){
                res[i] = Rcpp::internal::r_coerce<INTSXP,REALSXP>( left[index] ) ;    
            } else {
                res[i] = right[-index-1] ;    
            }
        }
        return res ;         
    }
    
    
    
    inline void incompatible_join_visitor(SEXP left, SEXP right, const std::string& name) {
        std::stringstream s ;
        s << "Can't join on '" << name << "' because of incompatible types (" << get_single_class(left) << "/" << get_single_class(right) << ")" ;
        stop( s.str() ) ;    
    }
    
    JoinVisitor* join_visitor( SEXP left, SEXP right, const std::string& name){
        // if( TYPEOF(left) != TYPEOF(right) ){
        //     std::stringstream ss ;
        //     ss << "Can't join on '" 
        //        << name 
        //        << "' because of incompatible types (" 
        //        << get_single_class(left) 
        //        << "/" 
        //        << get_single_class(right) 
        //        << ")" ;
        //     stop( ss.str() ) ;
        // }
        switch( TYPEOF(left) ){
            case INTSXP:
                {
                    bool lhs_factor = Rf_inherits( left, "factor" ) ;
                    switch( TYPEOF(right) ){
                        case INTSXP:
                            {
                                bool rhs_factor = Rf_inherits( right, "factor" ) ;
                                if( lhs_factor && rhs_factor){
                                    return new JoinFactorFactorVisitor(left, right) ;
                                } else if( !lhs_factor && !rhs_factor) {
                                    return new JoinVisitorImpl<INTSXP, INTSXP>( left, right ) ;
                                }
                                break ;
                            }
                        case REALSXP:   
                            {
                                if( lhs_factor ){ 
                                    incompatible_join_visitor(left, right, name) ;
                                } else if( is_bare_vector(right) ) {
                                    return new JoinVisitorImpl<INTSXP, REALSXP>( left, right) ;
                                } else {
                                    incompatible_join_visitor(left, right, name) ;
                                }
                                break ;
                                // what else: perhaps we can have INTSXP which is a Date and REALSXP which is a Date too ?
                            }
                        case LGLSXP:  
                            {
                                if( lhs_factor ){
                                    incompatible_join_visitor(left, right, name) ;
                                }
                                break ;
                            }
                        default: break ;
                    }
                    break ;  
                }
            case REALSXP:
                {
                    if( Rf_inherits( left, "Date" ) )
                        return new DateJoinVisitor(left, right ) ;
                    if( Rf_inherits( left, "POSIXct" ) )
                        return new POSIXctJoinVisitor(left, right ) ;
                    
                    return new JoinVisitorImpl<REALSXP,REALSXP>( left, right ) ;
                }
            case LGLSXP:  
                {
                    return new JoinVisitorImpl<LGLSXP,LGLSXP> ( left, right ) ;
                }
            case STRSXP:  
                {
                    return new JoinVisitorImpl<STRSXP,STRSXP> ( left, right ) ;
                }
            default: break ;
        }
        
        incompatible_join_visitor(left, right, name) ;
        return 0 ;
    }

}
