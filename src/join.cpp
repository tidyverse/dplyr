#include <dplyr.h>
using namespace dplyr ;
using namespace Rcpp ;

namespace dplyr{

    inline bool is_bare_vector( SEXP x){
        SEXP att = ATTRIB(x) ;

        // only allow R_Names. as in R's do_isvector
        while( att != R_NilValue ){
            SEXP tag = TAG(att) ;
            if( !( tag == R_NamesSymbol || tag == Rf_install("comment") ) ) return false ;
            att = CDR(att) ;
        }

        return true ;
    }


    // -------------- (int,lgl)
    template <int LHS_RTYPE, int RHS_RTYPE>
    inline size_t hash_int_int( JoinVisitorImpl<LHS_RTYPE,RHS_RTYPE>& joiner, int i){
        return joiner.RHS_hash_fun( i>=0 ? joiner.left[i] : joiner.right[-i-1] ) ;
    }
    template <>
    inline size_t JoinVisitorImpl<INTSXP,LGLSXP>::hash( int i){
        return hash_int_int<INTSXP,LGLSXP>( *this, i) ;
    }
    template <>
    inline size_t JoinVisitorImpl<LGLSXP,INTSXP>::hash( int i){
        return hash_int_int<LGLSXP,INTSXP>( *this, i) ;
    }
    template <int LHS_RTYPE, int RHS_RTYPE>
    inline SEXP subset_join_int_int( JoinVisitorImpl<LHS_RTYPE,RHS_RTYPE>& joiner, const std::vector<int>& indices ){
        int n = indices.size() ;
        IntegerVector res = no_init(n) ;
        for( int i=0; i<n; i++) {
            int index = indices[i] ;
            if( index >= 0 ){
                res[i] = joiner.left[index] ;
            } else {
                res[i] = joiner.right[-index-1] ;
            }
        }
        return res ;
    }
    template <>
    inline SEXP JoinVisitorImpl<INTSXP,LGLSXP>::subset( const std::vector<int>& indices ){
        return subset_join_int_int<INTSXP,LGLSXP>( *this, indices ) ;
    }
    template <>
    inline SEXP JoinVisitorImpl<LGLSXP,INTSXP>::subset( const std::vector<int>& indices ){
        return subset_join_int_int<LGLSXP,INTSXP>( *this, indices ) ;
    }

    template <int LHS_RTYPE, int RHS_RTYPE>
    inline SEXP subset_join_int_int( JoinVisitorImpl<LHS_RTYPE,RHS_RTYPE>& joiner, const VisitorSetIndexSet<DataFrameJoinVisitors>& set ){
        int n = set.size() ;
        IntegerVector res = no_init(n) ;
        VisitorSetIndexSet<DataFrameJoinVisitors>::const_iterator it=set.begin() ;
        for( int i=0; i<n; i++, ++it) {
            int index = *it ;
            if( index >= 0 ){
                res[i] = joiner.left[index] ;
            } else {
                res[i] = joiner.right[-index-1] ;
            }
        }
        return res ;
    }
    template <>
    inline SEXP JoinVisitorImpl<INTSXP,LGLSXP>::subset( const VisitorSetIndexSet<DataFrameJoinVisitors>& set ){
        return subset_join_int_int<INTSXP,LGLSXP>( *this, set ) ;
    }
    template <>
    inline SEXP JoinVisitorImpl<LGLSXP,INTSXP>::subset( const VisitorSetIndexSet<DataFrameJoinVisitors>& set ){
        return subset_join_int_int<LGLSXP,INTSXP>( *this, set ) ;
    }


    // -------------- (int,double)
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


    template <int RTYPE>
    inline SEXP subset_join_int_double( JoinVisitorImpl<RTYPE,REALSXP>& joiner, const VisitorSetIndexSet<DataFrameJoinVisitors>& set ){
        int n = set.size() ;
        NumericVector res = no_init(n) ;
        VisitorSetIndexSet<DataFrameJoinVisitors>::const_iterator it=set.begin() ;
        for( int i=0; i<n; i++, ++it) {
            int index = *it ;
            if( index >= 0){
                res[i] = Rcpp::internal::r_coerce<INTSXP,REALSXP>( joiner.left[index] ) ;
            } else {
                res[i] = joiner.right[-index-1] ;
            }
        }
        return res ;
    }
    template <>
    inline SEXP JoinVisitorImpl<INTSXP,REALSXP>::subset( const VisitorSetIndexSet<DataFrameJoinVisitors>& set ){
        return  subset_join_int_double<INTSXP>(*this, set );
    }
    template <>
    inline SEXP JoinVisitorImpl<LGLSXP,REALSXP>::subset( const VisitorSetIndexSet<DataFrameJoinVisitors>& set ){
        return  subset_join_int_double<LGLSXP>(*this, set );
    }

    // -------------- (double,int)
    template <int RTYPE>
    inline size_t hash_double_int( JoinVisitorImpl<REALSXP,RTYPE>& joiner, int i ){
        // if(i < 0) we need to take data in right
        if( i<0 ){
            int val = joiner.right[-i-1] ;
            if( val == NA_INTEGER ) return joiner.LHS_hash_fun( NA_REAL );
            return joiner.LHS_hash_fun( (double)val );
        }
        // otherwise take data in left
        return joiner.LHS_hash_fun( joiner.left[i] ) ;
    }
    template <>
    inline size_t JoinVisitorImpl<REALSXP,INTSXP>::hash(int i){
        size_t res = hash_double_int<INTSXP>( *this, i );
        return res ;
    }
    template <>
    inline size_t JoinVisitorImpl<REALSXP,LGLSXP>::hash(int i){
        return  hash_double_int<LGLSXP>( *this, i );
    }


    template <int RTYPE>
    inline SEXP subset_join_double_int( JoinVisitorImpl<REALSXP,RTYPE>& joiner, const std::vector<int>& indices ){
        int n = indices.size() ;
        NumericVector res = no_init(n) ;
        for( int i=0; i<n; i++) {
            int index = indices[i] ;
            if( index < 0 ){
                res[i] = Rcpp::internal::r_coerce<INTSXP,REALSXP>( joiner.right[-index-1] ) ;
            } else {
                res[i] = joiner.left[index] ;
            }
        }
        return res ;
    }
    template <>
    inline SEXP JoinVisitorImpl<REALSXP,INTSXP>::subset( const std::vector<int>& indices ){
        return subset_join_double_int<INTSXP>( *this, indices ) ;
    }
    template <>
    inline SEXP JoinVisitorImpl<REALSXP,LGLSXP>::subset( const std::vector<int>& indices ){
        return subset_join_double_int<LGLSXP>( *this, indices ) ;
    }


    template <int RTYPE>
    inline SEXP subset_join_double_int( JoinVisitorImpl<REALSXP,RTYPE>& joiner, const VisitorSetIndexSet<DataFrameJoinVisitors>& set ){
        int n = set.size() ;
        NumericVector res = no_init(n) ;
        VisitorSetIndexSet<DataFrameJoinVisitors>::const_iterator it=set.begin() ;
        for( int i=0; i<n; i++, ++it) {
            int index = *it ;
            if( index < 0){
                res[i] = Rcpp::internal::r_coerce<INTSXP,REALSXP>( joiner.right[-index-1] ) ;
            } else {
                res[i] = joiner.left[index] ;
            }
        }
        return res ;
    }
    template <>
    inline SEXP JoinVisitorImpl<REALSXP,INTSXP>::subset( const VisitorSetIndexSet<DataFrameJoinVisitors>& set ){
        return  subset_join_double_int<INTSXP>(*this, set );
    }
    template <>
    inline SEXP JoinVisitorImpl<REALSXP,LGLSXP>::subset( const VisitorSetIndexSet<DataFrameJoinVisitors>& set ){
        return  subset_join_double_int<LGLSXP>(*this, set );
    }



    // -----------------
    inline void incompatible_join_visitor(SEXP left, SEXP right, const std::string& name_left, const std::string& name_right) {
        stop( "Can't join on '%s' x '%s' because of incompatible types (%s / %s)",
            name_left, name_right, get_single_class(left), get_single_class(right)
        ) ;
    }

    JoinVisitor* join_visitor( SEXP left, SEXP right, const std::string& name_left, const std::string& name_right, bool warn_ ){

        // handle Date separately
        bool lhs_date = Rf_inherits( left, "Date") ;
        bool rhs_date = Rf_inherits( right, "Date") ;

        switch( lhs_date + rhs_date ){
          case 2: return new DateJoinVisitor( left, right ) ;
          case 1: stop( "cannot join a Date object with an object that is not a Date object" ) ;
          case 0: break ;
          default: break ;
        }

        bool lhs_time = Rf_inherits( left, "POSIXct" );
        bool rhs_time = Rf_inherits( right, "POSIXct" );
        switch( lhs_time + rhs_time ){
          case 2: return new POSIXctJoinVisitor( left, right) ;
          case 1: stop( "cannot join a POSIXct object with an object that is not a POSIXct object" ) ;
          case 0: break;
          default: break ;
        }

        switch( TYPEOF(left) ){
            case CPLXSXP:
                {
                    switch( TYPEOF(right) ){
                    case CPLXSXP: return new JoinVisitorImpl<CPLXSXP, CPLXSXP>( left, right ) ;
                    default:
                        break ;
                    }
                    break ;
                }
            case INTSXP:
                {
                    bool lhs_factor = Rf_inherits( left, "factor" ) ;
                    switch( TYPEOF(right) ){
                        case INTSXP:
                            {
                                bool rhs_factor = Rf_inherits( right, "factor" ) ;
                                if( lhs_factor && rhs_factor){
                                    if( same_levels(left, right) ){
                                        return new JoinFactorFactorVisitor_SameLevels(left, right) ;
                                    } else {
                                        if(warn_) Rf_warning( "joining factors with different levels, coercing to character vector" );
                                        return new JoinFactorFactorVisitor(left, right) ;
                                    }
                                } else if( !lhs_factor && !rhs_factor) {
                                    return new JoinVisitorImpl<INTSXP, INTSXP>( left, right ) ;
                                }
                                break ;
                            }
                        case REALSXP:
                            {
                                if( lhs_factor ){
                                    incompatible_join_visitor(left, right, name_left, name_right) ;
                                } else if( is_bare_vector(right) ) {
                                    return new JoinVisitorImpl<INTSXP, REALSXP>( left, right) ;
                                } else {
                                    incompatible_join_visitor(left, right, name_left, name_right) ;
                                }
                                break ;
                                // what else: perhaps we can have INTSXP which is a Date and REALSXP which is a Date too ?
                            }
                        case LGLSXP:
                            {
                                if( lhs_factor ){
                                    incompatible_join_visitor(left, right, name_left, name_right) ;
                                } else {
                                    return new JoinVisitorImpl<INTSXP, LGLSXP>( left, right) ;
                                }
                                break ;
                            }
                        case STRSXP:
                            {
                                if( lhs_factor ){
                                    if(warn_) Rf_warning( "joining factor and character vector, coercing into character vector" ) ;
                                    return new JoinFactorStringVisitor( left, right );
                                }
                            }
                        default: break ;
                    }
                    break ;
                }
            case REALSXP:
                {

                    switch( TYPEOF(right) ){
                    case REALSXP:
                        {
                            if( is_bare_vector( right ) ){
                                return new JoinVisitorImpl<REALSXP, REALSXP>( left, right) ;
                            }

                            break ;
                        }
                    case INTSXP:
                        {
                            if( is_bare_vector(right) ){
                                return new JoinVisitorImpl<REALSXP, INTSXP>( left, right) ;
                            }
                        }
                    default: break ;
                    }

                }
            case LGLSXP:
                {
                    switch( TYPEOF(right) ){
                    case LGLSXP:
                        {
                            return new JoinVisitorImpl<LGLSXP,LGLSXP> ( left, right ) ;
                        }
                    case INTSXP:
                        {
                            if( is_bare_vector(right) ){
                                return new JoinVisitorImpl<LGLSXP, INTSXP>( left, right ) ;
                            }
                            break ;
                        }
                    case REALSXP:
                        {
                            if( is_bare_vector(right) ){
                                return new JoinVisitorImpl<LGLSXP, REALSXP>( left, right ) ;
                            }
                        }
                    default: break ;
                    }
                    break ;
                }
            case STRSXP:
                {
                    switch( TYPEOF(right) ){
                    case INTSXP:
                        {
                            if( Rf_inherits(right, "factor" ) ){
                                if(warn_) Rf_warning( "joining character vector and factor, coercing into character vector" ) ;
                                return new JoinStringFactorVisitor( left, right ) ;
                            }
                            break ;
                        }
                    case STRSXP:
                        {
                            return new JoinVisitorImpl<STRSXP,STRSXP> ( left, right ) ;
                        }
                    default: break ;
                    }
                    break ;
                }
            default: break ;
        }

        incompatible_join_visitor(left, right, name_left, name_right) ;
        return 0 ;
    }

}
