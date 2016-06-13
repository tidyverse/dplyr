#ifndef dplyr_comparisons_different_H
#define dplyr_comparisons_different_H

namespace dplyr {

    // not defined on purpose
    template <int LHS_RTYPE, int RHS_RTYPE>
    struct comparisons_different ;

    // specialization when LHS_TYPE == RHS_TYPE
    template <int RTYPE>
    struct comparisons_different<RTYPE,RTYPE> : comparisons<RTYPE>{} ;

    // works for both LHS_RTYPE = INTSXP and LHS_RTYPE = LGLSXP
    template <int LHS_RTYPE>
    struct comparisons_int_double {

        inline bool is_less( int lhs, double rhs ) const {
            if( lhs == NA_INTEGER ){
                return is_nan(rhs) ;
            }
            return !( (double)lhs >= rhs ) ;
        }

        inline bool is_greater( int lhs, double rhs ) const {
            if( lhs == NA_INTEGER ){
                return is_nan(rhs) ;
            }
            return !( (double)lhs <= rhs ) ;
        }

        inline bool is_nan(double x) const {
            return Rcpp::traits::is_nan<REALSXP>(x) ;
        }

        inline bool equal_or_both_na( int lhs, double rhs ) const {
            if( lhs == NA_INTEGER && ISNA(rhs) ) return true ;
            return (double)lhs == rhs ;
        }

    } ;

    template <>
    struct comparisons_different<INTSXP, REALSXP> : comparisons_int_double<INTSXP>{} ;

    template <>
    struct comparisons_different<LGLSXP, REALSXP> : comparisons_int_double<LGLSXP>{} ;



    template <int LHS_RTYPE>
    struct comparisons_double_int {

        inline bool is_less( double lhs, int rhs ) const {
            if( is_nan(lhs) || ISNA(lhs) ) return false ;
            if( rhs == NA_INTEGER ) return true ;
            return lhs < (double)rhs ;
        }

        inline bool is_greater( double lhs, int rhs ) const {
            if( is_nan(lhs) || ISNA(lhs) ) return false ;
            if( rhs == NA_INTEGER ) return true ;
            return lhs > (double)rhs ;
        }

        inline bool is_nan(double x) const {
            return Rcpp::traits::is_nan<REALSXP>(x) ;
        }

        inline bool equal_or_both_na( double lhs, int rhs ) const {
            if( rhs == NA_INTEGER && ISNA(lhs) ) return true ;
            return (double)rhs == lhs ;
        }

    } ;

    template <>
    struct comparisons_different<REALSXP, INTSXP> : comparisons_double_int<INTSXP>{} ;

    template <>
    struct comparisons_different<REALSXP, LGLSXP> : comparisons_double_int<LGLSXP>{} ;

    template <>
    struct comparisons_different<INTSXP, LGLSXP> : comparisons<INTSXP>{} ;

    template <>
    struct comparisons_different<LGLSXP, INTSXP> : comparisons<INTSXP>{} ;

}

#endif

