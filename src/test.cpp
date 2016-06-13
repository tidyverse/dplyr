#include <dplyr.h>

using namespace Rcpp ;
using namespace dplyr ;

// [[Rcpp::export]]
LogicalVector test_comparisons(){
    dplyr::comparisons<REALSXP> comp ;
    return LogicalVector::create(
        comp.is_less( 1.0, 2.0 ),
        !comp.is_less( 2.0, 1.0 ),
        comp.is_less( NA_REAL, R_NaN ),
        ! comp.is_less( R_NaN, NA_REAL),
        ! comp.is_less( NA_REAL, 1.0 ),
        ! comp.is_less( R_NaN, 1.0 ),
        comp.is_less( 1.0, NA_REAL ),
        comp.is_less( 1.0, R_NaN )
        ) ;
}

