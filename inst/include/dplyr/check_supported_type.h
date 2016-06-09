#ifndef dplyr_check_supported_type_H
#define dplyr_check_supported_type_H

namespace dplyr {

  enum SupportedType {
    DPLYR_LGLSXP = LGLSXP,
    DPLYR_INTSXP = INTSXP,
    DPLYR_REALSXP = REALSXP,
    DPLYR_CPLXSXP = CPLXSXP,
    DPLYR_STRSXP = STRSXP,
    DPLYR_VECSXP = VECSXP
  };

  inline SupportedType check_supported_type(SEXP x, SEXP name = R_NilValue){
    switch( TYPEOF(x) ){
    case LGLSXP: return DPLYR_LGLSXP ;
    case INTSXP: return DPLYR_INTSXP ;
    case REALSXP: return DPLYR_REALSXP ;
    case CPLXSXP: return DPLYR_CPLXSXP ;
    case STRSXP: return DPLYR_STRSXP ;
    case VECSXP: return DPLYR_VECSXP ;
    default:
      if ( name == R_NilValue ) {
        stop( "Unsupported type %s", type2name(x)) ;
      }
      else {
        stop( "Unsupported type %s for column \"%s\"", type2name(x), CHAR(name)) ;
      }

      // Unreachable, can be removed with Rcpp > 0.12.5.2
      return DPLYR_LGLSXP ;
    }
  }

}
#endif
