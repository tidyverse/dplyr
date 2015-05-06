#ifndef dplyr_check_supported_type_H
#define dplyr_check_supported_type_H

namespace dplyr {

  inline void check_supported_type(SEXP x, SEXP name){
    switch( TYPEOF(x) ){
      case INTSXP:
      case REALSXP:
      case LGLSXP:
      case STRSXP:
      case VECSXP:
      case CPLXSXP: 
        return ;
      default:
        break ;
    }
    stop( "unsupported type for column '%s' (%s, classes = %s)", 
        CHAR(name), type2name(x), get_single_class(x) ) ;
  }

}
#endif
