#ifndef dplyr_check_supported_type_H
#define dplyr_check_supported_type_H

namespace dplyr {

  inline void check_supported_type(SEXP x, SEXP name){
    switch( TYPEOF(x) ){
      case INTSXP:
      case REALSXP:
      case LGLSXP:
      case STRSXP:
        break ;
      default:
      std::stringstream ss; 
      ss << "unsupported type for column '"
         << CHAR(name)
         << "' ("
         << type2name(x) ;
      SEXP classes = Rf_getAttrib(x, R_ClassSymbol ) ;
      if( !Rf_isNull(classes) ){   
        ss << ",  classes = " << collapse<STRSXP>(classes) ;
      }
      ss << ")" ;
      stop( ss.str() ) ;
    }   
  }

}
#endif
