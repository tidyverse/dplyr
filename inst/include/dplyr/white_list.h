#ifndef dplyr_white_list_H
#define dplyr_white_list_H

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

inline bool white_list(SEXP x){
    if( Rf_isMatrix(x) ) {
        // might have to refine later
        return true ;
    }
    switch( TYPEOF(x) ){
    case INTSXP:   return Rf_inherits(x, "Date") || Rf_inherits(x, "POSIXct" ) || Rf_inherits(x, "factor" ) || Rf_inherits(x, "AsIs") || is_bare_vector( x ) ;
    case REALSXP:  return Rf_inherits(x, "Date") || Rf_inherits(x, "POSIXct" ) || Rf_inherits(x,"difftime") || Rf_inherits(x, "AsIs") || is_bare_vector( x ) ;
    case LGLSXP:   return Rf_inherits(x, "AsIs") || is_bare_vector( x ) ;
    case STRSXP:   return Rf_inherits(x, "AsIs") || is_bare_vector( x ) ;
    case CPLXSXP:  return Rf_inherits(x, "AsIs") || is_bare_vector( x ) ;
    
    case VECSXP:   {
            if( Rf_inherits( x, "data.frame" ) ) return true ;
            return ! Rf_inherits(x, "POSIXlt") && is_bare_vector( x ) ;
    }
    
    default: break ;
    }
    return false ;
}

}
#endif

