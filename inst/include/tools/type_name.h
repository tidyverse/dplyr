#ifndef dplyr_tools_type_name_H
#define dplyr_tools_type_name_H

inline const char* type_name(SEXP x){
    #if RCPP_VERSION < Rcpp_Version(0,10,7)
        return sexp_to_name( TYPEOF(x) ) ;
    #else
        return type2name(x) ;
    #endif
}

#endif
