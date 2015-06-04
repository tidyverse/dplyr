#ifndef DPLYR_WORKAROUND_H
#define DPLYR_WORKAROUND_H

// Rcpp workaround - get rid of this once we have a released version 
//                   greater than 0.11.6
#if RCPP_DEV_VERSION > RcppDevVersion(0,11,6,0)
    #define SHALLOW_COPY(__NAME__,__SOURCE__) DataFrame __NAME__(shallow_copy(__SOURCE__))
#else
    #define SHALLOW_COPY(__NAME__,__SOURCE__)                       \
        SEXP shallow_copied = PROTECT( shallow_copy(__SOURCE__) ) ; \
        DataFrame __NAME__(shallow_copied) ;                        \
        UNPROTECT(1) 
#endif

#endif