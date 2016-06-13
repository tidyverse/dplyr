#ifndef dplyr_collapse_H
#define dplyr_collapse_H

namespace Rcpp {

    template <int RTYPE>
    const char* toString( typename ::Rcpp::traits::storage_type<RTYPE>::type from){
        SEXP s = internal::r_coerce<RTYPE,STRSXP>( from ) ;
        return CHAR(s) ;
    }

    template <int RTYPE>
    std::string collapse( const Vector<RTYPE>& x, const char* sep = ", " ){
        std::stringstream ss;
        int n = x.size() ;
        if( n > 0){
            ss << toString<RTYPE>(x[0]) ;
            for( int i=1; i<n; i++) {
                const char* st = toString<RTYPE>(x[i]) ;
                ss << sep << st ;
            }
        }

        return ss.str();
    }

}
#endif
