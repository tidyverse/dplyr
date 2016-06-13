#ifndef dplyr_tools_complex_H
#define dplyr_tools_complex_H

#if RCPP_VERSION < Rcpp_Version(0,12,2)
inline std::ostream & operator<<(std::ostream &os, const Rcomplex& cplx ){
    return os << cplx.r << "+" << cplx.i << "i" ;
}
#endif

#endif
