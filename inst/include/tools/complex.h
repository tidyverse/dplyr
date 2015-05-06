#ifndef dplyr_tools_complex_H
#define dplyr_tools_complex_H

inline std::ostream & operator<<(std::ostream &os, const Rcomplex& cplx ){
    return os << cplx.r << "+" << cplx.i << "i" ;
}

#endif
