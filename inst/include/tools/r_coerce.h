#ifndef dplyr_tools_r_coerce_h
#define dplyr_tools_r_coerce_h

namespace Rcpp {
namespace internal {

template<>
inline int r_coerce<STRSXP, LGLSXP>(SEXP from) {
  return NA_LOGICAL;
}

template<>
inline int r_coerce<STRSXP, INTSXP>(SEXP from) {
  return NA_INTEGER;
}

template<>
inline double r_coerce<STRSXP, REALSXP>(SEXP from) {
  return NA_REAL;
}

template<>
inline Rcomplex r_coerce<STRSXP, CPLXSXP>(SEXP from) {
  return r_coerce<REALSXP, CPLXSXP>(NA_REAL);
}

template<>
inline Rbyte r_coerce<STRSXP, RAWSXP>(SEXP from) {
  return 0;
}

}
}

#endif
