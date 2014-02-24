#ifndef DPLYR_SOLARIS_H
#define DPLYR_SOLARIS_H

#ifdef __SUNPRO_CC

namespace Rcpp{
namespace traits{

  template <typename T> struct is_convertible< std::vector<T>, SEXP> : public false_type{} ;
  
}
}

#endif

#endif
