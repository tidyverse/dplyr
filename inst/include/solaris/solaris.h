#ifndef DPLYR_SOLARIS_H
#define DPLYR_SOLARIS_H

#ifdef __SUNPRO_CC

namespace Rcpp{
namespace traits{

  template <> struct is_convertible< std::vector<int>, SEXP> : public true_type{} ;
  
}
}

#endif

#endif
