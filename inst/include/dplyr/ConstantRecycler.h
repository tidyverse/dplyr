#ifndef dplyr_ConstantRecycler_H
#define dplyr_ConstantRecycler_H

#include <Rcpp.h>
#include <tools/utils.h>

namespace dplyr {

template <int RTYPE>
class ConstantRecycler {
public:
  ConstantRecycler(SEXP constant_, int n_) :
    constant(constant_),
    n(n_)
  {}

  inline SEXP collect() {
    Rcpp::Vector<RTYPE> result(n, Rcpp::internal::r_vector_start<RTYPE>(constant)[0]);
    copy_most_attributes(result, constant);
    return result;
  }

private:
  SEXP constant;
  int n ;
};




}

#endif
