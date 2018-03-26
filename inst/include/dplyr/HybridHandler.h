#ifndef dplyr_dplyr_HybridHandler_H
#define dplyr_dplyr_HybridHandler_H

namespace dplyr {
class ILazySubsets;
class Result;

struct HybridHandler {
  typedef dplyr::Result* (*HybridHandlerFun)(SEXP, const dplyr::ILazySubsets&, int);

  HybridHandlerFun handler ;
  SEXP environment ;
  SEXP reference ;
  bool is_dplyr ;

  HybridHandler():
    handler(0),
    environment(Rcpp::Environment::empty_env()),
    reference(R_NilValue),
    is_dplyr(false)
  {}

  HybridHandler(HybridHandlerFun handler_, SEXP environment_, SEXP reference_):
    handler(handler_),
    environment(environment_),
    reference(reference_),
    is_dplyr(environment == Rcpp::Environment::namespace_env("dplyr"))
  {}

  bool hybrid(SEXP symbol, SEXP rho) const ;

};

}

#endif // dplyr_dplyr_HybridHandlerMap_H
