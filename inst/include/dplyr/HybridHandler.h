#ifndef dplyr_dplyr_HybridHandler_H
#define dplyr_dplyr_HybridHandler_H

namespace dplyr {
class ILazySubsets;
class Result;

struct HybridHandler {
  typedef dplyr::Result* (*HybridHandlerFun)(SEXP, const dplyr::ILazySubsets&, int);

  HybridHandlerFun handler ;
  SEXP reference ;
  SEXP env ;

  HybridHandler():
    handler(0),
    env(Rcpp::Environment::empty_env()),
    reference(R_NilValue)
  {}

  HybridHandler(HybridHandlerFun handler_, SEXP env_, SEXP reference_):
    handler(handler_), env(env_), reference(reference_)
  {}

};

}

#endif // dplyr_dplyr_HybridHandlerMap_H
