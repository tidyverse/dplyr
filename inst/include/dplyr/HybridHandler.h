#ifndef dplyr_dplyr_HybridHandler_H
#define dplyr_dplyr_HybridHandler_H

namespace dplyr {
class ILazySubsets;
class Result;

typedef dplyr::Result* (*HybridHandlerFun)(SEXP, const dplyr::ILazySubsets&, int);

struct HybridHandler {
  HybridHandlerFun handler ;
  SEXP reference ;

  HybridHandler():
    handler(0),
    reference(R_NilValue)
  {}

  HybridHandler(HybridHandlerFun handler_, SEXP reference_):
    handler(handler_), reference(reference_)
  {}

};

}

#endif // dplyr_dplyr_HybridHandlerMap_H
