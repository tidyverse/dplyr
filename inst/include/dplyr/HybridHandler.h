#ifndef dplyr_dplyr_HybridHandler_H
#define dplyr_dplyr_HybridHandler_H

namespace dplyr {
class ILazySubsets;
class Result;

struct HybridHandler {
  typedef dplyr::Result* (*HybridHandlerFun)(SEXP, const dplyr::ILazySubsets&, int);

  enum Origin { DPLYR, STATS, BASE };

  HybridHandlerFun handler ;
  SEXP reference ;
  Origin origin ;

  HybridHandler():
    handler(0),
    reference(R_NilValue),
    origin(DPLYR)
  {}

  HybridHandler(HybridHandlerFun handler_, Origin origin_, SEXP reference_):
    handler(handler_),
    reference(reference_),
    origin(origin_)
  {}

  bool hybrid(SEXP symbol, SEXP rho) const;

};

}

#endif // dplyr_dplyr_HybridHandlerMap_H
