#ifndef dplyr_dplyr_HybridHandler_H
#define dplyr_dplyr_HybridHandler_H

namespace dplyr {
  class LazySubsets;
  class Result;
}

typedef dplyr::Result* (*HybridHandler)(SEXP, const dplyr::LazySubsets&, int);

#endif // dplyr_dplyr_HybridHandlerMap_H
