#ifndef dplyr_CallProxy_H
#define dplyr_CallProxy_H

#include <dplyr/Result/GroupedCallProxy.h>

namespace dplyr {

  typedef GroupedCallProxy<Rcpp::DataFrame, LazySubsets> CallProxy;

}

#endif
