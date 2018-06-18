#ifndef dplyr_hybrid_hybrid_h
#define dplyr_hybrid_hybrid_h

#include <dplyr/hybrid/HybridVectorScalarResult.h>

#include <dplyr/hybrid/scalar_result/Count.h>

namespace dplyr{
namespace hybrid{

template <typename SlicedTibble, typename LazySubsets>
SEXP hybrid_summary(SEXP expr, const SlicedTibble& data, const LazySubsets& subsets, SEXP env){

  if(TYPEOF(expr) == LANGSXP && CAR(expr) == Rf_install("n")){
    return dplyr::hybrid::Count<SlicedTibble>(data).summarise();
  }

  return R_UnboundValue;
}

template <typename SlicedTibble, typename LazySubsets>
SEXP hybrid_mutate(SEXP expr, const SlicedTibble& data, const LazySubsets& subsets, SEXP env){

  if(TYPEOF(expr) == LANGSXP && CAR(expr) == Rf_install("n")){
    return dplyr::hybrid::Count<SlicedTibble>(data).window();
  }

  return R_UnboundValue;
}

}
}




#endif
