#ifndef dplyr_hybrid_count_h
#define dplyr_hybrid_count_h

#include <dplyr/hybrid/HybridVectorScalarResult.h>

namespace dplyr {
namespace hybrid {

template <typename SlicedTibble>
class Count : public HybridVectorScalarResult<INTSXP, SlicedTibble, Count<SlicedTibble> > {
public:
  typedef HybridVectorScalarResult<INTSXP, SlicedTibble, Count<SlicedTibble> > Parent ;

  Count(const SlicedTibble& data) : Parent(data) {}

  int process(const typename SlicedTibble::slicing_index& indices) const {
    return indices.size();
  }
} ;

template <typename SlicedTibble>
inline Count<SlicedTibble> n_(const SlicedTibble& data) {
  return Count<SlicedTibble>(data);
}

}
}


#endif
