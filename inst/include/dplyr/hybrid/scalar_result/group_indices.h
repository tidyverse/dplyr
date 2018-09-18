#ifndef dplyr_hybrid_group_indices_h
#define dplyr_hybrid_group_indices_h

#include <dplyr/hybrid/HybridVectorScalarResult.h>

namespace dplyr {
namespace hybrid {

namespace internal {

template <typename SlicedTibble>
class GroupIndices : public HybridVectorScalarResult<INTSXP, SlicedTibble, GroupIndices<SlicedTibble> > {
public:
  typedef HybridVectorScalarResult<INTSXP, SlicedTibble, GroupIndices> Parent ;

  GroupIndices(const SlicedTibble& data) : Parent(data) {}

  inline int process(const typename SlicedTibble::slicing_index& indices) const {
    return indices.group() + 1;
  }
};
}

// group_indices()
template <typename SlicedTibble>
internal::GroupIndices<SlicedTibble> group_indices_(const SlicedTibble& data) {
  return internal::GroupIndices<SlicedTibble>(data);
}

}
}

#endif
