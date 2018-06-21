#ifndef dplyr_hybrid_group_indices_h
#define dplyr_hybrid_group_indices_h

#include <dplyr/hybrid/HybridVectorScalarResult.h>

namespace dplyr {
namespace hybrid {

namespace internal{

template <typename Data>
class GroupIndices : public HybridVectorScalarResult<INTSXP, Data, GroupIndices<Data> > {
public:
  typedef HybridVectorScalarResult<INTSXP, Data, GroupIndices> Parent ;
  typedef typename Data::slicing_index Index;

  GroupIndices(const Data& data) : Parent(data){}

  inline int process(const Index& indices) const {
    return indices.group() + 1;
  }
};
}

// group_indices()
template <typename Data>
internal::GroupIndices<Data> group_indices_(const Data& data) {
  return internal::GroupIndices<Data>(data);
}

}
}

#endif
