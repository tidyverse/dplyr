#ifndef dplyr_hybrid_count_h
#define dplyr_hybrid_count_h

#include <dplyr/hybrid/HybridVectorScalarResult.h>

namespace dplyr {
namespace hybrid {

template <typename Data>
class Count : public HybridVectorScalarResult<INTSXP, Data, Count<Data> > {
public:
  typedef HybridVectorScalarResult<INTSXP, Data, Count<Data> > Parent ;
  typedef typename Data::slicing_index Index;

  Count(const Data& data) : Parent(data) {}

  int process(const Index& indices) const {
    return indices.size();
  }
} ;

template <typename Data>
inline Count<Data> n_(const Data& data) {
  return Count<Data>(data);
}

}
}


#endif
