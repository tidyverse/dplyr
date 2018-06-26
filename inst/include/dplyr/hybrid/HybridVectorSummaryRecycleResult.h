#ifndef DPLYR_HYBRID_HybridVectorSummaryRecycleResult_H
#define DPLYR_HYBRID_HybridVectorSummaryRecycleResult_H

#include <dplyr/hybrid/HybridVectorVectorResult.h>

namespace dplyr {
namespace hybrid {

template <int RTYPE, typename Data, typename Impl>
class HybridVectorSummaryRecycleResult :
  public HybridVectorVectorResult<RTYPE, Data, HybridVectorSummaryRecycleResult<RTYPE, Data, Impl> >
{
public:
  typedef HybridVectorVectorResult<RTYPE, Data, HybridVectorSummaryRecycleResult> Parent;
  typedef typename Data::slicing_index Index;
  typedef Rcpp::Vector<RTYPE> Vector;

  HybridVectorSummaryRecycleResult(const Data& data) : Parent(data) {}

  void fill(const Index& indices, Vector& out) const {
    int n = indices.size();
    typename Vector::stored_type value = self()->value(indices);
    for (int i = 0; i < n; i++) out[indices[i]] = value;
  }

private:

  inline const Impl* self() const {
    return static_cast<const Impl*>(this);
  }

};

}
}



#endif


