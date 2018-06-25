#ifndef dplyr_hybrid_ntile_h
#define dplyr_hybrid_ntile_h

#include <dplyr/hybrid/HybridVectorVectorResult.h>
#include <dplyr/hybrid/Column.h>
#include <dplyr/hybrid/Expression.h>

#include <dplyr/visitors/SliceVisitor.h>
#include <dplyr/OrderVisitorImpl.h>

namespace dplyr {
namespace hybrid {

namespace internal{

template <typename Data>
class Ntile1 : public HybridVectorVectorResult<INTSXP, Data, Ntile1<Data> >{
public:
  typedef HybridVectorVectorResult<INTSXP, Data, Ntile1> Parent;
  typedef typename Data::slicing_index Index;

  Ntile1(const Data& data, int ntiles_): Parent(data), ntiles(ntiles_){}

  void fill(const Index& indices, Rcpp::IntegerVector& out) const {
    int m = indices.size();
    for (int j = m - 1; j >= 0; j--) {
      out[ indices[j] ] = (int)floor((ntiles * j) / m) + 1;
    }
  }

private:
  int ntiles;
};


}

template <typename Data>
inline internal::Ntile1<Data> ntile_1(const Data& data, int ntiles){
  return internal::Ntile1<Data>(data, ntiles);
}

// template <typename Data, typename Operation>
// inline SEXP row_number_1(const Data& data, Column column, const Operation& op){
//   SEXP x = column.data;
//   switch(TYPEOF(x)){
//   case INTSXP: return op(internal::RowNumber1<Data, INTSXP>(data, x));
//   case REALSXP: return op(internal::RowNumber1<Data, REALSXP>(data, x));
//   default: break;
//   }
//   return R_UnboundValue;
// }

}
}

#endif
