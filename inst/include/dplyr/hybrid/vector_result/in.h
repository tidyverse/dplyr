#ifndef dplyr_hybrid_in_h
#define dplyr_hybrid_in_h

#include <dplyr/hybrid/HybridVectorVectorResult.h>
#include <dplyr/hybrid/Column.h>

#include <tools/hash.h>

namespace dplyr {
namespace hybrid {

namespace internal {

template <typename Data, int RTYPE>
class In_Column_Column : public HybridVectorVectorResult<LGLSXP, Data, In_Column_Column<Data, RTYPE> > {
public:
  typedef HybridVectorVectorResult<LGLSXP, Data, In_Column_Column> Parent;
  typedef typename Data::slicing_index Index;
  typedef Rcpp::Vector<RTYPE> Vector;
  typedef typename Vector::stored_type stored_type;

  In_Column_Column(const Data& data, SEXP x, SEXP y) :
    Parent(data),
    lhs(x),
    rhs(y)
  {}

  void fill(const Index& indices, Rcpp::LogicalVector& out) const {
    int n = indices.size();

    dplyr_hash_set<stored_type> set(n);
    for (int i = 0; i < indices.size(); i++) {
      set.insert((stored_type)rhs[indices[i]]);
    }

    for (int i = 0; i < n; i++) {
      stored_type value = lhs[indices[i]];
      if (Vector::is_na(value)) {
        out[ indices[i] ] = false;
      } else {
        out[ indices[i] ] = set.count(value);
      }
    }
  }

private:
  Vector lhs;
  Vector rhs;
};

}

template <typename Data, typename Operation>
inline SEXP in_column_column(const Data& data, Column col_x, Column col_y, const Operation& op) {
  if (TYPEOF(col_x.data) != TYPEOF(col_y.data)) return R_UnboundValue;
  SEXP x = col_x.data, y = col_y.data;

  switch (TYPEOF(x)) {
  case LGLSXP:
    return op(internal::In_Column_Column<Data, LGLSXP>(data, x, y));
  case RAWSXP:
    return op(internal::In_Column_Column<Data, RAWSXP>(data, x, y));
  case INTSXP:
    return op(internal::In_Column_Column<Data, INTSXP>(data, x, y));
  case REALSXP:
    return op(internal::In_Column_Column<Data, REALSXP>(data, x, y));
  case STRSXP:
    return op(internal::In_Column_Column<Data, STRSXP>(data, x, y));
  case CPLXSXP:
    return op(internal::In_Column_Column<Data, CPLXSXP>(data, x, y));
  case VECSXP:
    return op(internal::In_Column_Column<Data, VECSXP>(data, x, y));
  default:
    break;
  }
  return R_UnboundValue;

}


}
}

#endif
