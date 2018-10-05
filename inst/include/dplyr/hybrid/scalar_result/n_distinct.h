#ifndef dplyr_hybrid_n_distinct_h
#define dplyr_hybrid_n_distinct_h

#include <dplyr/hybrid/HybridVectorScalarResult.h>
#include <dplyr/hybrid/Expression.h>

#include <dplyr/visitor_set/VisitorEqualPredicate.h>
#include <dplyr/visitor_set/VisitorHash.h>
#include <dplyr/visitors/vector/MultipleVectorVisitors.h>

namespace dplyr {
namespace hybrid {

namespace internal {

template <typename SlicedTibble, bool NARM>
class N_Distinct : public HybridVectorScalarResult<INTSXP, SlicedTibble, N_Distinct<SlicedTibble, NARM> > {
public:
  typedef HybridVectorScalarResult<INTSXP, SlicedTibble, N_Distinct> Parent ;

  typedef VisitorHash<MultipleVectorVisitors> Hash;
  typedef VisitorEqualPredicate<MultipleVectorVisitors> Pred;
  typedef dplyr_hash_set<int, Hash, Pred > Set;

  N_Distinct(const SlicedTibble& data, List columns_):
    Parent(data),
    columns(columns_),
    nrows(data.nrows()),
    ngroups(data.ngroups())
  {}

  inline int process(const typename SlicedTibble::slicing_index& indices) const {
    MultipleVectorVisitors visitors(columns, nrows, ngroups, indices.group());
    int n = indices.size();
    Set set(n, Hash(visitors), Pred(visitors));

    for (int i = 0; i < n; i++) {
      int index = indices[i];
      if (!NARM || !visitors.is_na(index)) set.insert(index);
    }
    return set.size();
  }

private:
  List columns;
  int nrows;
  int ngroups;
};

}

template <typename SlicedTibble, typename Expression, typename Operation>
SEXP n_distinct_dispatch(const SlicedTibble& data, const Expression& expression, const Operation& op) {
  std::vector<SEXP> columns;
  bool narm = false;

  int n = expression.size();
  for (int i = 0; i < n; i++) {
    Column column;

    if (expression.is_named(i, symbols::narm)) {
      bool test ;
      // if we have na.rm= TRUE, or na.rm = FALSE, we can handle it
      if (expression.is_scalar_logical(i, test)) {
        narm = test;
      } else {
        // otherwise, we need R to evaluate it, so we give up
        return R_UnboundValue;
      }
    } else if (expression.is_column(i, column)) {
      columns.push_back(column.data);
    } else {
      // give up, R will handle the call
      return R_UnboundValue;
    }
  }

  // let R handle the call
  if (!columns.size()) {
    return R_UnboundValue;
  }

  if (narm) {
    return op(internal::N_Distinct<SlicedTibble, true>(data, wrap(columns)));
  } else {
    return op(internal::N_Distinct<SlicedTibble, false>(data, wrap(columns)));
  }
}

}
}

#endif
