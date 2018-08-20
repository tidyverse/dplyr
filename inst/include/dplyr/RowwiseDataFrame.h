#ifndef dplyr_tools_RowwiseDataFrame_H
#define dplyr_tools_RowwiseDataFrame_H

#include <tools/SlicingIndex.h>

#include <dplyr/subset/RowwiseSubset.h>
#include <tools/SymbolString.h>

namespace dplyr {

class RowwiseDataFrame;

class RowwiseDataFrameIndexIterator {
public:
  RowwiseDataFrameIndexIterator() : i(0) {}

  RowwiseDataFrameIndexIterator& operator++() {
    ++i;
    return *this;
  }

  RowwiseSlicingIndex operator*() const {
    return RowwiseSlicingIndex(i);
  }

  int i;
};

class RowwiseDataFrame {
public:
  typedef RowwiseDataFrameIndexIterator group_iterator;
  typedef RowwiseSlicingIndex slicing_index;
  typedef Subset<slicing_index> subset;

  RowwiseDataFrame(SEXP x):
    data_(x)
  {}
  RowwiseDataFrame(SEXP x, const RowwiseDataFrame& /* model */):
    data_(x)
  {}

  group_iterator group_begin() const {
    return RowwiseDataFrameIndexIterator();
  }

  DataFrame& data() {
    return data_;
  }
  const DataFrame& data() const {
    return data_;
  }

  inline int nvars() const {
    return 0;
  }

  inline SymbolString symbol(int) {
    stop("Rowwise data frames don't have grouping variables");
  }

  inline SEXP label(int) {
    return R_NilValue;
  }

  inline int nrows() const {
    return data_.nrows();
  }

  inline int ngroups() const {
    return nrows();
  }

  inline subset* create_subset(SEXP x) const {
    return rowwise_subset(x);
  }

  inline SymbolVector get_vars() const {
    return SymbolVector();
  }

private:

  DataFrame data_;

};

}

namespace Rcpp {
using namespace dplyr;

template <>
inline bool is<RowwiseDataFrame>(SEXP x) {
  return Rf_inherits(x, "rowwise_df");
}

}

#endif
