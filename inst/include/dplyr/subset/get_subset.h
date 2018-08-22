#ifndef dplyr_GroupedSubset_H
#define dplyr_GroupedSubset_H

#include <dplyr/visitors/subset/column_subset.h>
#include <dplyr/visitors/subset/DataFrameSubsetVisitors.h>
#include <dplyr/subset/Subset.h>
#include <tools/utils.h>

namespace dplyr {

template <typename Index>
class ColumnSubset : public Subset<Index> {
public:
  ColumnSubset(SEXP x_) : x(x_) {}
  virtual ~ColumnSubset() {}

  virtual SEXP get(const Index& indices) const {
    return column_subset(x, indices);
  };
  virtual SEXP get_variable() const {
    return x;
  };
  virtual bool is_summary() const {
    return false;
  };
private:
  SEXP x;
};

template <typename Index>
inline Subset<Index>* get_subset(SEXP x) {
  return new ColumnSubset<Index>(x);
}

}

#endif
