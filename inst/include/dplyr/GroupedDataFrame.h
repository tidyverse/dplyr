#ifndef dplyr_tools_GroupedDataFrame_H
#define dplyr_tools_GroupedDataFrame_H

#include <dplyr/registration.h>
#include <tools/SlicingIndex.h>

#include <dplyr/Result/GroupedSubset.h>

#include <tools/SymbolVector.h>
#include <tools/SymbolMap.h>

#include <dplyr/bad.h>

namespace dplyr {

class GroupedDataFrame;

class GroupedDataFrameIndexIterator {
public:
  GroupedDataFrameIndexIterator(const GroupedDataFrame& gdf_);

  GroupedDataFrameIndexIterator& operator++();

  GroupedSlicingIndex operator*() const;

  int i;
  const GroupedDataFrame& gdf;
  List indices;
};

class GroupedDataFrame {
public:
  typedef GroupedDataFrameIndexIterator group_iterator;
  typedef GroupedSlicingIndex slicing_index;
  typedef GroupedSubset subset;

  GroupedDataFrame(DataFrame x);
  GroupedDataFrame(DataFrame x, const SymbolVector& symbols_);

  group_iterator group_begin() const {
    return GroupedDataFrameIndexIterator(*this);
  }

  SymbolString symbol(int i) const {
    return symbols.get_name(i);
  }

  DataFrame& data() {
    return data_;
  }
  const DataFrame& data() const {
    return data_;
  }

  inline int ngroups() const {
    return labels.nrow();
  }

  inline int nvars() const {
    return labels.size() - 1;
  }

  inline int nrows() const {
    return data_.nrows();
  }

  inline SEXP label(int i) const {
    return labels[i];
  }

  inline int max_group_size() const {
    return max_group_size_;
  }

  inline bool has_group(const SymbolString& g) const {
    return symbols.has(g);
  }

  inline subset* create_subset(SEXP x) const {
    return grouped_subset(x, max_group_size());
  }

  inline List indices() const {
    return labels[labels.size() - 1] ;
  }

private:
  void set_max_group_size() {
    List indices = labels[labels.size() - 1];

    int n = indices.size();
    max_group_size_ = 0;
    for (int i = 0; i < n; i++) {
      max_group_size_ = std::max(max_group_size_, Rf_length(indices[i])) ;
    }
  }

  DataFrame data_;
  SymbolMap symbols;
  DataFrame labels;
  int max_group_size_ ;

};

inline GroupedDataFrameIndexIterator::GroupedDataFrameIndexIterator(const GroupedDataFrame& gdf_) :
  i(0), gdf(gdf_), indices(gdf.indices()) {}

inline GroupedDataFrameIndexIterator& GroupedDataFrameIndexIterator::operator++() {
  i++;
  return *this;
}

inline GroupedSlicingIndex GroupedDataFrameIndexIterator::operator*() const {
  return GroupedSlicingIndex(IntegerVector(indices[i]), i);
}

}

namespace Rcpp {
using namespace dplyr;

template <>
inline bool is<GroupedDataFrame>(SEXP x) {
  return Rf_inherits(x, "grouped_df");
}

}

#endif
