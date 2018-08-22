#ifndef dplyr_tools_GroupedDataFrame_H
#define dplyr_tools_GroupedDataFrame_H

#include <dplyr/registration.h>
#include <tools/SlicingIndex.h>

#include <tools/SymbolVector.h>
#include <tools/SymbolMap.h>
#include <tools/bad.h>

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

  GroupedDataFrame(DataFrame x);
  GroupedDataFrame(DataFrame x, const GroupedDataFrame& model);

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

  inline int size() const {
    return data_.size();
  }

  inline int ngroups() const {
    return groups.nrow();
  }

  inline int nvars() const {
    return nvars_ ;
  }

  inline int nrows() const {
    return data_.nrows();
  }

  inline SEXP label(int i) const {
    return groups[i];
  }

  inline bool has_group(const SymbolString& g) const {
    return symbols.has(g);
  }

  inline List indices() const {
    return groups[groups.size() - 1] ;
  }

  inline SymbolVector get_vars() const {
    return symbols.get_names();
  }

  static SymbolVector group_vars(SEXP x);

  inline const DataFrame& group_data() const {
    return groups;
  }

  template <typename Data>
  static void strip_groups(Data& x) {
    x.attr("groups") = R_NilValue;
  }

  template <typename Data>
  static void set_groups(Data& x, DataFrame groups) {
    x.attr("groups") = groups;
  }

  template <typename Data1, typename Data2>
  static void copy_groups(Data1& x, const Data2& y) {
    x.attr("groups") = y.attr("groups");
  }

private:

  DataFrame data_;
  SymbolMap symbols;
  DataFrame groups;
  int nvars_;

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
