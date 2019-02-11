#ifndef dplyr_tools_GroupedDataFrame_H
#define dplyr_tools_GroupedDataFrame_H

#include <dplyr/registration.h>
#include <tools/SlicingIndex.h>
#include <tools/VectorView.h>

#include <tools/SymbolVector.h>
#include <tools/SymbolMap.h>
#include <tools/bad.h>
#include <dplyr/symbols.h>

namespace dplyr {

class GroupedDataFrame;

class GroupedDataFrameIndexIterator {
public:
  GroupedDataFrameIndexIterator(const GroupedDataFrame& gdf_);

  GroupedDataFrameIndexIterator& operator++();

  GroupedSlicingIndex operator*() const;

  int i;
  const GroupedDataFrame& gdf;
  ListView indices;
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

  inline SEXP indices() const {
    return groups[groups.size() - 1] ;
  }

  inline SymbolVector get_vars() const {
    return symbols.get_names();
  }

  inline const DataFrame& group_data() const {
    return groups;
  }

  template <typename Data>
  static void strip_groups(Data& x) {
    x.attr("groups") = R_NilValue;
  }

  template <typename Data>
  static void set_groups(Data& x, SEXP groups) {
    x.attr("groups") = groups;
  }

  template <typename Data1, typename Data2>
  static void copy_groups(Data1& x, const Data2& y) {
    x.attr("groups") = y.attr("groups");
  }

  static inline CharacterVector classes() {
    return Rcpp::CharacterVector::create("grouped_df", "tbl_df", "tbl", "data.frame");
  }

  bool drops() const {
    SEXP drop_attr = Rf_getAttrib(groups, symbols::dot_drop);
    return Rf_isNull(drop_attr) || (is<bool>(drop_attr) && LOGICAL(drop_attr)[0] != FALSE);
  }

private:

  SymbolVector group_vars() const ;

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
  return GroupedSlicingIndex(indices[i], i);
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
