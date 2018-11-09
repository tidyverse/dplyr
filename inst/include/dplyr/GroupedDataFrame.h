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

  GroupedDataFrame(SEXP x):
    data_(x),
    group_sizes(),
    biggest_group_size(0),
    symbols(get_vars(data_)),
    labels()
  {
    // handle lazyness
    bool is_lazy = Rf_isNull(data_.attr("group_sizes")) || Rf_isNull(data_.attr("labels"));

    if (is_lazy) {
      build_index_cpp_by_ref(data_);
    }
    group_sizes = data_.attr("group_sizes");
    biggest_group_size  = data_.attr("biggest_group_size");

    // attr() and operator= both might allocate (according to rchk),
    // need to protect
    RObject labels_attr(data_.attr("labels"));
    labels = labels_attr;

    if (!is_lazy) {
      // check consistency of the groups
      int rows_in_groups = sum(group_sizes);
      if (data_.nrows() != rows_in_groups) {
        bad_arg(".data", "is a corrupt grouped_df, contains {rows} rows, and {group_rows} rows in groups",
                _["rows"] = data_.nrows(), _["group_rows"] = rows_in_groups);
      }
    }
  }

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
    return group_sizes.size();
  }

  inline int nvars() const {
    return labels.size();
  }

  inline int nrows() const {
    return data_.nrows();
  }

  inline SEXP label(int i) const {
    return labels[i];
  }

  inline int max_group_size() const {
    return biggest_group_size;
  }

  inline bool has_group(const SymbolString& g) const {
    return symbols.has(g);
  }

  inline subset* create_subset(SEXP x) const {
    return grouped_subset(x, max_group_size());
  }

private:

  DataFrame data_;
  IntegerVector group_sizes;
  int biggest_group_size;
  SymbolMap symbols;
  DataFrame labels;

};

inline GroupedDataFrameIndexIterator::GroupedDataFrameIndexIterator(const GroupedDataFrame& gdf_) :
  i(0), gdf(gdf_), indices(gdf.data().attr("indices")) {}

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
  return Rf_inherits(x, "grouped_df") && Rf_getAttrib(x, Rf_install("vars")) != R_NilValue;
}

}

#endif
