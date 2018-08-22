#ifndef dplyr_LazySplitSubsets_H
#define dplyr_LazySplitSubsets_H

#include <tools/SymbolMap.h>

#include <dplyr/data/GroupedDataFrame.h>
#include <dplyr/data/RowwiseDataFrame.h>
#include <dplyr/data/NaturalDataFrame.h>

#include <dplyr/visitors/subset/column_subset.h>

namespace dplyr {

struct SubsetData {
  SubsetData(bool summary_, SEXP symbol_, SEXP data_) :
    summary(summary_),
    symbol(symbol_),
    data(data_),
    resolved(R_UnboundValue)
  {}

  template <typename Index>
  inline void materialize(const Index& indices) {
    resolved = summary ?
               column_subset(data, RowwiseSlicingIndex(indices.group())) :
               column_subset(data, indices)
               ;
  }

  bool is_resolved() const {
    return resolved != R_UnboundValue;
  }

  bool summary;
  SEXP symbol;
  SEXP data;
  SEXP resolved;
};

template <class Data>
class LazySplitSubsets {
  typedef typename Data::slicing_index slicing_index;

public:
  LazySplitSubsets(const Data& gdf_) :
    gdf(gdf_),
    subsets(),
    symbol_map()
  {
    const DataFrame& data = gdf.data();
    CharacterVector names = data.names();
    int n = data.size();
    LOG_INFO << "processing " << n << " vars: " << names;
    for (int i = 0; i < n; i++) {
      input_column(names[i], data[i]);
    }
  }

public:
  const SymbolVector get_variable_names() const {
    return symbol_map.get_names();
  }

  SEXP get_variable(int i) const {
    return subsets[i].data;
  }

  SEXP get_variable(const SymbolString& symbol) const {
    return subsets[symbol_map.get(symbol)].data;
  }

  SEXP get(const SymbolString& symbol, const slicing_index& indices) const {
    int idx = symbol_map.get(symbol);

    SubsetData subset = subsets[idx];
    if (!subset.is_resolved()) {
      subset.materialize(indices);
    }
    return subset.resolved;
  }

  bool is_summary(int i) const {
    return subsets[i].summary;
  }

  bool is_summary(const SymbolString& symbol) const {
    return subsets[symbol_map.get(symbol)].summary;
  }

  bool has_variable(const SymbolString& head) const {
    return symbol_map.has(head);
  }

  void input_column(const SymbolString& symbol, SEXP x) {
    input_impl(symbol, false, x);
  }

  int size() const {
    return subsets.size();
  }

  int nrows() const {
    return gdf.nrows();
  }

  void clear() {
    for (size_t i = 0; i < subsets.size(); i++) {
      subsets[i].resolved = R_UnboundValue;
    }
  }

  void input_summarised(const SymbolString& symbol, SEXP x) {
    input_impl(symbol, true, x);
  }

private:

  const Data& gdf;

  std::vector<SubsetData> subsets ;
  SymbolMap symbol_map;

  void input_impl(const SymbolString& symbol, bool summarised, SEXP x) {
    SymbolMapIndex index = symbol_map.insert(symbol);
    SubsetData subset(summarised, symbol.get_sexp(), x);
    if (index.origin == NEW) {
      subsets.push_back(subset);
    } else {
      int idx = index.pos;
      subsets[idx] = subset;
    }
  }
};

}
#endif
