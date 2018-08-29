#ifndef dplyr_LazySplitSubsets_H
#define dplyr_LazySplitSubsets_H

#include <tools/SymbolMap.h>

#include <dplyr/data/GroupedDataFrame.h>
#include <dplyr/data/RowwiseDataFrame.h>
#include <dplyr/data/NaturalDataFrame.h>

#include <dplyr/visitors/subset/column_subset.h>

namespace dplyr {

template <typename Index>
struct SubsetData {
public:
  SubsetData(bool summary_, SEXP symbol_, SEXP data_) :
    summary(summary_),
    symbol(symbol_),
    data(data_),
    resolved(R_UnboundValue)
  {}

  inline SEXP get(const Index& indices, SEXP env) {
    if (!is_resolved()) {
      materialize(indices, env);
    }
    return resolved;
  }

  inline void materialize(const Index& indices, SEXP env) {
    Shield<SEXP> value(summary ?
                       column_subset(data, RowwiseSlicingIndex(indices.group())) :
                       column_subset(data, indices)
                      );
    Rf_defineVar(symbol, value, env);
    resolved = value;
  }

  bool is_resolved() const {
    return resolved != R_UnboundValue;
  }

  bool is_summary() const {
    return summary;
  }

  inline SEXP get_data() const {
    return data;
  }

  inline void clear() {
    resolved = R_UnboundValue;
  }

  inline void update(const Index& indices, SEXP env) {
    if (is_resolved()) {
      materialize(indices, env);
    }
  }

private:

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

  const SubsetData<slicing_index>* maybe_get_subset_data(const SymbolString& symbol) const {
    int pos = symbol_map.find(symbol);
    if (pos >= 0) {
      return &subsets[pos];
    } else {
      return 0;
    }
  }

  const SubsetData<slicing_index>& get_subset_data(int i) const {
    return subsets[i];
  }

  SEXP get(const SymbolString& symbol, const slicing_index& indices, SEXP mask) {
    int idx = symbol_map.get(symbol);
    return subsets[idx].get(indices, mask);
  }

  void input_column(const SymbolString& symbol, SEXP x) {
    input_impl(symbol, false, x);
  }

  void input_summarised(const SymbolString& symbol, SEXP x) {
    input_impl(symbol, true, x);
  }

  int size() const {
    return subsets.size();
  }

  void clear() {
    for (size_t i = 0; i < subsets.size(); i++) {
      subsets[i].clear();
    }
  }

  void update(const slicing_index& indices, SEXP mask) {
    for (size_t i = 0; i < subsets.size(); i++) {
      subsets[i].update(indices, mask);
    }
  }

private:
  const Data& gdf;

  std::vector< SubsetData<slicing_index> > subsets ;
  SymbolMap symbol_map;

  void input_impl(const SymbolString& symbol, bool summarised, SEXP x) {
    SymbolMapIndex index = symbol_map.insert(symbol);
    SubsetData<slicing_index> subset(summarised, Rf_installChar(symbol.get_sexp()), x);
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
