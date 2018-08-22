#ifndef dplyr_LazySplitSubsets_H
#define dplyr_LazySplitSubsets_H

#include <tools/SymbolMap.h>

#include <dplyr/data/GroupedDataFrame.h>
#include <dplyr/data/RowwiseDataFrame.h>
#include <dplyr/data/NaturalDataFrame.h>

#include <dplyr/visitors/subset/column_subset.h>

namespace dplyr {

template <class Data>
class LazySplitSubsets {
  typedef typename Data::slicing_index slicing_index;

public:
  LazySplitSubsets(const Data& gdf_) :
    gdf(gdf_),
    columns(),
    symbol_map(),
    resolved()
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
    return columns[i].second;
  }

  SEXP get_variable(const SymbolString& symbol) const {
    return columns[symbol_map.get(symbol)].second;
  }

  SEXP get(const SymbolString& symbol, const slicing_index& indices) const {
    int idx = symbol_map.get(symbol);

    SEXP value = resolved[idx];
    if (value == R_NilValue) {
      const Pair& pair = columns[idx];

      resolved[idx] = value = pair.first ?
                              column_subset(pair.second, RowwiseSlicingIndex(indices.group())) :
                              column_subset(pair.second, indices)
                              ;
    }
    return value;
  }

  bool is_summary(int i) const {
    return columns[i].first;
  }

  bool is_summary(const SymbolString& symbol) const {
    return columns[symbol_map.get(symbol)].first;
  }

  bool has_variable(const SymbolString& head) const {
    return symbol_map.has(head);
  }

  void input_column(const SymbolString& symbol, SEXP x) {
    input_impl(symbol, false, x);
  }

  int size() const {
    return columns.size();
  }

  int nrows() const {
    return gdf.nrows();
  }

  void clear() {
    for (size_t i = 0; i < resolved.size(); i++) {
      resolved[i] = R_NilValue;
    }
  }

  void input_summarised(const SymbolString& symbol, SEXP x) {
    input_impl(symbol, true, x);
  }

private:

  const Data& gdf;

  typedef std::pair<bool, SEXP> Pair;
  std::vector<Pair> columns ;

  SymbolMap symbol_map;
  mutable std::vector<SEXP> resolved;

  void input_impl(const SymbolString& symbol, bool summarised, SEXP x) {
    SymbolMapIndex index = symbol_map.insert(symbol);
    Pair pair(summarised, x);
    if (index.origin == NEW) {
      columns.push_back(pair);
      resolved.push_back(R_NilValue);
    } else {
      int idx = index.pos;
      columns[idx] = pair;
      resolved[idx] = R_NilValue;
    }
  }
};

}
#endif
