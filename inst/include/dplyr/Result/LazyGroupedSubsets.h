#ifndef dplyr_LazyGroupedSubsets_H
#define dplyr_LazyGroupedSubsets_H

#include <tools/SymbolMap.h>
#include <boost/shared_ptr.hpp>

#include <dplyr/GroupedDataFrame.h>
#include <dplyr/RowwiseDataFrame.h>
#include <dplyr/NaturalDataFrame.h>

#include <dplyr/SummarisedVariable.h>

#include <dplyr/Result/GroupedSubset.h>
#include <dplyr/Result/ILazySubsets.h>

namespace dplyr {

class LazySplitSubsets {
public:
  LazySplitSubsets(const DataFrame& data) :
    subsets(data.size()),
    summarised(data.size(), false),
    symbol_map(data.size(), data.names())
  {
    CharacterVector names = data.names();
    int n = data.size();
    LOG_INFO << "processing " << n << " vars: " << names;
    for (int i = 0; i < n; i++) {
      subsets[i] = data[i];
    }
  }

  inline const CharacterVector get_names() const {
    return symbol_map.get_names().get_vector() ;
  }

  inline SEXP get_variable(int i) const {
    return subsets[i];
  }

  inline SEXP get_variable(const SymbolString& symbol) const {
    return get_variable(symbol_map.get(symbol));
  }

  inline bool is_summary(int i) const {
    return summarised[i];
  }

  inline bool is_summary(const SymbolString& symbol) const {
    return is_summary(symbol_map.get(symbol));
  }

  inline bool has_variable(const SymbolString& name) const {
    return symbol_map.has(name);
  }

  inline int size() const {
    return subsets.size();
  }

  inline void input(const SymbolString& symbol, SEXP x, bool summary = false) {
    SymbolMapIndex index = symbol_map.insert(symbol);
    if (index.origin == NEW) {
      subsets.push_back(x);
      summarised.push_back(summary);
    } else {
      subsets[index.pos] = x ;
      summarised[index.pos] = summary;
    }
  }

private:

  std::vector<SEXP> subsets;
  std::vector<bool> summarised;
  SymbolMap symbol_map;

};

}
#endif
