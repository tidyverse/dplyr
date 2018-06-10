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

template <class Data>
class LazySplitSubsets {
  typedef typename Data::subset subset;

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
      input(names[i], data[i]);
    }
  }

  const CharacterVector get_names() const {
    return symbol_map.get_names().get_vector() ;
  }

  SEXP get_variable(int i) const {
    return subsets[i]->get_variable();
  }

  SEXP get_variable(const SymbolString& symbol) const {
    return subsets[symbol_map.get(symbol)]->get_variable();
  }

  bool is_summary(int i) const {
    return subsets[i]->is_summary();
  }

  bool is_summary(const SymbolString& symbol) const {
    return subsets[symbol_map.get(symbol)]->is_summary();
  }

  bool has_variable(const SymbolString& head) const {
    return symbol_map.has(head);
  }

  void input(const SymbolString& symbol, SEXP x) {
    input_subset(symbol, gdf.create_subset(x));
  }

  int size() const {
    return subsets.size();
  }

  void input_summarised(const SymbolString& symbol, SummarisedVariable x) {
    input_subset(symbol, summarised_subset(x));
  }

private:

  const Data& gdf;
  std::vector< boost::shared_ptr<subset> > subsets;
  SymbolMap symbol_map;

  void input_subset(const SymbolString& symbol, subset* sub) {
    SymbolMapIndex index = symbol_map.insert(symbol);
    if (index.origin == NEW) {
      subsets.push_back(boost::shared_ptr<subset>(sub));
    } else {
      subsets[index.pos].reset(sub) ;
    }
  }
};

}
#endif
