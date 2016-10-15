#ifndef dplyr_LazyGroupedSubsets_H
#define dplyr_LazyGroupedSubsets_H

#include <tools/SymbolMap.h>

#include <dplyr/GroupedDataFrame.h>

#include <dplyr/DataFrameSubsetVisitors.h>

#include <dplyr/Result/GroupedSubset.h>
#include <dplyr/Result/ILazySubsets.h>

namespace dplyr {

  class LazyGroupedSubsets : public ILazySubsets {
  public:
    LazyGroupedSubsets(const GroupedDataFrame& gdf_) :
      gdf(gdf_),
      symbol_map(),
      subsets(),
      resolved(),
      owner(true)
    {
      int max_size = gdf.max_group_size();
      const DataFrame& data = gdf.data();
      CharacterVector names = data.names();
      int n = data.size();
      LOG_INFO << "processing " << n << " vars: " << names;
      for (int i=0; i<n; i++) {
        input_subset(names[i], grouped_subset(data[i], max_size));
      }
    }

    LazyGroupedSubsets(const LazyGroupedSubsets& other) :
      gdf(other.gdf),
      symbol_map(other.symbol_map),
      subsets(other.subsets),
      resolved(other.resolved),
      owner(false)
    {}

    void clear() {
      for (size_t i=0; i<resolved.size(); i++) {
        resolved[i] = R_NilValue;
      }
    }

    int count(SEXP head) const {
      int res = symbol_map.has(head);
      return res;
    }

    virtual int size() const {
      return subsets.size();
    }

    virtual int nrows() const {
      return gdf.nrows();
    }

    SEXP get_variable(SEXP symbol) const {
      return subsets[symbol_map.get(symbol)]->get_variable();
    }
    bool is_summary(SEXP symbol) const {
      return subsets[symbol_map.get(symbol)]->is_summary();
    }
    SEXP get(SEXP symbol, const SlicingIndex& indices) {
      int idx = symbol_map.get(symbol);

      SEXP value = resolved[idx];
      if (value == R_NilValue) {
        resolved[idx] = value = subsets[idx]->get(indices);
      }
      return value;
    }

    virtual ~LazyGroupedSubsets() {
      if (owner) {
        for (size_t i=0; i<subsets.size(); i++) {
          delete subsets[i];
        }
      }
    }

    void input(SEXP symbol, SEXP x) {
      input_subset(symbol, grouped_subset(x, gdf.max_group_size()));
    }

    void input(SEXP symbol, SummarisedVariable x) {
      input_subset(symbol, summarised_grouped_subset(x, gdf.max_group_size()));
    }

  private:
    const GroupedDataFrame& gdf;
    SymbolMap symbol_map;
    std::vector<GroupedSubset*> subsets;
    std::vector<SEXP> resolved;

    bool owner;

    void input_subset(SEXP symbol, GroupedSubset* sub) {
      SymbolMapIndex index = symbol_map.insert(symbol);
      if (index.origin == NEW) {
        subsets.push_back(sub);
        resolved.push_back(R_NilValue);
      } else {
        int idx = index.pos;
        delete subsets[idx];
        subsets[idx] = sub;
        resolved[idx] = R_NilValue;
      }
    }
  };

}
#endif
