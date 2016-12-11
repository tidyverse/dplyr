#ifndef dplyr_LazyRowwiseSubsets_H
#define dplyr_LazyRowwiseSubsets_H

#include <tools/SymbolMap.h>

#include <dplyr/RowwiseDataFrame.h>
#include <dplyr/SummarisedVariable.h>

#include <dplyr/Result/RowwiseSubset.h>
#include <dplyr/Result/ILazySubsets.h>

namespace dplyr {

  class LazyRowwiseSubsets : public ILazySubsets {
  public:
    LazyRowwiseSubsets(const RowwiseDataFrame& rdf_):
      rdf(rdf_),
      subsets(),
      symbol_map(),
      resolved(),
      owner(true)
    {
      const DataFrame& data = rdf.data();
      CharacterVector names = data.names();
      int n = data.size();
      LOG_INFO << "processing " << n << " vars: " << names;
      for (int i=0; i<n; i++) {
        input(names[i], data[i]);
      }
    }

    LazyRowwiseSubsets(const LazyRowwiseSubsets& other) :
      rdf(other.rdf),
      subsets(other.subsets),
      symbol_map(other.symbol_map),
      resolved(other.resolved),
      owner(false)
    {}

    virtual ~LazyRowwiseSubsets() {
      if (owner) {
        for (size_t i=0; i<subsets.size(); i++) {
          delete subsets[i];
        }
      }
    }

  public:
    virtual CharacterVector get_variable_names() const {
      return symbol_map.get_names();
    }

    virtual SEXP get_variable(SEXP symbol) const {
      return subsets[symbol_map.get(symbol)]->get_variable();
    }

    virtual bool is_summary(SEXP symbol) const {
      return subsets[symbol_map.get(symbol)]->is_summary();
    }

    virtual int count(SEXP head) const {
      int res = symbol_map.has(head);
      return res;
    }

    void input(SEXP symbol, SEXP x) {
      input_subset(symbol, rowwise_subset(x));
    }

    virtual int size() const {
      return subsets.size();
    }

    virtual int nrows() const {
      return rdf.nrows();
    }

  public:
    void clear() {
      for (size_t i=0; i<resolved.size(); i++) {
        resolved[i] = R_NilValue;
      }
    }

    SEXP get(SEXP symbol, const SlicingIndex& indices) const {
      int idx = symbol_map.get(symbol);

      SEXP value = resolved[idx];
      if (value == R_NilValue) {
        resolved[idx] = value = subsets[idx]->get(indices);
      }
      return value;
    }

    void input_summarised(SEXP symbol, SummarisedVariable x) {
      input_subset(symbol, summarised_subset(x));
    }

  private:
    const RowwiseDataFrame& rdf;
    std::vector<RowwiseSubset*> subsets;
    SymbolMap symbol_map;
    mutable std::vector<SEXP> resolved;

    bool owner;

    void input_subset(const Symbol& symbol, RowwiseSubset* sub) {
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
