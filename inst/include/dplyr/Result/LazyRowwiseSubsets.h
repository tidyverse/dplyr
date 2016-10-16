#ifndef dplyr_LazyRowwiseSubsets_H
#define dplyr_LazyRowwiseSubsets_H

#include <tools/hash.h>
#include <tools/delete_all.h>

#include <dplyr/RowwiseDataFrame.h>

#include <dplyr/Result/RowwiseSubset.h>
#include <dplyr/Result/ILazySubsets.h>

namespace dplyr {

  class LazyRowwiseSubsets : public ILazySubsets {
  public:
    typedef dplyr_hash_map<SEXP, RowwiseSubset*> RowwiseSubsetMap;
    typedef dplyr_hash_map<SEXP, SEXP> ResolvedSubsetMap;

    LazyRowwiseSubsets(const RowwiseDataFrame& rdf_):
      rdf(rdf_),
      subset_map(),
      resolved_map(),
      owner(true)
    {
      const DataFrame& data = rdf.data();
      CharacterVector names = data.names();
      int n = data.size();
      for (int i=0; i<n; i++) {
        subset_map[ Rf_installChar(names[i]) ] = rowwise_subset(data[i]);
      }
    }

    LazyRowwiseSubsets(const LazyRowwiseSubsets& other) :
      rdf(other.rdf),
      subset_map(other.subset_map),
      resolved_map(other.resolved_map),
      owner(false)
    {}

    virtual ~LazyRowwiseSubsets() {
      if (owner) delete_all_second(subset_map);
    }

  public:
    virtual SEXP get_variable(SEXP symbol) const {
      RowwiseSubsetMap::const_iterator it = subset_map.find(symbol);
      if (it == subset_map.end()) {
        stop("variable '%s' not found in the dataset", CHAR(PRINTNAME(symbol)));
      }
      return it->second->get_variable();
    }

    virtual bool is_summary(SEXP symbol) const {
      RowwiseSubsetMap::const_iterator it = subset_map.find(symbol);
      return it->second->is_summary();
    }

    virtual int count(SEXP head) const {
      return subset_map.count(head);
    }

    void input(SEXP symbol, SEXP x) {
      if (TYPEOF(symbol) == SYMSXP) {
        input_subset(symbol, rowwise_subset(x));
      } else {
        input_subset(Rf_installChar(symbol), rowwise_subset(x));
      }
    }

    virtual int size() const {
      return subset_map.size();
    }

    virtual int nrows() const {
      return rdf.nrows();
    }

  public:
    void clear() {
      resolved_map.clear();
    }

    SEXP get(SEXP symbol, const SlicingIndex& indices) {
      ResolvedSubsetMap::const_iterator it = resolved_map.find(symbol);
      if (it == resolved_map.end()) {
        SEXP res = subset_map[symbol]->get(indices);
        resolved_map[symbol] = res;
        return res;
      } else {
        return it->second;
      }
    }

  private:
    const RowwiseDataFrame& rdf;
    RowwiseSubsetMap subset_map;
    ResolvedSubsetMap resolved_map;
    bool owner;

    void input_subset(SEXP symbol, RowwiseSubset* sub) {
      RowwiseSubsetMap::iterator it = subset_map.find(symbol);
      if (it == subset_map.end()) {
        subset_map[symbol] = sub;
      } else {
        // found it, replacing the subset
        delete it->second;
        it->second = sub;
      }
    }
  };


}
#endif
