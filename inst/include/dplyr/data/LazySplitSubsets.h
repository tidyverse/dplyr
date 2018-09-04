#ifndef dplyr_LazySplitSubsets_H
#define dplyr_LazySplitSubsets_H

#include <tools/SymbolMap.h>

#include <dplyr/data/GroupedDataFrame.h>
#include <dplyr/data/RowwiseDataFrame.h>
#include <dplyr/data/NaturalDataFrame.h>

#include <dplyr/visitors/subset/column_subset.h>

namespace dplyr {

template <class SlicedTibble>
class LazySplitSubsets;

template <typename SlicedTibble>
struct ColumnBinding {
public:
  typedef typename SlicedTibble::slicing_index slicing_index;

  ColumnBinding(bool summary_, SEXP symbol_, SEXP data_) :
    summary(summary_),
    symbol(symbol_),
    data(data_)
  {}

  // called by the active binding
  inline SEXP get(const slicing_index& indices, SEXP mask_resolved) {
    return materialize(indices, mask_resolved);
  }

  bool is_summary() const {
    return summary;
  }

  inline SEXP get_data() const {
    return data;
  }

  inline void update_indices(const slicing_index& indices, SEXP env) {
    materialize(indices, env);
  }

  inline void install(SEXP mask_active, SEXP mask_resolved, int pos, LazySplitSubsets<SlicedTibble>* subsets) {
    static Function active_binding_fun(".active_binding_fun", Rcpp::Environment::namespace_env("dplyr"));

    R_MakeActiveBinding(
      symbol,
      active_binding_fun(
        pos,
        XPtr< LazySplitSubsets<SlicedTibble> >(subsets, false)
      ),
      mask_active
    );
  }
  inline void update(SEXP mask_active, SEXP mask_resolved) {}

private:

  inline SEXP materialize(const slicing_index& indices, SEXP mask_resolved) {
    Shield<SEXP> value(summary ?
                       column_subset(data, RowwiseSlicingIndex(indices.group())) :
                       column_subset(data, indices)
                      );
    Rf_defineVar(symbol, value, mask_resolved);
    return value;
  }

  bool summary;
  SEXP symbol;
  SEXP data;

};

template <>
struct ColumnBinding<NaturalDataFrame> {
public:
  typedef NaturalDataFrame::slicing_index slicing_index;

  ColumnBinding(bool summary_, SEXP symbol_, SEXP data_) :
    summary(summary_),
    symbol(symbol_),
    data(data_)
  {}

  // (not really) called by the active binding
  inline SEXP get(const slicing_index& indices, SEXP mask_resolved) {
    return data;
  }

  bool is_summary() const {
    return summary;
  }

  inline SEXP get_data() const {
    return data;
  }

  inline void update_indices(const slicing_index& /* indices */, SEXP /* env */) {}

  inline void install(SEXP mask_active, SEXP mask_resolved, int pos, LazySplitSubsets<NaturalDataFrame>* subsets) {
    Rf_defineVar(symbol, data, mask_active);
  }
  inline void update(SEXP mask_active, SEXP mask_resolved) {
    Rf_defineVar(symbol, data, mask_active);
  }

private:

  bool summary;
  SEXP symbol;
  SEXP data;
};


class LazySplitSubsetsBase {
public:
  virtual ~LazySplitSubsetsBase() {}

  virtual SEXP materialize(int idx) {
    return R_UnboundValue;
  }
};

template <class SlicedTibble>
class LazySplitSubsets : public LazySplitSubsetsBase {
  typedef typename SlicedTibble::slicing_index slicing_index;

public:
  LazySplitSubsets(const SlicedTibble& gdf_) :
    gdf(gdf_),
    subsets(),
    symbol_map(),
    mask_active(child_env(R_EmptyEnv)),
    mask_resolved(child_env(mask_active))
  {
    const DataFrame& data = gdf.data();
    CharacterVector names = data.names();
    int n = data.size();
    LOG_INFO << "processing " << n << " vars: " << names;
    for (int i = 0; i < n; i++) {
      input_column(names[i], data[i]);
    }
  }

  const SymbolVector get_variable_names() const {
    return symbol_map.get_names();
  }

  const ColumnBinding<SlicedTibble>* maybe_get_subset_binding(const SymbolString& symbol) const {
    int pos = symbol_map.find(symbol);
    if (pos >= 0) {
      return &subsets[pos];
    } else {
      return 0;
    }
  }

  const ColumnBinding<SlicedTibble>& get_subset_binding(int i) const {
    return subsets[i];
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

  void reset(SEXP parent_env) {
    materialized.clear();
    update_mask_resolved();

    overscope = internal::rlang_api().new_data_mask(mask_resolved, mask_active, parent_env);
    overscope[".data"] = internal::rlang_api().as_data_pronoun(mask_active);
    SET_ENCLOS(mask_active, parent_env);
  }

  void update(const slicing_index& indices) {
    set_current_indices(indices);
    for (size_t i = 0; i < materialized.size(); i++) {
      subsets[materialized[i]].update_indices(indices, mask_resolved);
    }
  }

  // called from the active binding
  virtual SEXP materialize(int idx) {
    SEXP res = subsets[idx].get(get_current_indices(), mask_resolved);
    materialized.push_back(idx);
    return res;
  }

  SEXP eval(SEXP expr, const slicing_index& indices) {
    // update the bindings and the data context variables
    update(indices);

    // these are used by n(), ...
    overscope["..group_size"] = indices.size();
    overscope["..group_number"] = indices.group() + 1;

    // evaluate the call in the overscope
    SEXP res = Rcpp_eval(expr, overscope);

    return res;
  }

private:
  // forbid copy construction of this class
  LazySplitSubsets(const LazySplitSubsets&);

  const SlicedTibble& gdf;

  std::vector< ColumnBinding<SlicedTibble> > subsets ;
  std::vector<int> materialized ;
  SymbolMap symbol_map;

  Environment mask_active;
  Environment mask_resolved;
  Environment overscope;

  const slicing_index* current_indices;

  void set_current_indices(const slicing_index& indices) {
    current_indices = &indices;
  }

  const slicing_index& get_current_indices() {
    const slicing_index& indices = *current_indices;
    return indices;
  }

  void input_impl(const SymbolString& symbol, bool summarised, SEXP x) {
    // lookup in the symbol map for the position
    SymbolMapIndex index = symbol_map.insert(symbol);

    SEXP sym = Rf_installChar(symbol.get_sexp());
    ColumnBinding<SlicedTibble> subset(summarised, sym, x);

    if (index.origin == NEW) {
      // when this is a new variable, install the active binding
      subset.install(mask_active, mask_resolved, index.pos, this);

      subsets.push_back(subset);
    } else {
      // otherwise, update it
      subset.update(mask_active, mask_resolved);

      int idx = index.pos;
      subsets[idx] = subset;

    }
  }

  void update_mask_resolved() {
    mask_resolved = child_env(mask_active);
  }

};

template <>
inline void LazySplitSubsets<NaturalDataFrame>::update_mask_resolved() {}

}
#endif
