#ifndef dplyr_DataMask_H
#define dplyr_DataMask_H

#include <tools/SymbolMap.h>

#include <dplyr/data/GroupedDataFrame.h>
#include <dplyr/data/RowwiseDataFrame.h>
#include <dplyr/data/NaturalDataFrame.h>

#include <dplyr/visitors/subset/column_subset.h>

namespace dplyr {

template <class SlicedTibble>
class DataMask;

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

  inline void install(SEXP mask_active, SEXP mask_resolved, int pos, DataMask<SlicedTibble>* data_mask) {
    static Function active_binding_fun(".active_binding_fun", Rcpp::Environment::namespace_env("dplyr"));

    R_MakeActiveBinding(
      symbol,
      active_binding_fun(
        pos,
        XPtr< DataMask<SlicedTibble> >(data_mask, false)
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

  inline void install(SEXP mask_active, SEXP mask_resolved, int /* pos */, DataMask<NaturalDataFrame>* /* data_mask */) {
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


class DataMaskBase {
public:
  virtual ~DataMaskBase() {}

  virtual SEXP materialize(int idx) {
    return R_UnboundValue;
  }
};

template <class SlicedTibble>
class DataMask : public DataMaskBase {
  typedef typename SlicedTibble::slicing_index slicing_index;

public:
  DataMask(const SlicedTibble& gdf) :
    column_bindings(),
    symbol_map(gdf.data().size(), gdf.data().names()),
    active_bindings_ready(false)
  {
    const DataFrame& data = gdf.data();
    CharacterVector names = data.names();
    int n = data.size();
    LOG_INFO << "processing " << n << " vars: " << names;

    // install the subsets without lookups in symbol_map
    for (int i = 0; i < n; i++) {
      SEXP symbol = Rf_installChar(SymbolString(names[i]).get_sexp());
      column_bindings.push_back(ColumnBinding<SlicedTibble>(false, symbol, data[i]));
    }
  }

  const ColumnBinding<SlicedTibble>* maybe_get_subset_binding(const SymbolString& symbol) const {
    int pos = symbol_map.find(symbol);
    if (pos >= 0) {
      return &column_bindings[pos];
    } else {
      return 0;
    }
  }

  const ColumnBinding<SlicedTibble>& get_subset_binding(int i) const {
    return column_bindings[i];
  }

  void input_column(const SymbolString& symbol, SEXP x) {
    input_impl(symbol, false, x);
  }

  void input_summarised(const SymbolString& symbol, SEXP x) {
    input_impl(symbol, true, x);
  }

  int size() const {
    return column_bindings.size();
  }

  // before treating new expression
  // in its environment: parent_env
  void reset(SEXP parent_env) {
    if (!active_bindings_ready) {
      // the active bindings have not been used at all
      // so setup the environments ...
      mask_active = child_env(R_EmptyEnv);
      mask_resolved = child_env(mask_active);

      // ... and install the bindings
      for (int i = 0; i < column_bindings.size(); i++) {
        column_bindings[i].install(mask_active, mask_resolved, i, this);
      }

      active_bindings_ready = true;
    } else {
      // forget about which indices are materialized
      materialized.clear();

      // update the materialized environment if needed
      update_mask_resolved();
    }

    // finally setup the data mask with
    // bottom    : the environment with the "resolved" bindings, this is initially empty but gets filled
    //             as soon as the active binding is resolved
    //
    // top       : the environment containing active bindings.
    //
    // overscope : where .data etc ... are installed
    overscope = internal::rlang_api().new_data_mask(mask_resolved, mask_active, parent_env);

    // install the pronoun
    overscope[".data"] = internal::rlang_api().as_data_pronoun(mask_active);

    // change the parent environment of mask_active
    SET_ENCLOS(mask_active, parent_env);
  }

  void update(const slicing_index& indices) {
    set_current_indices(indices);
    for (size_t i = 0; i < materialized.size(); i++) {
      column_bindings[materialized[i]].update_indices(indices, mask_resolved);
    }
  }

  // called from the active binding
  virtual SEXP materialize(int idx) {
    SEXP res = column_bindings[idx].get(get_current_indices(), mask_resolved);
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
  DataMask(const DataMask&);

  std::vector< ColumnBinding<SlicedTibble> > column_bindings ;
  std::vector<int> materialized ;
  SymbolMap symbol_map;

  Environment mask_active;
  Environment mask_resolved;
  Environment overscope;

  bool active_bindings_ready;
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
      if (active_bindings_ready) {
        subset.install(mask_active, mask_resolved, index.pos, this);
      }

      column_bindings.push_back(subset);
    } else {
      // otherwise, update it
      if (active_bindings_ready) {
        subset.update(mask_active, mask_resolved);
      }

      column_bindings[index.pos] = subset;

    }
  }

  void update_mask_resolved() {
    mask_resolved = child_env(mask_active);
  }

};

template <>
inline void DataMask<NaturalDataFrame>::update_mask_resolved() {}

}
#endif
